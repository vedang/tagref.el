;;; tagref.el --- Tagref cross-reference support -*- lexical-binding: t -*-

;; Copyright (C) 2026 Vedang Manerikar

;; Author: Vedang Manerikar
;; Maintainer: Vedang Manerikar
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience
;; Homepage: https://github.com/vedang/tagref.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Emacs integration for tagref, a tool for
;; managing cross-references in code.  It provides:
;;
;; - Completion for [ref:] and [tag:] directives
;; - Xref integration for navigating to tag definitions (M-.)
;; - A check command that displays errors in a compilation buffer
;; - A tag listing command for browsing all tags
;; - Font-lock highlighting for tag and ref directives
;;
;; The mode is project-aware: enabling `tagref-mode' activates tagref
;; features for all buffers in the current project.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'xref)
(require 'compile)
(require 'tabulated-list)
(require 'ansi-color)

;;;; Customization

(defgroup tagref nil
  "Tagref cross-reference support."
  :group 'tools
  :prefix "tagref-")

(defcustom tagref-executable "tagref"
  "Path to the tagref executable."
  :type 'string
  :group 'tagref)

(defcustom tagref-arguments nil
  "Additional arguments passed to tagref commands."
  :type '(repeat string)
  :group 'tagref)

;;;; Faces

(defface tagref-tag-face
  '((t :inherit font-lock-function-name-face :underline t))
  "Face for tagref tag directives."
  :group 'tagref)

(defface tagref-ref-face
  '((t :inherit font-lock-constant-face :underline t))
  "Face for tagref ref directives."
  :group 'tagref)

;;;; Project tracking

;; Forward declaration for byte-compiler
(defvar tagref-mode)

(defvar tagref--enabled-projects (make-hash-table :test 'equal)
  "Hash table of project roots where `tagref-mode' is enabled.")

(defun tagref--project-root ()
  "Return the project root directory for the current buffer."
  (when-let ((proj (project-current)))
    (project-root proj)))

(defun tagref--in-enabled-project-p ()
  "Return non-nil if current buffer is in a tagref-enabled project."
  (when-let ((root (tagref--project-root)))
    (gethash root tagref--enabled-projects)))

(defun tagref--in-git-repo-p ()
  "Return non-nil if `default-directory' is in a git repository."
  (locate-dominating-file default-directory ".git"))

(defvar tagref--ripgrep-available 'unknown
  "Cache for ripgrep availability check.  One of `unknown', t, or nil.")

(defun tagref--ripgrep-available-p ()
  "Return non-nil if ripgrep (rg) is available."
  (when (eq tagref--ripgrep-available 'unknown)
    (setq tagref--ripgrep-available (executable-find "rg")))
  tagref--ripgrep-available)

;;;; Utilities

(defun tagref--call-process (&rest args)
  "Call tagref with ARGS and return output as string.
Returns nil if tagref fails or if not in a project."
  (when-let ((root (tagref--project-root)))
    (let ((default-directory root))
      (with-temp-buffer
        (let ((exit-code (apply #'call-process
                                tagref-executable
                                nil t nil
                                (append tagref-arguments args))))
          (when (zerop exit-code)
            (buffer-string)))))))

(defun tagref--parse-tag-line (line)
  "Parse a tagref `list-tags' output LINE.
Returns (NAME . (FILE . LINE-NUMBER)) or nil."
  ;; Format: [tag:name] @ file:line
  (when (string-match "\\[tag:\\([^]]+\\)\\] @ \\(.+\\):\\([0-9]+\\)$" line)
    (let ((name (match-string 1 line))
          (file (match-string 2 line))
          (line-num (string-to-number (match-string 3 line))))
      (cons name (cons file line-num)))))

(defun tagref--get-tags ()
  "Get all tags from the project.
Returns an alist of (NAME . (FILE . LINE-NUMBER))."
  (when-let ((output (tagref--call-process "list-tags")))
    (delq nil (mapcar #'tagref--parse-tag-line
                      (split-string output "\n" t)))))

(defun tagref--find-column-of-tag (file line tag-name)
  "Find the column of [tag:TAG-NAME] in FILE at LINE.
Returns the 0-based column, or 0 if not found."
  (let ((root (tagref--project-root)))
    (when root
      (let ((full-path (expand-file-name file root)))
        (when (file-exists-p full-path)
          (with-temp-buffer
            (insert-file-contents full-path)
            (goto-char (point-min))
            (forward-line (1- line))
            (if (re-search-forward (format "\\[tag:%s\\]"
                                           (regexp-quote tag-name))
                                   (line-end-position) t)
                (- (match-beginning 0) (line-beginning-position))
              0)))))))

;;;; Font-lock

(defconst tagref-font-lock-keywords
  `((,(rx "[tag:" (group (+ (not (any "]")))) "]")
     (0 'tagref-tag-face t))
    (,(rx "[ref:" (group (+ (not (any "]")))) "]")
     (0 'tagref-ref-face t)))
  "Font-lock keywords for tagref directives.")

(defun tagref--enable-font-lock ()
  "Enable font-lock for tagref directives in current buffer."
  (font-lock-add-keywords nil tagref-font-lock-keywords)
  (when font-lock-mode
    (font-lock-flush)))

(defun tagref--disable-font-lock ()
  "Disable font-lock for tagref directives in current buffer."
  (font-lock-remove-keywords nil tagref-font-lock-keywords)
  (when font-lock-mode
    (font-lock-flush)))

;;;; Completion

(defconst tagref--directive-start-regexp "\\[\\(tag\\|ref\\):"
  "Regexp matching the start of a tagref directive.")

(defun tagref--directive-at-point ()
  "Return directive info if point is inside a tagref directive.
Returns (TYPE BEG END PREFIX) where:
- TYPE is \"tag\" or \"ref\"
- BEG is the start of the tag name (after colon)
- END is point
- PREFIX is the text between colon and point
Returns nil if not inside a directive."
  (save-excursion
    (let ((pt (point))
          (line-beg (line-beginning-position)))
      (when (re-search-backward tagref--directive-start-regexp line-beg t)
        (let* ((type (match-string 1))
               (beg (match-end 0))
               (prefix (buffer-substring-no-properties beg pt)))
          ;; Only match if we haven't passed the closing bracket
          (unless (string-match-p "]" prefix)
            (list type beg pt prefix)))))))

(defun tagref-completion-at-point ()
  "Completion-at-point function for tagref directives."
  (when (tagref--in-enabled-project-p)
    (when-let ((directive (tagref--directive-at-point)))
      (pcase-let ((`(,type ,beg ,end ,_prefix) directive))
        (let ((tags (tagref--get-tags)))
          (when tags
            (list beg end
                  (mapcar #'car tags)
                  :exclusive 'no
                  :category 'tagref-tag
                  :annotation-function
                  (lambda (candidate)
                    (when-let ((info (assoc candidate tags)))
                      (format " %s:%d" (cadr info) (cddr info))))
                  :exit-function
                  (lambda (_candidate status)
                    (when (eq status 'finished)
                      ;; Insert closing bracket if not already present
                      (unless (looking-at-p "\\]")
                        (insert "]"))
                      ;; Convert [tag: to [ref: if user selected existing tag
                      (when (string= type "tag")
                        (save-excursion
                          (when (re-search-backward "\\[tag:" (line-beginning-position) t)
                            (replace-match "[ref:")))))))))))))

;;;; Xref Backend

(defun tagref--xref-backend ()
  "Return the tagref xref backend if in an enabled project."
  (when (tagref--in-enabled-project-p)
    'tagref))

(defun tagref--parse-directive-at-point ()
  "Parse the tagref directive at point.
Returns (TYPE . NAME) where TYPE is \"tag\" or \"ref\" and NAME is
the tag name, or nil if point is not inside a directive."
  (save-excursion
    (let ((pt (point))
          (line-beg (line-beginning-position))
          (line-end (line-end-position)))
      (when (re-search-backward tagref--directive-start-regexp line-beg t)
        (let ((type (match-string 1))
              (name-start (match-end 0)))
          (when (re-search-forward "\\]" line-end t)
            (let* ((bracket-pos (1- (point)))  ; position of ]
                   (name-end-pos (1- bracket-pos)))  ; last char of name
              (when (and (>= pt name-start) (<= pt name-end-pos))
                (cons type (string-trim (buffer-substring-no-properties
                                         name-start bracket-pos)))))))))))

(defun tagref--directive-type-at-point ()
  "Return the directive type at point (\"tag\" or \"ref\"), or nil."
  (car (tagref--parse-directive-at-point)))

(defun tagref--identifier-at-point ()
  "Return the tag name at point if inside a directive."
  (cdr (tagref--parse-directive-at-point)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql tagref)))
  "Return identifier at point for tagref backend."
  (tagref--identifier-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql tagref)) identifier)
  "Return xref definitions for IDENTIFIER.
On a [ref:...], jumps to the tag definition.
On a [tag:...], shows a message suggesting M-? instead."
  (when-let ((root (tagref--project-root)))
    (let ((directive-type (tagref--directive-type-at-point)))
      (if (string= directive-type "tag")
          ;; On a tag: suggest using M-? to find references
          (user-error "This is a tag definition.  Use M-? to find references")
        ;; On a ref (or elsewhere): show tag definition
        (let ((tags (tagref--get-tags)))
          (when-let ((info (assoc identifier tags)))
            (let* ((file (cadr info))
                   (line (cddr info))
                   (col (or (tagref--find-column-of-tag file line identifier) 0))
                   (full-path (expand-file-name file root)))
              (list (xref-make identifier
                               (xref-make-file-location full-path line col))))))))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql tagref)))
  "Return completion table for tagref identifiers."
  (mapcar #'car (tagref--get-tags)))

(defun tagref--find-refs (tag-name)
  "Find all references to TAG-NAME in the project.
Returns a list of (FILE . LINE-NUMBER) pairs.
Uses ripgrep if available, then git grep in git repos, then grep."
  (when-let ((root (tagref--project-root)))
    (let ((default-directory root)
          (literal-pattern (format "[ref:%s]" tag-name))
          (use-rg (tagref--ripgrep-available-p))
          (use-git (tagref--in-git-repo-p))
          results)
      (with-temp-buffer
        (let ((exit-code
               (cond
                ;; Prefer ripgrep (fastest, respects .gitignore by default)
                (use-rg
                 (call-process "rg" nil t nil
                               "--line-number" "--fixed-strings"
                               "--no-heading" "--with-filename"
                               literal-pattern))
                ;; Git grep in git repos (fast, respects .gitignore)
                (use-git
                 (call-process "git" nil t nil
                               "grep" "-n" "--fixed-strings"
                               literal-pattern))
                ;; Fall back to grep
                (t
                 (call-process "grep" nil t nil
                               "-rn" "--include=*"
                               (format "\\[ref:%s\\]" tag-name) ".")))))
          (when (zerop exit-code)
            (goto-char (point-min))
            ;; rg and git grep output: FILE:LINE:...
            ;; grep output: ./FILE:LINE:...
            (let ((output-regexp (if (or use-rg use-git)
                                     "^\\([^:]+\\):\\([0-9]+\\):"
                                   "^\\./\\([^:]+\\):\\([0-9]+\\):")))
              (while (re-search-forward output-regexp nil t)
                (push (cons (match-string 1)
                            (string-to-number (match-string 2)))
                      results))))))
      (nreverse results))))

(cl-defmethod xref-backend-references ((_backend (eql tagref)) identifier)
  "Return xref references for IDENTIFIER (places where it is referenced).
When called from a [tag:...] directive, includes the tag definition
as the first entry to provide context.  Shows a message if there are
no references."
  (when-let ((root (tagref--project-root)))
    (let* ((refs (tagref--find-refs identifier))
           (on-tag-p (string= (tagref--directive-type-at-point) "tag")))
      ;; When on a tag with no references, show helpful message
      (when (and on-tag-p (null refs))
        (user-error "No references found for [tag:%s]" identifier))
      (let* ((ref-xrefs (mapcar (lambda (ref)
                                  (let* ((file (car ref))
                                         (line (cdr ref))
                                         (full-path (expand-file-name file root)))
                                    (xref-make (format "[ref:%s]" identifier)
                                               (xref-make-file-location full-path line 0))))
                                refs))
             ;; When on a tag, include the tag definition as first entry
             (tag-xref (when on-tag-p
                         (when-let* ((tags (tagref--get-tags))
                                     (info (assoc identifier tags)))
                           (let* ((file (cadr info))
                                  (line (cddr info))
                                  (col (or (tagref--find-column-of-tag file line identifier) 0))
                                  (full-path (expand-file-name file root)))
                             (xref-make (format "[tag:%s] (definition)" identifier)
                                        (xref-make-file-location full-path line col)))))))
        (if tag-xref
            (cons tag-xref ref-xrefs)
          ref-xrefs)))))

;;;; Check Command

(defvar tagref-error-regexp-alist
  `((tagref-error
     ;; Matches: @ ./path/to/file.el:42 or @ path/to/file:42
     ,(rx "@ " (group (+ (not ":"))) ":" (group (+ digit)))
     1 2 nil 2))
  "Compilation error regexp alist for tagref output.")

;;;###autoload
(defun tagref-check ()
  "Run tagref check and display results in a compilation buffer.
This command is project-aware and will enable `tagref-mode' for
the current project if not already enabled."
  (interactive)
  (unless (tagref--project-root)
    (user-error "Not in a project"))
  (tagref-mode 1)
  (let ((default-directory (tagref--project-root))
        (command (mapconcat #'shell-quote-argument
                            (append (list tagref-executable)
                                    tagref-arguments
                                    (list "check"))
                            " ")))
    (compilation-start command 'tagref-compilation-mode)))

(define-compilation-mode tagref-compilation-mode "Tagref"
  "Compilation mode for tagref check output."
  (setq-local compilation-error-regexp-alist-alist
              (append tagref-error-regexp-alist
                      compilation-error-regexp-alist-alist))
  (setq-local compilation-error-regexp-alist '(tagref-error))
  ;; Enable ANSI color interpretation for tagref's colored output
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))

;;;; Tag Listing

(defvar-local tagref-list--tags nil
  "Cache of tags for the current listing buffer.")

(defvar tagref-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'tagref-list-goto-tag)
    (define-key map (kbd "g") #'tagref-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `tagref-list-mode'.")

(define-derived-mode tagref-list-mode tabulated-list-mode "Tagref-List"
  "Major mode for displaying tagref tags.
Supports `next-error' navigation with \\[next-error] and \\[previous-error]."
  (setq tabulated-list-format [("Tag" 40 t)
                               ("File" 50 t)
                               ("Line" 6 t)])
  (setq tabulated-list-sort-key '("Tag" . nil))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  ;; Enable next-error support
  (setq-local next-error-function #'tagref-list-next-error)
  (setq-local next-error-last-buffer (current-buffer)))

(defun tagref-list--make-entries (tags)
  "Convert TAGS alist to tabulated-list entries."
  (mapcar (lambda (tag)
            (let ((name (car tag))
                  (file (cadr tag))
                  (line (cddr tag)))
              (list name (vector name file (number-to-string line)))))
          tags))

(defun tagref-list-refresh ()
  "Refresh the tag listing."
  (interactive)
  (let ((tags (tagref--get-tags)))
    (setq tagref-list--tags tags)
    (setq tabulated-list-entries (tagref-list--make-entries tags))
    (tabulated-list-print t)))

(defun tagref-list-goto-tag ()
  "Go to the tag at point."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry))
              (name (aref entry 0))
              (info (assoc name tagref-list--tags))
              (file (cadr info))
              (line (cddr info))
              (root (tagref--project-root))
              (full-path (expand-file-name file root)))
    (find-file full-path)
    (goto-char (point-min))
    (forward-line (1- line))
    ;; Try to position at the tag itself
    (when (re-search-forward (format "\\[tag:%s\\]" (regexp-quote name))
                             (line-end-position) t)
      (goto-char (match-beginning 0)))))

(defun tagref-list-next-error (n &optional reset)
  "Move to the Nth next tag in the list and visit it.
This function is used by `next-error' and `previous-error'.
If RESET is non-nil, go to the first tag."
  (interactive "p")
  (when reset
    (goto-char (point-min))
    (forward-line 2))  ; Skip header
  (forward-line n)
  ;; Ensure we're on a valid entry
  (when (eobp)
    (forward-line -1))
  (when (< (line-number-at-pos) 3)
    (goto-char (point-min))
    (forward-line 2))
  (tagref-list-goto-tag))

;;;###autoload
(defun tagref-list-tags ()
  "Display a list of all tags in the project.
This command is project-aware and will enable `tagref-mode' for
the current project if not already enabled.
The resulting buffer supports `next-error' navigation."
  (interactive)
  (unless (tagref--project-root)
    (user-error "Not in a project"))
  (tagref-mode 1)
  (let ((root (tagref--project-root)))
    (with-current-buffer (get-buffer-create "*tagref-tags*")
      (let ((default-directory root))
        (tagref-list-mode)
        (tagref-list-refresh)
        (pop-to-buffer (current-buffer))
        ;; Register as next-error source
        (setq next-error-last-buffer (current-buffer))))))

;;;; Minor Mode

(defun tagref--enable-in-buffer ()
  "Enable tagref features in the current buffer if in an enabled project."
  (when (and (tagref--in-enabled-project-p)
             ;; Only enable in programming/text modes to avoid slowing
             ;; down search buffers and other special buffers
             (derived-mode-p 'prog-mode 'text-mode 'conf-mode))
    (setq tagref-mode t)
    (tagref--enable-font-lock)
    ;; Add completion and xref hooks
    (add-hook 'completion-at-point-functions #'tagref-completion-at-point -90 t)
    (add-hook 'xref-backend-functions #'tagref--xref-backend nil t)))

(defun tagref--disable-in-buffer ()
  "Disable tagref features in the current buffer."
  (setq tagref-mode nil)
  (tagref--disable-font-lock)
  (remove-hook 'completion-at-point-functions #'tagref-completion-at-point t)
  (remove-hook 'xref-backend-functions #'tagref--xref-backend t))

(defun tagref--enable-in-project ()
  "Enable `tagref-mode' for all buffers in the current project."
  (when-let ((root (tagref--project-root)))
    (puthash root t tagref--enabled-projects)
    ;; Enable in all existing project buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (tagref--project-root)
                   (string= (tagref--project-root) root))
          (tagref--enable-in-buffer))))
    ;; Hook for future buffers
    (add-hook 'find-file-hook #'tagref--enable-in-buffer)))

(defun tagref--disable-in-project ()
  "Disable `tagref-mode' for all buffers in the current project."
  (when-let ((root (tagref--project-root)))
    (remhash root tagref--enabled-projects)
    ;; Disable in all existing project buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (tagref--project-root)
                   (string= (tagref--project-root) root))
          (tagref--disable-in-buffer))))
    ;; Remove hook if no projects are enabled
    (when (zerop (hash-table-count tagref--enabled-projects))
      (remove-hook 'find-file-hook #'tagref--enable-in-buffer))))

;;;###autoload
(define-minor-mode tagref-mode
  "Toggle `tagref-mode' for the current project.

When enabled, provides project-wide:
- Font-lock highlighting for [tag:...] and [ref:...] directives
- Completion for [ref:] and [tag:] directives
- Xref integration for \\[xref-find-definitions] navigation to tag definitions
- `tagref-check' command for validation
- `tagref-list-tags' command for browsing tags

This mode is project-aware: enabling it activates tagref features
for all buffers in the current project."
  :lighter " Tagref"
  :group 'tagref
  (if tagref-mode
      (if (tagref--project-root)
          (tagref--enable-in-project)
        (setq tagref-mode nil)
        (user-error "Not in a project; tagref-mode requires project.el"))
    (tagref--disable-in-project)))

(provide 'tagref)
;;; tagref.el ends here
