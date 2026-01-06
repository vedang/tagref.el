;;; tagref.el --- Tagref cross-reference support -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tagref Contributors

;; Author: Tagref Contributors
;; Maintainer: Tagref Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; Homepage: https://github.com/stepchowfun/tagref

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Emacs integration for tagref, a tool for managing
;; cross-references in code.  It provides:
;;
;; - Completion for [ref:] and [tag:] directives
;; - Xref integration for navigating to tag definitions (M-.)
;; - A check command that displays errors in a compilation buffer
;; - A tag listing command for browsing all tags
;;
;; Enable `tagref-mode' in buffers where you want tagref support.

;;; Code:

(require 'cl-lib)
(require 'xref)
(require 'compile)
(require 'tabulated-list)

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

;; Forward declaration for byte-compiler
(defvar tagref-mode)

;;;; Utilities

(defun tagref--project-root ()
  "Return the project root directory.
Uses `project-root' if available, otherwise `default-directory'."
  (or (when-let ((proj (project-current)))
        (if (fboundp 'project-root)
            (project-root proj)
          ;; Emacs 27 compatibility
          (with-no-warnings
            (car (project-roots proj)))))
      default-directory))

(defun tagref--call-process (&rest args)
  "Call tagref with ARGS and return output as string.
Returns nil if tagref fails."
  (let ((default-directory (tagref--project-root)))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process
                              tagref-executable
                              nil t nil
                              (append tagref-arguments args))))
        (when (zerop exit-code)
          (buffer-string))))))

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

;;;; Completion

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
      (when (re-search-backward "\\[\\(tag\\|ref\\):" line-beg t)
        (let* ((type (match-string 1))
               (beg (match-end 0))
               (prefix (buffer-substring-no-properties beg pt)))
          ;; Only match if we haven't passed the closing bracket
          (unless (string-match-p "]" prefix)
            (list type beg pt prefix)))))))

(defun tagref--capf ()
  "Completion-at-point function for tagref directives."
  (when-let ((directive (tagref--directive-at-point)))
    (pcase-let ((`(,type ,beg ,end ,_prefix) directive))
      (let ((tags (tagref--get-tags)))
        (when tags
          (list beg end
                (mapcar #'car tags)
                :exclusive 'no
                :annotation-function
                (lambda (candidate)
                  (when-let ((info (assoc candidate tags)))
                    (format " %s:%d" (cadr info) (cddr info))))
                :exit-function
                (when (string= type "tag")
                  (lambda (_candidate status)
                    (when (eq status 'finished)
                      ;; Convert [tag: to [ref: if user selected existing tag
                      (save-excursion
                        (when (re-search-backward "\\[tag:" (line-beginning-position) t)
                          (replace-match "[ref:"))))))))))))

;;;; Xref Backend

(defun tagref--xref-backend ()
  "Return the tagref xref backend if in `tagref-mode'."
  (when tagref-mode
    'tagref))

(defun tagref--identifier-at-point ()
  "Return the tag name at point if inside a directive."
  (save-excursion
    (let ((pt (point))
          (line-beg (line-beginning-position))
          (line-end (line-end-position)))
      ;; Search backward for opening bracket
      (when (re-search-backward "\\[\\(tag\\|ref\\):" line-beg t)
        (let ((name-start (match-end 0)))
          ;; Find closing bracket
          (when (re-search-forward "\\]" line-end t)
            (let ((name-end (1- (point))))
              ;; Check if original point was within the name
              (when (and (>= pt name-start) (<= pt name-end))
                (string-trim (buffer-substring-no-properties
                              name-start name-end))))))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql tagref)))
  "Return identifier at point for tagref backend."
  (tagref--identifier-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql tagref)) identifier)
  "Return xref definitions for IDENTIFIER."
  (let ((tags (tagref--get-tags))
        (root (tagref--project-root)))
    (when-let ((info (assoc identifier tags)))
      (let* ((file (cadr info))
             (line (cddr info))
             (full-path (expand-file-name file root)))
        (list (xref-make identifier
                         (xref-make-file-location full-path line 0)))))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql tagref)))
  "Return completion table for tagref identifiers."
  (mapcar #'car (tagref--get-tags)))

;;;; Check Command

(defvar tagref-error-regexp-alist
  `((tagref-error
     ,(rx "at " (group (+ (not ":"))) ":" (group (+ digit)))
     1 2 nil 2))
  "Compilation error regexp alist for tagref output.")

;;;###autoload
(defun tagref-check ()
  "Run tagref check and display results in a compilation buffer."
  (interactive)
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
  (setq-local compilation-error-regexp-alist '(tagref-error)))

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
  "Major mode for displaying tagref tags."
  (setq tabulated-list-format [("Tag" 40 t)
                               ("File" 50 t)
                               ("Line" 6 t)])
  (setq tabulated-list-sort-key '("Tag" . nil))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

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
    (forward-line (1- line))))

;;;###autoload
(defun tagref-list-tags ()
  "Display a list of all tags in the project."
  (interactive)
  (let ((root (tagref--project-root)))
    (with-current-buffer (get-buffer-create "*tagref-tags*")
      (let ((default-directory root))
        (tagref-list-mode)
        (tagref-list-refresh)
        (pop-to-buffer (current-buffer))))))

;;;; Minor Mode

(defun tagref--enable ()
  "Enable `tagref-mode' features."
  (add-hook 'completion-at-point-functions #'tagref--capf nil t)
  (add-hook 'xref-backend-functions #'tagref--xref-backend nil t))

(defun tagref--disable ()
  "Disable `tagref-mode' features."
  (remove-hook 'completion-at-point-functions #'tagref--capf t)
  (remove-hook 'xref-backend-functions #'tagref--xref-backend t))

;;;###autoload
(define-minor-mode tagref-mode
  "Minor mode for tagref cross-reference support.

When enabled, provides:
- Completion for [ref:] and [tag:] directives
- Xref integration for \\[xref-find-definitions] navigation to tag definitions
- `tagref-check' command for validation
- `tagref-list-tags' command for browsing tags"
  :lighter " Tagref"
  :group 'tagref
  (if tagref-mode
      (tagref--enable)
    (tagref--disable)))

(provide 'tagref)
;;; tagref.el ends here
