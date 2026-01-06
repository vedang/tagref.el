;;; tagref.el --- Tagref cross-reference support -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Tagref Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/stepchowfun/tagref

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

;;;; Xref Backend (stub)

(defun tagref--xref-backend ()
  "Return the tagref xref backend if in `tagref-mode'."
  nil)

;;;; Commands (stubs)

;;;###autoload
(defun tagref-check ()
  "Run tagref check and display results in a compilation buffer."
  (interactive)
  (message "Not yet implemented"))

;;;###autoload
(defun tagref-list-tags ()
  "Display a list of all tags in the project."
  (interactive)
  (message "Not yet implemented"))

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
