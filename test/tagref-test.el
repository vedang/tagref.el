;;; tagref-test.el --- Tests for tagref.el -*- lexical-binding: t -*-

;;; Commentary:

;; Buttercup tests for tagref.el

;;; Code:

(require 'buttercup)
(require 'tagref)

;;; Package Structure Tests

(describe "tagref package"
  (it "provides tagref feature"
    (expect (featurep 'tagref) :to-be t))

  (it "defines tagref-mode"
    (expect (fboundp 'tagref-mode) :to-be t))

  (it "defines tagref customization group"
    (expect (get 'tagref 'custom-group) :not :to-be nil)))

;;; Customization Tests

(describe "tagref customization"
  (it "has tagref-executable variable"
    (expect (boundp 'tagref-executable) :to-be t)
    (expect tagref-executable :to-equal "tagref"))

  (it "has tagref-arguments variable"
    (expect (boundp 'tagref-arguments) :to-be t)
    (expect tagref-arguments :to-equal nil)))

;;; Minor Mode Tests

(describe "tagref-mode"
  (it "can be enabled in a buffer"
    (with-temp-buffer
      (tagref-mode 1)
      (expect tagref-mode :to-be t)))

  (it "can be disabled in a buffer"
    (with-temp-buffer
      (tagref-mode 1)
      (tagref-mode -1)
      (expect tagref-mode :to-be nil)))

  (it "adds capf to completion-at-point-functions when enabled"
    (with-temp-buffer
      (tagref-mode 1)
      (expect (memq 'tagref--capf completion-at-point-functions) :not :to-be nil)))

  (it "removes capf from completion-at-point-functions when disabled"
    (with-temp-buffer
      (tagref-mode 1)
      (tagref-mode -1)
      (expect (memq 'tagref--capf completion-at-point-functions) :to-be nil)))

  (it "adds xref backend when enabled"
    (with-temp-buffer
      (tagref-mode 1)
      (expect (memq 'tagref--xref-backend xref-backend-functions) :not :to-be nil)))

  (it "removes xref backend when disabled"
    (with-temp-buffer
      (tagref-mode 1)
      (tagref-mode -1)
      (expect (memq 'tagref--xref-backend xref-backend-functions) :to-be nil))))

;;; Utility Function Tests

(describe "tagref--project-root"
  (it "returns a directory"
    (expect (file-directory-p (tagref--project-root)) :to-be t)))

(describe "tagref--parse-tag-line"
  (it "parses valid tag line"
    (let ((result (tagref--parse-tag-line "[tag:my_tag] @ src/main.rs:42")))
      (expect result :not :to-be nil)
      (expect (car result) :to-equal "my_tag")
      (expect (cadr result) :to-equal "src/main.rs")
      (expect (cddr result) :to-equal 42)))

  (it "parses tag with spaces"
    (let ((result (tagref--parse-tag-line "[tag:my tag name] @ path/to/file.py:100")))
      (expect result :not :to-be nil)
      (expect (car result) :to-equal "my tag name")))

  (it "returns nil for invalid line"
    (expect (tagref--parse-tag-line "not a tag line") :to-be nil)
    (expect (tagref--parse-tag-line "") :to-be nil)))

;;; Directive Detection Tests

(describe "tagref--directive-at-point"
  (it "detects ref directive"
    (with-temp-buffer
      (insert "[ref:my_tag]")
      (goto-char 8)  ; inside "my_tag"
      (let ((result (tagref--directive-at-point)))
        (expect result :not :to-be nil)
        (expect (nth 0 result) :to-equal "ref"))))

  (it "detects tag directive"
    (with-temp-buffer
      (insert "[tag:my_tag]")
      (goto-char 8)
      (let ((result (tagref--directive-at-point)))
        (expect result :not :to-be nil)
        (expect (nth 0 result) :to-equal "tag"))))

  (it "returns nil outside directive"
    (with-temp-buffer
      (insert "some text")
      (goto-char 5)
      (expect (tagref--directive-at-point) :to-be nil)))

  (it "returns nil after closing bracket"
    (with-temp-buffer
      (insert "[ref:my_tag] more text")
      (goto-char 15)  ; in "more text"
      (expect (tagref--directive-at-point) :to-be nil))))

;;; Identifier at Point Tests

(describe "tagref--identifier-at-point"
  (it "extracts tag name from ref"
    (with-temp-buffer
      (insert "[ref:my_tag]")
      (goto-char 8)
      (expect (tagref--identifier-at-point) :to-equal "my_tag")))

  (it "extracts tag name from tag"
    (with-temp-buffer
      (insert "[tag:another_tag]")
      (goto-char 10)
      (expect (tagref--identifier-at-point) :to-equal "another_tag")))

  (it "trims whitespace"
    (with-temp-buffer
      (insert "[ref: spaced_tag ]")
      (goto-char 10)
      (expect (tagref--identifier-at-point) :to-equal "spaced_tag")))

  (it "returns nil outside directive"
    (with-temp-buffer
      (insert "regular text")
      (goto-char 5)
      (expect (tagref--identifier-at-point) :to-be nil))))

;;; Command Tests

(describe "tagref-check"
  (it "is an interactive command"
    (expect (commandp 'tagref-check) :to-be t)))

(describe "tagref-list-tags"
  (it "is an interactive command"
    (expect (commandp 'tagref-list-tags) :to-be t)))

;;; tagref-test.el ends here
