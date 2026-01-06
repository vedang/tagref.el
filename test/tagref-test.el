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

;;; Command Tests

(describe "tagref-check"
  (it "is an interactive command"
    (expect (commandp 'tagref-check) :to-be t)))

(describe "tagref-list-tags"
  (it "is an interactive command"
    (expect (commandp 'tagref-list-tags) :to-be t)))

;;; tagref-test.el ends here
