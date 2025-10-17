;;; ox-ipynb-v2-test.el --- Tests for ox-ipynb-v2  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>

;;; Commentary:
;; Test suite for ox-ipynb-v2.el

;;; Code:

(require 'ert)
(require 'ox-ipynb-v2)
(require 'json)

(defvar ox-ipynb-v2-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing test files.")

(defvar ox-ipynb-v2-fixtures-dir
  (expand-file-name "fixtures" ox-ipynb-v2-test-dir)
  "Directory containing test fixture files.")

;;; Utility functions

(defun ox-ipynb-v2-test--load-fixture (filename)
  "Load org fixture FILENAME and return buffer."
  (let ((filepath (expand-file-name filename ox-ipynb-v2-fixtures-dir)))
    (find-file-noselect filepath)))

(defun ox-ipynb-v2-test--parse-json-buffer (buffer)
  "Parse JSON from BUFFER and return as hash table."
  (with-current-buffer buffer
    (goto-char (point-min))
    (json-read)))

;;; Tests

(ert-deftest ox-ipynb-v2-test-language-detection ()
  "Test that language detection works correctly."
  (with-temp-buffer
    (insert "#+TITLE: Test\n\n")
    (insert "#+BEGIN_SRC python\n")
    (insert "print('hello')\n")
    (insert "#+END_SRC\n")
    (org-mode)
    (should (eq 'python (ox-ipynb-v2--get-language)))))

(ert-deftest ox-ipynb-v2-test-language-keyword ()
  "Test explicit language keyword."
  (with-temp-buffer
    (insert "#+OX-IPYNB-LANGUAGE: R\n\n")
    (insert "#+BEGIN_SRC python\n")
    (insert "print('hello')\n")
    (insert "#+END_SRC\n")
    (org-mode)
    (should (eq 'R (ox-ipynb-v2--get-language)))))

(ert-deftest ox-ipynb-v2-test-export-file-name ()
  "Test export filename detection."
  (with-temp-buffer
    (insert "#+EXPORT_FILE_NAME: custom.ipynb\n\n")
    (insert "Some content\n")
    (org-mode)
    (should (string= "custom.ipynb" (ox-ipynb-v2--get-export-file-name)))))

(ert-deftest ox-ipynb-v2-test-markdown-split-simple ()
  "Test splitting markdown with code blocks."
  (let ((markdown "# Header\n\nSome text.\n\n```python\nprint('hello')\n```\n\nMore text.\n"))
    (clrhash ox-ipynb-v2--src-block-results)
    (let ((cells (ox-ipynb-v2--split-markdown-cells markdown)))
      (should (= 3 (length cells)))
      (should (string= "markdown" (plist-get (nth 0 cells) :type)))
      (should (string= "code" (plist-get (nth 1 cells) :type)))
      (should (string= "python" (plist-get (nth 1 cells) :language)))
      (should (string= "markdown" (plist-get (nth 2 cells) :type))))))

(ert-deftest ox-ipynb-v2-test-cell-to-json-markdown ()
  "Test converting markdown cell to JSON."
  (let* ((cell '(:type "markdown" :content "# Test\n\nContent"))
         (json-cell (ox-ipynb-v2--cell-to-json cell)))
    (should (string= "markdown" (cdr (assq 'cell_type json-cell))))
    (should (assq 'id json-cell))
    (should (assq 'metadata json-cell))
    (should (assq 'source json-cell))))

(ert-deftest ox-ipynb-v2-test-cell-to-json-code ()
  "Test converting code cell to JSON."
  (let* ((cell '(:type "code" :language "python" :content "print('hello')"))
         (json-cell (ox-ipynb-v2--cell-to-json cell)))
    (should (string= "code" (cdr (assq 'cell_type json-cell))))
    (should (assq 'execution_count json-cell))
    (should (assq 'outputs json-cell))
    (should (assq 'source json-cell))))

(ert-deftest ox-ipynb-v2-test-notebook-structure ()
  "Test that notebook JSON has correct structure."
  (let* ((cells '((:type "markdown" :content "# Test")))
         (nb-json (ox-ipynb-v2--build-notebook-json cells 'python)))
    (should (= 4 (cdr (assq 'nbformat nb-json))))
    (should (assq 'cells nb-json))
    (should (assq 'metadata nb-json))))

(ert-deftest ox-ipynb-v2-test-simple-export-direct ()
  "Test exporting simple.org with direct method."
  (let* ((fixture-buffer (ox-ipynb-v2-test--load-fixture "simple.org"))
         (output-buffer nil))
    (unwind-protect
        (progn
          (with-current-buffer fixture-buffer
            (setq output-buffer (ox-ipynb-v2-export-to-buffer nil nil nil nil nil 'direct)))

          (should (bufferp output-buffer))

          (with-current-buffer output-buffer
            (let ((json-obj (ox-ipynb-v2-test--parse-json-buffer output-buffer)))
              (should (hash-table-p json-obj))
              (should (= 4 (gethash "nbformat" json-obj)))
              (should (arrayp (gethash "cells" json-obj)))
              (should (> (length (gethash "cells" json-obj)) 0)))))

      ;; Cleanup
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer))
      (when (buffer-live-p fixture-buffer)
        (kill-buffer fixture-buffer)))))

(ert-deftest ox-ipynb-v2-test-latex-filter ()
  "Test LaTeX fragment filtering."
  (should (string= "$$x^2$$"
                   (ox-ipynb-v2--filter-latex-fragment "\\[x^2\\]" 'md nil)))
  (should (string= "$x$"
                   (ox-ipynb-v2--filter-latex-fragment "\\(x\\)" 'md nil))))

(provide 'ox-ipynb-v2-test)

;;; ox-ipynb-v2-test.el ends here
