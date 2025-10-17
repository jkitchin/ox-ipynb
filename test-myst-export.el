;;; test-myst-export.el --- Test MyST markdown export -*- lexical-binding: t; -*-

(add-to-list 'load-path default-directory)
(require 'ox-ipynb-v2)

;; Enable MyST format
(setq ox-ipynb-v2-myst-format t)

(let ((base-dir default-directory))
  ;; Test with test-image.org
  (find-file (expand-file-name "examples/test-image.org" base-dir))
  (let ((md (ox-ipynb-v2--export-to-markdown)))
    (with-temp-file (expand-file-name "examples/test-image-myst.md" base-dir)
      (insert md))
    (message "MyST markdown exported to examples/test-image-myst.md"))

  ;; Also test with simple.org
  (find-file (expand-file-name "tests/fixtures/simple.org" base-dir))
  (let ((md (ox-ipynb-v2--export-to-markdown)))
    (with-temp-file (expand-file-name "tests/fixtures/simple-myst.md" base-dir)
      (insert md))
    (message "MyST markdown exported to tests/fixtures/simple-myst.md")))
