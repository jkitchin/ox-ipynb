;;; test-subtree-export.el --- Test subtree export -*- lexical-binding: t; -*-

(add-to-list 'load-path default-directory)
(require 'ox-ipynb-v2)

;; Enable MyST format
(setq ox-ipynb-v2-myst-format t)

(let ((base-dir default-directory))
  ;; Open the simple.org file
  (find-file (expand-file-name "tests/fixtures/simple.org" base-dir))

  ;; Go to first heading
  (goto-char (point-min))
  (re-search-forward "^\\* " nil t)

  ;; Try subtree export
  (condition-case err
      (let ((md (ox-ipynb-v2--export-to-markdown t)))
        (with-temp-file (expand-file-name "tests/fixtures/simple-subtree.md" base-dir)
          (insert md))
        (message "SUCCESS: Subtree export completed without error")
        (message "Output written to tests/fixtures/simple-subtree.md"))
    (error
     (message "ERROR: %s" (error-message-string err)))))
