;;; test-drawer-export.el --- Test export with drawer results -*- lexical-binding: t; -*-

(add-to-list 'load-path default-directory)
(require 'ox-ipynb-v2)

(let ((base-dir default-directory))
  ;; Open the drawer-results.org file
  (find-file (expand-file-name "tests/fixtures/drawer-results.org" base-dir))

  ;; Export to markdown
  (let ((md (ox-ipynb-v2--export-to-markdown)))
    (with-temp-file (expand-file-name "tests/fixtures/drawer-results.md" base-dir)
      (insert md))
    (message "Markdown exported to tests/fixtures/drawer-results.md")

    ;; Check if the output contains drawer markers (it shouldn't)
    (if (or (string-match ":RESULTS:" md)
            (string-match ":END:" md))
        (message "FAILURE: Output still contains drawer markers")
      (message "SUCCESS: Drawer markers properly removed")))

  ;; Now test complete export to JSON
  (let* ((cells (ox-ipynb-v2--split-markdown-cells (ox-ipynb-v2--export-to-markdown)))
         (notebook-json (ox-ipynb-v2--build-notebook-json cells 'ipython))
         (json-str (json-encode notebook-json)))

    (with-temp-file (expand-file-name "tests/fixtures/drawer-results.ipynb" base-dir)
      (insert json-str))
    (message "Notebook exported to tests/fixtures/drawer-results.ipynb")

    ;; Check if the JSON contains the output without drawer markers
    (if (string-match ":RESULTS:" json-str)
        (message "FAILURE: JSON still contains :RESULTS: marker")
      (message "SUCCESS: JSON output is clean"))))
