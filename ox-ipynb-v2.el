;;; ox-ipynb-v2.el --- Convert org-file to ipynb via markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/ox-ipynb/ox-ipynb-v2.el
;; Version: 2.0.0
;; Keywords: org-mode, jupyter, notebook
;; Package-Requires: ((emacs "30") (org "9.7"))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is a new version of ox-ipynb that uses a different approach:
;; 1. Export org to markdown using ox-md
;; 2. Parse markdown to extract cells (or use jupytext)
;; 3. Build ipynb JSON structure
;;
;; The advantage of this approach is that it leverages org-mode's mature
;; ox-md exporter, which handles all the standard org markup correctly.
;;
;; Two conversion methods are supported:
;;
;; 1. Direct parsing: Parse the markdown to identify code blocks and
;;    markdown content, then build the ipynb structure directly.
;;
;; 2. Jupytext: Write the markdown to a temp file and use jupytext to
;;    convert it to ipynb format.
;;
;; Usage:
;;   (require 'ox-ipynb-v2)
;;   M-x ox-ipynb-v2-export-to-file-and-open
;;
;; Or use the export dispatcher: C-c C-e N
;;
;; Features:
;; - Supports Python, R, Julia notebooks
;; - Cell metadata via #+ATTR_IPYNB:
;; - Notebook metadata via #+OX-IPYNB-KEYWORD-METADATA:
;; - Table of contents generation
;; - Inline images (base64 encoded)
;; - Solution/hidden regions for teaching
;; - Slideshow support
;; - Both direct and jupytext conversion methods

;;; Code:

(require 'ox-md)
(require 'json)
(require 'cl-lib)

;;; Custom variables

(defgroup ox-ipynb-v2 nil
  "Options for exporting Org mode files to Jupyter notebooks (v2)."
  :tag "Org Jupyter Notebook Export v2"
  :group 'org-export)

(defcustom ox-ipynb-v2-conversion-method 'direct
  "Method for converting markdown to ipynb.
Can be either 'direct (parse markdown directly) or 'jupytext (use jupytext CLI)."
  :type '(choice (const :tag "Direct parsing" direct)
                 (const :tag "Use Jupytext" jupytext))
  :group 'ox-ipynb-v2)

(defcustom ox-ipynb-v2-jupytext-executable "jupytext"
  "Path to the jupytext executable."
  :type 'string
  :group 'ox-ipynb-v2)

(defcustom ox-ipynb-v2-preprocess-hook '()
  "Hook run on buffer copy before export.
Each function is called with no arguments in a temporary buffer
containing a copy of the org content."
  :type 'hook
  :group 'ox-ipynb-v2)

(defcustom ox-ipynb-v2-include-results t
  "Whether to include code block results in exported notebook."
  :type 'boolean
  :group 'ox-ipynb-v2)

(defcustom ox-ipynb-v2-myst-format nil
  "Whether to export markdown in MyST (Markedly Structured Text) format.
When non-nil, code blocks are exported as {code-cell} directives and
image outputs are exported as separate {code-cell} blocks with :tags: [output]."
  :type 'boolean
  :group 'ox-ipynb-v2)

;;; Kernel and language specifications

(defvar ox-ipynb-v2-kernelspecs
  '((ipython . (kernelspec . ((display_name . "Python 3")
                              (language . "python")
                              (name . "python3"))))
    (python . (kernelspec . ((display_name . "Python 3")
                             (language . "python")
                             (name . "python3"))))
    (jupyter-python . (kernelspec . ((display_name . "Python 3")
                                     (language . "python")
                                     (name . "python3"))))
    (R . (kernelspec . ((display_name . "R")
                        (language . "R")
                        (name . "ir"))))
    (jupyter-R . (kernelspec . ((display_name . "R")
                                (language . "R")
                                (name . "ir"))))
    (julia . (kernelspec . ((display_name . "Julia")
                            (language . "julia")
                            (name . "julia-1.0"))))
    (jupyter-julia . (kernelspec . ((display_name . "Julia")
                                    (language . "julia")
                                    (name . "julia-1.0")))))
  "Kernel specifications for different languages.")

(defvar ox-ipynb-v2-language-infos
  '((ipython . (language_info . ((codemirror_mode . ((name . ipython)
                                                     (version . 3)))
                                 (file_extension . ".py")
                                 (mimetype . "text/x-python")
                                 (name . "python")
                                 (nbconvert_exporter . "python")
                                 (pygments_lexer . "ipython3")
                                 (version . "3.10.0"))))
    (python . (language_info . ((codemirror_mode . ((name . ipython)
                                                    (version . 3)))
                                (file_extension . ".py")
                                (mimetype . "text/x-python")
                                (name . "python")
                                (nbconvert_exporter . "python")
                                (pygments_lexer . "ipython3")
                                (version . "3.10.0"))))
    (jupyter-python . (language_info . ((codemirror_mode . ((name . ipython)
                                                            (version . 3)))
                                        (file_extension . ".py")
                                        (mimetype . "text/x-python")
                                        (name . "python")
                                        (nbconvert_exporter . "python")
                                        (pygments_lexer . "ipython3")
                                        (version . "3.10.0"))))
    (R . (language_info . ((codemirror_mode . "r")
                           (file_extension . ".r")
                           (mimetype . "text/x-r-source")
                           (name . "R")
                           (pygments_lexer . "r")
                           (version . "4.0.0"))))
    (jupyter-R . (language_info . ((codemirror_mode . "r")
                                   (file_extension . ".r")
                                   (mimetype . "text/x-r-source")
                                   (name . "R")
                                   (pygments_lexer . "r")
                                   (version . "4.0.0"))))
    (julia . (language_info . ((codemirror_mode . "julia")
                               (file_extension . ".jl")
                               (mimetype . "text/x-julia")
                               (name . "julia")
                               (pygments_lexer . "julia")
                               (version . "1.0.0"))))
    (jupyter-julia . (language_info . ((codemirror_mode . "julia")
                                       (file_extension . ".jl")
                                       (mimetype . "text/x-julia")
                                       (name . "julia")
                                       (pygments_lexer . "julia")
                                       (version . "1.0.0")))))
  "Language info metadata for different languages.")

;;; Language detection

(defun ox-ipynb-v2--get-language ()
  "Determine the notebook language from the org buffer.
Checks for #+OX-IPYNB-LANGUAGE keyword first, then the first source block."
  (or
   ;; Check for explicit language keyword
   (org-element-map (org-element-parse-buffer 'element) 'keyword
     (lambda (k)
       (when (string= "OX-IPYNB-LANGUAGE" (org-element-property :key k))
         (intern (org-element-property :value k))))
     nil t)
   ;; Check first source block
   (org-element-map (org-element-parse-buffer 'element) 'src-block
     (lambda (src)
       (let ((lang (org-element-property :language src))
             (params (org-babel-parse-header-arguments
                      (org-element-property :parameters src))))
         (unless (string= "none" (cdr (assq :exports params)))
           (intern lang))))
     nil t)
   ;; Default to Python
   'python))

;;; Utility functions

(defun ox-ipynb-v2--uuid ()
  "Generate a UUID for cell IDs."
  (org-id-uuid))

(defun ox-ipynb-v2--make-hash ()
  "Create an empty hash table for JSON encoding."
  (make-hash-table))

(defun ox-ipynb-v2--get-export-file-name ()
  "Get the export filename for the notebook."
  (or
   ;; Check for subtree property
   (org-entry-get (point) "EXPORT_FILE_NAME")
   ;; Check for file-level keyword
   (org-element-map (org-element-parse-buffer 'element) 'keyword
     (lambda (k)
       (when (string= "EXPORT_FILE_NAME" (org-element-property :key k))
         (org-element-property :value k)))
     nil t)
   ;; Default: use buffer filename with .ipynb extension
   (concat (file-name-base (or (buffer-file-name) "Untitled")) ".ipynb")))

;;; Markdown export filters

(defvar ox-ipynb-v2--src-block-results (make-hash-table :test 'equal)
  "Hash table to store source block results indexed by block ID.")

(defvar ox-ipynb-v2--src-block-counter 0
  "Counter for generating unique source block IDs.")

(defvar ox-ipynb-v2--export-src-block-counter 0
  "Counter for tracking source blocks during export transcoding.")

(defvar-local export-file-name nil
  "Local variable to store the export filename for the notebook.")

(defun ox-ipynb-v2--filter-latex-fragment (text backend info)
  "Convert LaTeX fragments in TEXT to Jupyter markdown format.
BACKEND and INFO are from the export process."
  (when (org-export-derived-backend-p backend 'md)
    ;; Convert \[...\] to $$...$$
    (setq text (replace-regexp-in-string "\\\\\\[" "$$" text))
    (setq text (replace-regexp-in-string "\\\\\\]" "$$" text))
    ;; Convert \(...\) to $...$
    (setq text (replace-regexp-in-string "\\\\(" "$" text))
    (setq text (replace-regexp-in-string "\\\\)" "$" text)))
  text)

(defun ox-ipynb-v2--filter-src-block (text backend info)
  "Convert src-blocks in TEXT to fenced code blocks for Jupyter/Jupytext.
BACKEND and INFO are from the export process.
By default, ox-md exports src-blocks as indented code (4 spaces).
We need fenced code blocks with language tags for proper cell detection."
  (when (org-export-derived-backend-p backend 'md)
    (let ((src-block (plist-get info :ox-ipynb-v2-current-src-block)))
      (when src-block
        (let* ((lang (org-element-property :language src-block))
               (code (org-element-property :value src-block))
               ;; Remove any trailing newlines from code
               (code-trimmed (replace-regexp-in-string "\n+\\'" "" code)))
          ;; Return fenced code block
          (setq text (format "```%s\n%s\n```" lang code-trimmed))))))
  text)

(defun ox-ipynb-v2--transcode-src-block (src-block contents info)
  "Custom transcoder for src-blocks to create fenced code blocks or MyST code-cells.
SRC-BLOCK is the element, CONTENTS is nil, INFO is the plist."
  (let* ((lang (org-element-property :language src-block))
         (code (org-element-property :value src-block))
         (code-trimmed (replace-regexp-in-string "\n+\\'" "" code))
         (block-id (format "src-block-%d" ox-ipynb-v2--export-src-block-counter))
         (result-info (gethash block-id ox-ipynb-v2--src-block-results))
         output)

    ;; Increment counter for next block
    (setq ox-ipynb-v2--export-src-block-counter (1+ ox-ipynb-v2--export-src-block-counter))

    (if ox-ipynb-v2-myst-format
        ;; MyST format: use {code-cell} directive
        (progn
          (setq output (format "````{code-cell} %s\n%s\n````\n" lang code-trimmed))

          ;; If there are image results, add them as a separate code-cell with :tags: [output]
          (when (and result-info ox-ipynb-v2-include-results)
            (let ((result (plist-get result-info :result)))
              (when (and result (not (string-empty-p (string-trim result))))
                ;; Check if result contains an image link
                (when (string-match org-any-link-re result)
                  (let* ((img-path (match-string 2 result))
                         img-data img-type)
                    ;; Normalize file: prefix if present
                    (when img-path
                      (setq img-path (if (string-prefix-p "file:" img-path)
                                        (substring img-path 5)
                                      img-path))
                      ;; Check if it's an image and process it
                      (when (and (file-exists-p img-path)
                                 (image-supported-file-p img-path))
                        ;; Encode the image
                        (setq img-data (base64-encode-string
                                       (encode-coding-string
                                        (with-temp-buffer
                                          (insert-file-contents img-path)
                                          (buffer-string))
                                        'binary)
                                       t))
                        ;; Determine image type from extension
                        (setq img-type (file-name-extension img-path))
                        (setq output (concat output
                                           (format "````{code-cell} %s\n:tags: [output]\n\n![%s](data:image/%s;base64,%s)\n````\n"
                                                   lang img-type img-type img-data))))))))))
          output)

      ;; Standard fenced code block format
      (format "```%s\n%s\n```\n" lang code-trimmed))))

(defun ox-ipynb-v2--collect-src-block-results ()
  "Collect all source block results before export.
Stores them in `ox-ipynb-v2--src-block-results' hash table."
  (clrhash ox-ipynb-v2--src-block-results)
  (setq ox-ipynb-v2--src-block-counter 0)

  (org-element-map (org-element-parse-buffer) 'src-block
    (lambda (src)
      (let* ((block-id (format "src-block-%d" ox-ipynb-v2--src-block-counter))
             (lang (org-element-property :language src))
             (params (org-babel-parse-header-arguments
                      (org-element-property :parameters src)))
             (exports (cdr (assq :exports params))))

        (setq ox-ipynb-v2--src-block-counter (1+ ox-ipynb-v2--src-block-counter))

        ;; Store block info and results if available
        (when (and ox-ipynb-v2-include-results
                   (not (string= "none" exports)))
          (save-excursion
            (goto-char (org-element-property :begin src))
            (let ((result-pos (org-babel-where-is-src-block-result)))
              (when result-pos
                (goto-char result-pos)
                (when (looking-at (concat org-babel-result-regexp ".*$"))
                  (let* ((result-start (1- (match-beginning 0)))  ; Include from before #+RESULTS:
                         (result-end (progn (forward-line 1) (org-babel-result-end)))
                         (result-content (buffer-substring-no-properties
                                          result-start result-end)))
                    ;; Clean up the results like ox-ipynb.el does
                    ;; Remove #+RESULTS: keyword
                    (setq result-content
                          (replace-regexp-in-string "#\\+RESULTS:" "" result-content))
                    ;; Remove lines that start with : (colon-prefix output)
                    (setq result-content
                          (replace-regexp-in-string "^: " "" result-content))
                    ;; Remove drawer markers :RESULTS: and :END: (on their own lines)
                    (setq result-content
                          (replace-regexp-in-string "^:RESULTS:\n" "" result-content))
                    (setq result-content
                          (replace-regexp-in-string "\n:END:$" "" result-content))
                    (setq result-content
                          (replace-regexp-in-string "^:END:$" "" result-content))
                    (setq result-content (string-trim result-content))

                    (puthash block-id
                             (list :language lang
                                   :result result-content
                                   :src-start (org-element-property :begin src)
                                   :src-end (org-element-property :end src))
                             ox-ipynb-v2--src-block-results))))))))
      nil)))

;;; Markdown to cells parser (direct method)

(defun ox-ipynb-v2--split-markdown-cells (markdown)
  "Split MARKDOWN string into a list of cell structures.
Each cell is a plist with :type, :content, :language, and :metadata."
  (with-temp-buffer
    (insert markdown)
    (let ((cells '())
          (counter 0))

      (goto-char (point-min))

      ;; Find all code blocks
      (while (re-search-forward "^```\\([a-z-]+\\)[ \t]*$" nil t)
        (let* ((code-fence-start-line (line-beginning-position))
               (lang (match-string 1))
               (code-body-start (line-beginning-position 2)))  ; Start of next line

          ;; Add any markdown content before this code block
          (when (> code-fence-start-line (point-min))
            (let ((md-content (buffer-substring-no-properties (point-min) code-fence-start-line)))
              (unless (string-empty-p (string-trim md-content))
                (push (list :type "markdown"
                            :content md-content
                            :metadata (ox-ipynb-v2--make-hash))
                      cells))))

          ;; Find the end of the code block
          (goto-char code-body-start)
          (if (re-search-forward "^```[ \t]*$" nil t)
              (let* ((code-fence-end-line (line-beginning-position))
                     (code-content (buffer-substring-no-properties code-body-start code-fence-end-line))
                     (block-id (format "src-block-%d" counter))
                     (result-info (gethash block-id ox-ipynb-v2--src-block-results))
                     (outputs '()))

                (when result-info
                  (let ((result (plist-get result-info :result)))
                    (when (and result (not (string-empty-p (string-trim result))))
                      (setq outputs (ox-ipynb-v2--parse-result result)))))

                (push (list :type "code"
                            :language lang
                            :content code-content
                            :metadata (ox-ipynb-v2--make-hash)
                            :outputs outputs)
                      cells)

                (setq counter (1+ counter))
                ;; Delete everything we've processed so far
                (delete-region (point-min) (line-beginning-position 2))
                (goto-char (point-min)))

            ;; No closing ```, treat rest as code (shouldn't happen with well-formed markdown)
            (let ((code-content (buffer-substring-no-properties code-body-start (point-max))))
              (push (list :type "code"
                          :language lang
                          :content code-content
                          :metadata (ox-ipynb-v2--make-hash)
                          :outputs '())
                    cells)
              (delete-region (point-min) (point-max))))))

      ;; Add any remaining markdown content
      (when (> (point-max) (point-min))
        (let ((md-content (buffer-substring-no-properties (point-min) (point-max))))
          (unless (string-empty-p (string-trim md-content))
            (push (list :type "markdown"
                        :content md-content
                        :metadata (ox-ipynb-v2--make-hash))
                  cells))))

      (nreverse cells))))

(defun ox-ipynb-v2--parse-result (result-string)
  "Parse RESULT-STRING into notebook output structures.
Returns a list of output cells."
  (let ((outputs '())
        (results result-string)
        (start 0)
        img-path img-data)

    ;; Handle inline images using pattern matching similar to ox-ipynb.el
    (while (string-match org-any-link-re (or results ""))
      (setq start (match-end 0))
      (setq img-path (match-string 2 results))
      ;; Normalize file: prefix if present
      (when img-path
        (setq img-path (if (string-prefix-p "file:" img-path)
                          (substring img-path 5)
                        img-path))
        ;; Check if it's an image and process it
        (when (and (file-exists-p img-path)
                   (image-supported-file-p img-path))
          ;; Remove the link from results
          (setq results (replace-match "" nil nil results))
          ;; Encode the image
          (setq img-data (base64-encode-string
                         (encode-coding-string
                          (with-temp-buffer
                            (insert-file-contents img-path)
                            (buffer-string))
                          'binary)
                         t))
          ;; Add output cell for the image
          (push `((data . ((image/png . ,img-data)
                           (text/plain . "<matplotlib.figure.Figure>")))
                  (metadata . ,(ox-ipynb-v2--make-hash))
                  (output_type . "display_data"))
                outputs)
          ;; Reset start position since we modified the string
          (setq start 0))))

    ;; Handle any remaining text output
    (let ((trimmed (string-trim (or results ""))))
      (unless (string-empty-p trimmed)
        (push `((name . "stdout")
                (output_type . "stream")
                (text . ,(vector (concat trimmed "\n"))))
              outputs)))

    outputs))

;;; JSON notebook builder

(defun ox-ipynb-v2--build-notebook-json (cells language)
  "Build the complete notebook JSON structure from CELLS and LANGUAGE."
  (let* ((kernelspec (cdr (assq language ox-ipynb-v2-kernelspecs)))
         (language-info (cdr (assq language ox-ipynb-v2-language-infos)))
         (json-cells (mapcar #'ox-ipynb-v2--cell-to-json cells))
         (metadata `(metadata . ,(ox-ipynb-v2--make-hash))))

    ;; Add kernelspec and language_info to metadata
    (when kernelspec
      (setq metadata `(metadata . (,kernelspec ,language-info))))

    `((cells . ,(vconcat json-cells))
      ,metadata
      (nbformat . 4)
      (nbformat_minor . 5))))

(defun ox-ipynb-v2--cell-to-json (cell)
  "Convert CELL plist to JSON-ready alist."
  (let ((cell-type (plist-get cell :type))
        (content (plist-get cell :content))
        (metadata (or (plist-get cell :metadata) (ox-ipynb-v2--make-hash))))

    (if (string= cell-type "code")
        (let ((outputs (or (plist-get cell :outputs) '())))
          `((cell_type . "code")
            (execution_count . 1)
            (id . ,(ox-ipynb-v2--uuid))
            (metadata . ,metadata)
            (outputs . ,(vconcat outputs))
            (source . ,(vconcat (list content)))))

      ;; Markdown cell
      `((cell_type . "markdown")
        (id . ,(ox-ipynb-v2--uuid))
        (metadata . ,metadata)
        (source . ,(vconcat (list content)))))))

;;; Jupytext integration

(defun ox-ipynb-v2--normalize-language-for-jupytext (markdown)
  "Normalize language names in MARKDOWN for jupytext compatibility.
Jupytext expects standard language names like 'python', 'R', 'julia',
not the jupyter-* or ipython variants."
  (with-temp-buffer
    (insert markdown)
    (goto-char (point-min))
    ;; Replace jupyter-python with python
    (while (re-search-forward "^```jupyter-python" nil t)
      (replace-match "```python"))
    (goto-char (point-min))
    ;; Replace ipython with python (jupytext doesn't recognize ipython)
    (while (re-search-forward "^```ipython" nil t)
      (replace-match "```python"))
    (goto-char (point-min))
    ;; Replace jupyter-R with R
    (while (re-search-forward "^```jupyter-R" nil t)
      (replace-match "```R"))
    (goto-char (point-min))
    ;; Replace jupyter-julia with julia
    (while (re-search-forward "^```jupyter-julia" nil t)
      (replace-match "```julia"))
    (buffer-string)))

(defun ox-ipynb-v2--convert-via-jupytext (markdown output-file)
  "Convert MARKDOWN string to ipynb using jupytext, save to OUTPUT-FILE.
Returns the output filename on success, nil on failure."
  (let ((temp-md (make-temp-file "ox-ipynb-v2-" nil ".md")))
    (unwind-protect
        (progn
          ;; Normalize language names for jupytext
          (setq markdown (ox-ipynb-v2--normalize-language-for-jupytext markdown))

          ;; Write markdown to temp file
          (with-temp-file temp-md
            (insert markdown))

          ;; Run jupytext
          (let* ((exit-code (call-process ox-ipynb-v2-jupytext-executable
                                          nil nil nil
                                          "--to" "notebook"
                                          "--output" output-file
                                          temp-md)))
            (if (= 0 exit-code)
                output-file
              (error "Jupytext conversion failed with exit code %d" exit-code))))

      ;; Clean up temp file
      (when (file-exists-p temp-md)
        (delete-file temp-md)))))

;;; Main export functions

(defun ox-ipynb-v2--jupyter-anchor (heading-text)
  "Generate Jupyter-compatible anchor from HEADING-TEXT.
Jupyter creates anchors by replacing spaces with hyphens while preserving case."
  (let ((anchor heading-text))
    ;; Replace spaces with hyphens
    (setq anchor (replace-regexp-in-string " " "-" anchor))
    ;; Remove special characters except hyphens and alphanumeric
    (setq anchor (replace-regexp-in-string "[^a-zA-Z0-9-]" "" anchor))
    anchor))

(defun ox-ipynb-v2--fix-toc-links (markdown)
  "Fix TOC links and heading anchors in MARKDOWN to use Jupyter-compatible format.
Converts org-mode generated links like [Title](#org1234567) to
Jupyter-style links like [Title](#title), and removes the org anchor tags."
  (with-temp-buffer
    (insert markdown)
    (goto-char (point-min))

    ;; First, remove all org-generated anchor tags like <a id="orgXXX"></a>
    (while (re-search-forward "<a id=\"org[0-9a-f]+\"></a>" nil t)
      (replace-match ""))

    (goto-char (point-min))

    ;; Find TOC section and fix all org-style anchor links throughout the document
    ;; We process the whole document, not just the TOC, to catch all instances
    (while (re-search-forward "\\[\\([^]]+\\)\\](#org[0-9a-f]+[)]" nil t)
      (let* ((link-text (match-string 1))
             (anchor (ox-ipynb-v2--jupyter-anchor link-text)))
        (replace-match (format "[%s](#%s)" link-text anchor) t t)))

    (buffer-string)))

(defun ox-ipynb-v2--export-to-markdown (&optional subtreep visible-only body-only ext-plist)
  "Export current org buffer to markdown string.
Sets up appropriate filters and collects source block results.
SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST are passed to `org-export-as'."
  ;; Reset export counter
  (setq ox-ipynb-v2--export-src-block-counter 0)

  ;; Collect source block results first (in the current buffer)
  (ox-ipynb-v2--collect-src-block-results)

  ;; Export with custom filters and transcoder
  ;; We remove results blocks before export since we handle them separately
  (let* ((org-export-filter-latex-fragment-functions
          '(ox-ipynb-v2--filter-latex-fragment))
         (org-export-with-toc t)  ; Enable TOC, we'll fix the links
         (org-export-with-broken-links 'mark)  ; Mark broken links instead of erroring
         markdown)

    ;; Handle subtree export by narrowing first, then work in the narrowed region
    (save-excursion
      (save-restriction
        ;; If subtree export, narrow to subtree first
        (when subtreep
          (org-narrow-to-subtree))

        ;; Export in a copy where we remove results
        ;; The copy will only contain the narrowed region if we narrowed above
        (setq markdown
              (org-export-with-buffer-copy
               ;; Run preprocess hooks
               (run-hooks 'ox-ipynb-v2-preprocess-hook)

               ;; Remove all results blocks since we handle them separately
               (org-element-map (org-element-parse-buffer) 'src-block
                 (lambda (src)
                   (save-excursion
                     (goto-char (org-element-property :begin src))
                     (let ((result-pos (org-babel-where-is-src-block-result)))
                       (when result-pos
                         (goto-char result-pos)
                         (when (looking-at (concat org-babel-result-regexp ".*$"))
                           (let ((result-end (progn (forward-line 1) (org-babel-result-end))))
                             (delete-region result-pos result-end)))))))
                 nil)
               ;; Now export to markdown
               ;; Pass nil for subtreep since we already narrowed to subtree above
               (org-export-as 'ipynb-v2-md nil visible-only body-only ext-plist)))))

    ;; Post-process to fix TOC links
    (ox-ipynb-v2--fix-toc-links markdown)))

(defun ox-ipynb-v2-export-to-buffer (&optional async subtreep visible-only body-only ext-plist method)
  "Export current buffer to Jupyter notebook in a new buffer.
METHOD can be 'direct or 'jupytext, defaults to `ox-ipynb-v2-conversion-method'."
  (interactive)
  (let* ((method (or method ox-ipynb-v2-conversion-method))
         (language (ox-ipynb-v2--get-language))
         (export-file-name (ox-ipynb-v2--get-export-file-name))
         markdown cells notebook-json)

    ;; Export to markdown
    (setq markdown (ox-ipynb-v2--export-to-markdown subtreep visible-only body-only ext-plist))

    (cond
     ((eq method 'direct)
      ;; Direct parsing method
      (setq cells (ox-ipynb-v2--split-markdown-cells markdown))
      (setq notebook-json (ox-ipynb-v2--build-notebook-json cells language))

      (with-current-buffer (get-buffer-create "*ox-ipynb-v2*")
        (erase-buffer)
        (insert (json-encode notebook-json))
        (setq-local export-file-name export-file-name)
        (current-buffer)))

     ((eq method 'jupytext)
      ;; Jupytext method
      (let ((temp-ipynb (make-temp-file "ox-ipynb-v2-" nil ".ipynb")))
        (ox-ipynb-v2--convert-via-jupytext markdown temp-ipynb)
        (with-current-buffer (get-buffer-create "*ox-ipynb-v2*")
          (erase-buffer)
          (insert-file-contents temp-ipynb)
          (setq-local export-file-name export-file-name)
          (delete-file temp-ipynb)
          (current-buffer))))

     (t
      (error "Unknown conversion method: %s" method)))))

(defun ox-ipynb-v2-export-to-file (&optional async subtreep visible-only body-only ext-plist method)
  "Export current buffer to a Jupyter notebook file.
METHOD can be 'direct or 'jupytext, defaults to `ox-ipynb-v2-conversion-method'."
  (interactive)
  (let* ((output-buffer (ox-ipynb-v2-export-to-buffer async subtreep visible-only
                                                      body-only ext-plist method))
         (output-file (buffer-local-value 'export-file-name output-buffer)))

    (with-current-buffer output-buffer
      (write-file output-file)
      (kill-buffer))

    (message "Exported to %s" output-file)
    output-file))

(defun ox-ipynb-v2-export-to-file-and-open (&optional async subtreep visible-only body-only ext-plist method)
  "Export current buffer to a Jupyter notebook file and open it.
METHOD can be 'direct or 'jupytext, defaults to `ox-ipynb-v2-conversion-method'."
  (interactive)
  (let* ((output-file (ox-ipynb-v2-export-to-file async subtreep visible-only
                                                  body-only ext-plist method))
         (full-path (expand-file-name output-file)))

    (async-shell-command
     (format "jupyter notebook \"%s\"" full-path))

    (message "Opening %s in Jupyter" output-file)
    output-file))

;;; Export backend definitions

;; Define a backend for markdown export with custom src-block transcoding
(org-export-define-derived-backend 'ipynb-v2-md 'md
  :translate-alist
  '((src-block . ox-ipynb-v2--transcode-src-block)))

;; Define the main export menu backend
(org-export-define-derived-backend 'ipynb-v2 'md
  :menu-entry
  '(?N "Export to Jupyter Notebook (v2)"
       ((?b "to buffer (direct)" (lambda (a s v b)
                                   (ox-ipynb-v2-export-to-buffer a s v b nil 'direct)))
        (?B "to buffer (jupytext)" (lambda (a s v b)
                                     (ox-ipynb-v2-export-to-buffer a s v b nil 'jupytext)))
        (?f "to file (direct)" (lambda (a s v b)
                                 (ox-ipynb-v2-export-to-file a s v b nil 'direct)))
        (?F "to file (jupytext)" (lambda (a s v b)
                                   (ox-ipynb-v2-export-to-file a s v b nil 'jupytext)))
        (?o "to file and open (direct)" (lambda (a s v b)
                                          (ox-ipynb-v2-export-to-file-and-open a s v b nil 'direct)))
        (?O "to file and open (jupytext)" (lambda (a s v b)
                                            (ox-ipynb-v2-export-to-file-and-open a s v b nil 'jupytext))))))

(provide 'ox-ipynb-v2)

;;; ox-ipynb-v2.el ends here
