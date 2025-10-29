# Makefile for ox-ipynb examples
# Converts all .org files to .ipynb notebooks

# Find all .org files except README.org
ORG_FILES := $(filter-out README.org, $(wildcard *.org))
IPYNB_FILES := $(ORG_FILES:.org=.ipynb)

# Special handling files (require different export functions)
ORG_REF_FILE := 08-citations-org-ref.org
ORG_REF_IPYNB := $(ORG_REF_FILE:.org=.ipynb)

SLIDESHOW_FILE := 11-slideshow.org
SLIDESHOW_IPYNB := $(SLIDESHOW_FILE:.org=.ipynb)

PARTICIPANT_FILE := 14-participant.org
PARTICIPANT_IPYNB := $(PARTICIPANT_FILE:.org=.ipynb)

NO_RESULTS_FILE := 15-no-results.org
NO_RESULTS_IPYNB := $(NO_RESULTS_FILE:.org=.ipynb)

SPECIAL_FILES := $(ORG_REF_FILE) $(SLIDESHOW_FILE) $(PARTICIPANT_FILE) $(NO_RESULTS_FILE)
SPECIAL_IPYNB_FILES := $(SPECIAL_FILES:.org=.ipynb)

# Regular files (excluding special cases)
REGULAR_ORG_FILES := $(filter-out $(SPECIAL_FILES), $(ORG_FILES))
REGULAR_IPYNB_FILES := $(REGULAR_ORG_FILES:.org=.ipynb)

.PHONY: all clean open list help

# Default target: show help
.DEFAULT_GOAL := help

# Help target
help:
	@echo "Available targets:"
	@echo "  make all     - Build all .ipynb notebooks from .org files"
	@echo "  make open    - Build all notebooks and open them with 'uv run jupyter notebook'"
	@echo "  make clean   - Remove all generated .ipynb files and HTML slides"
	@echo "  make list    - List all notebook targets"
	@echo "  make <file>  - Build a specific notebook (e.g., make 01-basic.ipynb)"
	@echo ""
	@echo "Special notebooks:"
	@echo "  11-slideshow.ipynb         - Also generates 11-slideshow.slides.html"
	@echo "  14-participant.ipynb       - Removes solutions and hidden content"
	@echo "  15-no-results.ipynb        - Strips all execution results"
	@echo ""
	@echo "Examples:"
	@echo "  make all                    # Build all notebooks"
	@echo "  make 01-basic.ipynb        # Build just one notebook"
	@echo "  make 11-slideshow.ipynb    # Build slideshow (creates HTML slides too)"
	@echo "  make clean all             # Clean and rebuild everything"
	@echo "  make open                  # Build and open in Jupyter"

# Build all notebooks
all: $(IPYNB_FILES)
	@echo "All notebooks built successfully!"

# Rule for regular org files
$(REGULAR_IPYNB_FILES): %.ipynb: %.org
	@echo "Exporting $< to $@..."
	@emacs --batch \
		--eval "(add-to-list 'load-path \"$(shell pwd)/..\")" \
		--eval "(require 'ox-ipynb)" \
		--eval "(find-file \"$<\")" \
		--eval "(ox-ipynb-export-to-ipynb-file)" \
		--eval "(kill-emacs)"

# Special rule for org-ref file (requires org-ref preprocessing)
# Falls back to normal export if org-ref is not available
$(ORG_REF_IPYNB): $(ORG_REF_FILE)
	@echo "Exporting $< to $@..."
	@emacs --batch \
		--eval "(add-to-list 'load-path \"$(shell pwd)/..\")" \
		--eval "(add-to-list 'load-path \"$(shell pwd)/../../org-ref/\")" \
		--eval "(let ((default-directory \"/Users/jkitchin/.emacs.d/elpa/\")) (normal-top-level-add-subdirs-to-load-path))" \
		--eval "(require 'ox-ipynb)" \
		--eval "(condition-case err \
		          (progn \
		            (require 'org-ref) \
		            (message \"org-ref loaded successfully\")) \
		          (error \
		            (message \"org-ref not available: %s\" err)))" \
		--eval "(find-file \"$<\")" \
		--eval "(if (featurep 'org-ref) \
		          (let ((org-export-with-broken-links t) \
		                (org-export-before-parsing-hook '(org-ref-cite-natmove org-ref-csl-preprocess-buffer))) \
		            (message \"Exporting with org-ref preprocessing...\") \
		            (ox-ipynb-export-to-ipynb-file)) \
		          (progn \
		            (message \"Using standard export (org-ref not loaded)\") \
		            (ox-ipynb-export-to-ipynb-file)))" \
		--eval "(kill-emacs)"

# Special rule for slideshow file
# Exports to .ipynb with slide metadata, then converts to HTML slides
$(SLIDESHOW_IPYNB): $(SLIDESHOW_FILE)
	@echo "Exporting $< to $@ (as slides)..."
	@emacs --batch \
		--eval "(add-to-list 'load-path \"$(shell pwd)/..\")" \
		--eval "(require 'ox-ipynb)" \
		--eval "(find-file \"$<\")" \
		--eval "(ox-ipynb-export-to-ipynb-file)" \
		--eval "(kill-emacs)"
	@echo "Converting $@ to HTML slides..."
	@jupyter nbconvert "$@" --to slides

# Special rule for participant notebook
$(PARTICIPANT_IPYNB): $(PARTICIPANT_FILE)
	@echo "Exporting $< to $@ (participant notebook - removing solutions)..."
	@emacs --batch \
		--eval "(add-to-list 'load-path \"$(shell pwd)/..\")" \
		--eval "(require 'ox-ipynb)" \
		--eval "(find-file \"$<\")" \
		--eval "(ox-ipynb-export-to-participant-notebook)" \
		--eval "(kill-emacs)"

# Special rule for no-results notebook
$(NO_RESULTS_IPYNB): $(NO_RESULTS_FILE)
	@echo "Exporting $< to $@ (no results)..."
	@emacs --batch \
		--eval "(add-to-list 'load-path \"$(shell pwd)/..\")" \
		--eval "(require 'ox-ipynb)" \
		--eval "(find-file \"$<\")" \
		--eval "(ox-ipynb-export-to-ipynb-no-results-file-and-open)" \
		--eval "(kill-emacs)"

# Open all notebooks with uv
open: $(IPYNB_FILES)
	@echo "Opening notebooks with uv..."
	uv run jupyter notebook

# Clean generated notebooks and slides
clean:
	@echo "Cleaning generated .ipynb files and HTML slides..."
	rm -f $(IPYNB_FILES)
	rm -f $(SLIDESHOW_FILE:.org=.slides.html)

# Show list of targets
list:
	@echo "Regular notebooks:"
	@for file in $(REGULAR_IPYNB_FILES); do echo "  $$file"; done
	@echo "Special notebooks:"
	@echo "  $(ORG_REF_IPYNB)"
