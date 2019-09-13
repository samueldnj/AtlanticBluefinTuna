export PATH := /Library/TeX/texbin:$(PATH)    # add LaTeX path
export PATH := /usr/local/bin:~/.local/bin:$(PATH) # add pandoc-citeproc-preamble

# Cluster targets
pdf: index.pdf
html: index.html
docx: index.docx

all: pdf html docx

## pdf
%.pdf: %.Rmd Makefile
	Rscript -e 'bookdown::render_book(input = "$<", clean = TRUE, output_format = "bookdown::pdf_document2")'

# html
%.html: %.Rmd Makefile
	Rscript -e 'bookdown::render_book(input = "$<", clean = TRUE, output_format = "bookdown::html_document2")'

# docx
%.docx: %.Rmd Makefile
	Rscript -e 'bookdown::render_book(input = "$<", clean = TRUE, output_format = "bookdown::word_document2")'

# all formats (depends on yaml header)
%.all: %.Rmd Makefile
	Rscript -e 'bookdown::render_book(input = "$<", output_format = "all")'
