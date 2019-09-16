export PATH := /Library/TeX/texbin:$(PATH)    # add LaTeX path
export PATH := /usr/local/bin:~/.local/bin:$(PATH) # add pandoc-citeproc-preamble

# Cluster targets
pdf:   tunaDelayMPdescription.pdf  #TandF.pdf supplemental1.pdf
html:  tunaDelayMPdescription.html #TandF.html supplemental1.html
docx:  tunaDelayMPdescription.docx

all: pdf html docx

## pdf
%.pdf: %.Rmd makefile
	Rscript -e 'rmarkdown::render(input = "$<", clean = FALSE, output_format = "bookdown::pdf_document2")'
