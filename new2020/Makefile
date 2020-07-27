FILE = main
TARGET = $(FILE).pdf

RMD_FILES = $(shell find sections -name '*.Rmd')
TEX_FILES = $(patsubst %.Rmd, %.tex, $(RMD_FILES))

default: $(TARGET)

$(TARGET): $(FILE).Rnw $(TEX_FILES) allrefs.bib
	@Rscript -e 'knitr::knit("$<")'
	@latexmk -pdf $<

%.tex: %.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_format = rmarkdown::pdf_document(keep_tex = TRUE))'
	@rm $*.pdf
	@Rscript clean_tex.R $@


