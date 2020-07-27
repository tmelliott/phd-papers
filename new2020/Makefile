FILE = main
TARGET = $(FILE).pdf

RMD_FILES = $(shell find sections -name '*.Rmd')
TEX_FILES = $(patsubst %.Rmd, %.tex, $(RMD_FILES))

default: $(TARGET)

$(TARGET): $(FILE).tex $(TEX_FILES) allrefs.bib
	@latexmk -pdf $<

$(FILE).tex: $(FILE).Rnw $(TEX_FILES)
	@Rscript -e 'knitr::knit("$<")'

%.tex: %.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_format = rmarkdown::pdf_document(keep_tex = TRUE))'
	@rm $*.pdf
	@Rscript clean_tex.R $@

clean:
	@latexmk -pdf -c $(FILE).tex
