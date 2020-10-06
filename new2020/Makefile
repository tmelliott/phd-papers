FILE = main
TARGET = $(FILE).pdf

RMD_FILES = $(shell find sections -name '*.Rmd')
TEX_FILES = $(patsubst %.Rmd, %.tex, $(RMD_FILES))

default: $(TARGET)

install:
	@Rscript -e 'install.packages(c("stringr", "rmarkdown", "knitr", "tidyverse", "patchwork", "viridis", "kableExtra"))'

$(TARGET): $(FILE).tex $(TEX_FILES) allrefs.bib
	@echo "Building $@"
	@latexmk -pdf -silent $<
	@echo Done!

$(FILE).tex: $(FILE).Rnw $(TEX_FILES)
	@echo "Compiling $< -> $@"
	@Rscript -e 'knitr::knit("$<", quiet = TRUE)'
	@echo ""

%.tex: %.Rmd
	@echo "Compiling $< -> $@"
	@Rscript -e 'rmarkdown::render("$<", output_format = rmarkdown::pdf_document(keep_tex = TRUE), quiet = TRUE)'
	@rm $*.pdf
	@echo "Cleaning $@"
	@Rscript clean_tex.R $@
	@echo ""

clean:
	@latexmk -pdf -c $(FILE).tex
	@rm -f sections/*.log

fullclean:
	@latexmk -pdf -C -f $(FILE).tex
	@rm -f $(FILE).tex sections/*.tex sections/*.log