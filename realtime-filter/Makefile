FILE := realtime-filter

LATEX := latexmk -interaction=nonstopmode -pdf

RNW := $(FILE).Rnw
TEX := $(FILE).tex
PDF := $(FILE).pdf

TARGET := $(PDF)


$(PDF): $(TEX)
	$(LATEX) -pdf -f

$(TEX): $(RNW)
	R -e "knitr::knit(\"$(RNW)\")"

auto:
	while sleepuntilmod $(RNW) ; do make $(PDF) ; done


tidy:
	@latexmk -c

clean:
	@latexmk -bibtex -C
	@rm -rf auto
	@rm $(TEX)
