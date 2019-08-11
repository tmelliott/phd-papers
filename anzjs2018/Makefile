file = anzjs

texfiles = $(shell find sections -name '*.tex')

default: $(file).pdf

$(file).pdf: $(file).tex $(texfiles)
	@latexmk -pdf $<

# html: $(file).dvi
# 	latex2html -dir html -mkdir $<

# $(file).dvi: $(file).tex | $(texfiles)
# 	@latexmk -f $<

