file = anzjs

texfiles = $(shell find sections -name '*.tex')

default: $(file).pdf

$(file).pdf: $(file).tex $(texfiles)
	@latexmk -pdf $<

# html: $(file).dvi
# 	latex2html -dir html -mkdir $<

# $(file).dvi: $(file).tex | $(texfiles)
# 	@latexmk -f $<

SHELL=/bin/bash
submission: $(file).pdf
	@rm -rf final
	@mkdir -p final/figures
	@cp $(file).tex final/
	@cp figures/02_*.pdf \
		figures/03_particle_filter_{1,2,4}.pdf \
		figures/04_*.pdf \
		final/figures/
	@bibexport -o reflist2.bib anzjs.aux
	@mv reflist2.bib final/reflist.bib
	@cd final; zip figures.zip figures

check_submission:
	@cp anzsauth.cls anzsj.bst final
	@cd final; pdflatex anzjs; bibtex anzjs; pdflatex anzjs; pdflatex anzjs; pdflatex anzjs
