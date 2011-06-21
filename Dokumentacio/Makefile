LATEX		= xelatex
LATEXOPTS	= -shell-escape

all: doc.pdf

doc.aux doc.toc: doc.tex
	$(LATEX) $(LATEXOPTS) $<

doc.pdf: doc.tex doc.toc doc.aux
	$(LATEX) $(LATEXOPTS) doc.tex
