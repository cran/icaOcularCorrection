DRIVERS = vignette_v1
R_PROG = R
R_LIB = `${R_PROG} RHOME`
TPUTS = "${TEXINPUTS}":${R_LIB}/share/texmf/
RNWSRC = vignette_v1
#TEXSRC = 

all: $(DRIVERS:=.pdf)
	${RM} *.out

allR: $(RNWSRC:=.R)
	touch allR

%.tex: %.Rnw
	${R_PROG} CMD Sweave $<

%.R: %.Rnw
	${R_PROG} CMD Stangle $<

%.pdf : %.tex
	TEXINPUTS=${TPUTS} texi2dvi --pdf $<
clean:
	$(RM) $(DRIVERS:=.pdf) $(RNWSRC:=.tex) \
          $(RNWSRC:=.R) Rplots.ps $(DRIVERS:=.out) allR figs/*.pdf figs/*.eps \
          *.aux *.log *.toc *.nav *.snm *.vrb *.dvi

dist:
	$(RM) $(RNWSRC:=.tex) \
          Rplots.ps $(DRIVERS:=.out) allR figs/*.pdf figs/*.eps \
          *.aux *.log *.toc *.nav *.snm *.vrb *.dvi

%_R.txt: %.R
	cp $< $@

%D.tex: %.tex
	@echo "\\documentclass[dvipsnames,pdflatex,beamer]{beamer}" >> $@
	@echo "\\input{"$*"}" >> $@
	@echo "\\end{document}" >> $@

%H.tex: %.tex
	@echo "\\documentclass[dvipsnames,pdflatex,handout]{beamer}" >> $@
	@echo "\\input{"$*"}" >> $@
	@echo "\\end{document}" >> $@

%H.pdf: %H.tex
	TEXINPUTS=${TPUTS} texi2dvi --pdf $<
#	texclean $<

%D.pdf: %D.tex
	TEXINPUTS=${TPUTS} texi2dvi --pdf $<
#	texclean $<

%-4.tex: %H.pdf
	@echo "\\documentclass[letterpaper,landscape]{article}" > $@
	@echo "\\usepackage{pdfpages}" >> $@
	@echo "\\begin{document}" >> $@
	@echo "\\includepdf[nup=2x2,pages=-]{"$*"H.pdf}" >> $@
	@echo "\\end{document}" >> $@

%-4.pdf: %-4.tex
	texi2dvi --pdf $<
#	texclean $<


%-4.tex: %.pdf
	@echo "\\documentclass[letterpaper,landscape]{article}" > $@
	@echo "\\usepackage{pdfpages}" >> $@
	@echo "\\begin{document}" >> $@
	@echo "\\includepdf[nup=2x2,pages=-]{"$*".pdf}" >> $@
	@echo "\\end{document}" >> $@

%-4.pdf: %-4.tex
	texi2dvi --pdf $<
	texclean $<

%-2.tex: %.pdf
	@Echo "\\documentclass[letterpaper]{article}" > $@
	@echo "\\usepackage{pdfpages}" >> $@
	@echo "\\begin{document}" >> $@
	@echo "\\includepdf[nup=1x2,pages=-]{"$*".pdf}" >> $@
	@echo "\\end{document}" >> $@

%-2.pdf: %-2.tex
	texi2dvi --pdf $<
	texclean $<


# A4 paper version
%-4a4.tex : %H.pdf
	@echo "\\documentclass[a4paper,landscape]{article}" > $@
	@echo "\\usepackage{pdfpages}" >> $@
	@echo "\\begin{document}" >> $@
	@echo "\\includepdf[nup=2x2,pages=-]{"$*"H.pdf}" >> $@
	@echo "\\end{document}" >> $@

%-4a4.pdf : %-4.tex
	texi2dvi --pdf $<
	texclean $<
