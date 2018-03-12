TEX=pdflatex
PLANTUML=deps/plantuml.jar

MASTER=src/master.tex
TEX_OPTIONS=options.tex
SRCS=$(shell find src -name '*.tex') \
     $(shell find src -name '*.bib') \
		 $(shell find src/listings) \
		 src/slides.tex

PANDOC_FLAGS= -t beamer \
		-f markdown+multiline_tables \
						 -s \
						 -H src/customizations.tex \
						 -Vurlcolor=linkcolor \
						 --highlight-style=haddock \
						 --slide-level=2 \
						 --filter pandoc-include-code \
						 -fmarkdown-implicit_figures \

DIAGRAM_SRCS=$(shell find src/diagrams -name '*.dot')
DIAGRAMS=$(DIAGRAM_SRCS:src/diagrams/%.dot=target/diagrams/%.png)

UML_SRCS=$(shell find src/uml -name '*.uml.txt')
UMLS=$(UML_SRCS:src/uml/%.uml.txt=target/uml/%.png)

SLIDES_DIR=target/slides
SLIDES=$(SLIDES_DIR)/slides.pdf

SLIDES_NO_NOTES_DIR=target/slides-no-notes
SLIDES_NO_NOTES=$(SLIDES_NO_NOTES_DIR)/slides-no-notes.pdf

.PHONY: all
all: slides programs diagrams

.PHONY: slides
slides: $(SLIDES) $(SLIDES_NO_NOTES)

target/slides.tex: src/slides.md src/customizations.tex src/notes.tex diagrams
	mkdir -p target
	pandoc $(PANDOC_FLAGS) \
		-H src/notes.tex \
		$< \
		-o $@

target/slides-no-notes.tex: src/slides.md src/customizations.tex diagrams
	mkdir -p target
	pandoc $(PANDOC_FLAGS) -V classoption=handout $< -o $@

$(SLIDES): target/slides.tex
	rm -rf $(SLIDES_DIR)
	mkdir -p $(SLIDES_DIR)
	cp target/slides.tex $(SLIDES_DIR)/slides.tex
	cd $(SLIDES_DIR) && \
		$(TEX) \
		-jobname slides \
		-halt-on-error \
		slides.tex

$(SLIDES_NO_NOTES): target/slides-no-notes.tex
	rm -rf $(SLIDES_NO_NOTES_DIR)
	mkdir -p $(SLIDES_NO_NOTES_DIR)
	cp target/slides-no-notes.tex $(SLIDES_NO_NOTES_DIR)/slides.tex
	cd $(SLIDES_NO_NOTES_DIR) && \
		$(TEX) \
		-jobname slides-no-notes \
		-halt-on-error \
		slides.tex

programs:
	cd src/listings/data-structures && stack build

target/diagrams/%.png: src/diagrams/%.dot
	mkdir -p target/diagrams
	dot -Tpng $< -o $@
	convert $@ -trim $@

target/uml/%.png: src/uml/%.uml.txt src/uml/styles.iuml $(PLANTUML)
	mkdir -p $(shell dirname $@)
	cat $< | java -jar $(PLANTUML) -tpng -pipe > $@
	convert $@ -trim $@


.PHONY: diagrams
diagrams: $(DIAGRAMS) $(UMLS)

present: $(SLIDES)
	pdfpc \
	--notes=right \
	--disable-auto-grouping \
	$(SLIDES)

clean:
	rm -rf target

$(PLANTUML):
	mkdir -p $(shell dirname $@)
	wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -O $@
