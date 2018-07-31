# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

#______________________________________________________________________________
TEMPLATE		:= samartcl.latex
PANDOC_FLAGS	:= --template=$(TEMPLATE)

BIB				:= $(HOME)/Dropbox/bibliography
BIBLIO			:= $(BIB)/references.bib
STYLE			:= $(BIB)/styles/nature.csl

DIRMD = $(shell dir *.md)

DIRPDF = $(DIRMD:.md=.pdf)

all: clean Makefile $(DIRPDF)

%.pdf: %.md
	sed 's:figures/:../figures/:g' $< \
	| pandoc \
		$(PANDOC_FLAGS) \
		--filter pandoc-citeproc \
		--bibliography $(BIBLIO) \
		--csl $(STYLE) \
        -o $@
	echo $@ >> provide

clean:
	rm -f *.pdf provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	echo "No tools required"

.PHONY: all install-tools clean
