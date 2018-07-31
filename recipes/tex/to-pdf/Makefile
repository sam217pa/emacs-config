# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

#______________________________________________________________________________


DIRTEX = $(shell dir *.tex)

DIRPDF = $(DIRTEX:.tex=.pdf)

all: clean Makefile $(DIRPDF)

%.pdf: %.tex
	latexmk -f $<
	echo $@ >> provide

clean:
	latexmk -C *.tex
	rm -f *.pdf provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	echo "No tools required"

.PHONY: all install-tools clean
