# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

#______________________________________________________________________________


DIRORG = $(shell dir *.org)
DIRPDF = $(DIRORG:.org=.pdf)

all: clean Makefile $(DIRPDF)

# compile latex to pdf using default latexmk recipe (lualatex + biber)
%.pdf: %.tex
	latexmk -f $<

# export org file to latex using emacs batch
%.tex: %.org
	emacs $< -f org-latex-export-to-latex --kill
	echo $@ >> provide


clean:
	rm -f *.pdf provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	echo "No tools required"

.PHONY: all install-tools clean
