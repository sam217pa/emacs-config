DIRRTF = $(shell dir *.rtf)

DIRTXT = $(DIRRTF:.rtf=.txt)

all: clean Makefile $(DIRTXT)

%.txt: %.rtf
	textutil -convert txt $<
	echo $@ >> provide

clean:
	rm -f *.txt provide

install-tools:
	echo "No tools required"

.PHONY: all install-tools clean
