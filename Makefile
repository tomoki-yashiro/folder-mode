# -*- mode: makefile-gmake -*-

ELISP	= folder-mode.el
TARGET	= ${ELISP:.el=.elc}

.PHONY: all build clean

all: build

build: ${TARGET}

clean:
	${RM} ${TARGET}

%.elc: %.el
	emacs --batch -f batch-byte-compile $<
