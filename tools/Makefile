#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id: Makefile 8616 2007-11-22 22:14:43Z doligez $

include ../config/Makefile

CAMLRUN=../boot/llamarun
CAMLC=$(CAMLRUN) ../boot/llamac -strict-sequence -nostdlib -I ../boot
CAMLLEX=$(CAMLRUN) ../boot/llamalex
INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../driver
COMPFLAGS= -warn-error A $(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: llamadep
.PHONY: all

# The dependency generator

CAMLDEP_OBJ=depend.cmo depmain.cmo
CAMLDEP_IMPORTS=misc.cmo config.cmo clflags.cmo terminfo.cmo \
  linenum.cmo warnings.cmo location.cmo longident.cmo \
  syntaxerr.cmo parser.cmo lexer.cmo parse.cmo

llamadep: depend.cmi $(CAMLDEP_OBJ)
	$(CAMLC) $(LINKFLAGS) -o llamadep $(CAMLDEP_IMPORTS) $(CAMLDEP_OBJ)

# llamadep is precious: sometimes we are stuck in the middle of a
# bootstrap and we need to remake the dependencies
clean::
	if test -f llamadep; then mv -f llamadep llamadep.bak; else :; fi

install::
	cp llamadep $(BINDIR)/llamadep$(EXE)

# Common stuff

.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

clean::
	rm -f *.cmo *.cmi

depend:
	$(CAMLRUN) ./llamadep $(INCLUDES) *.mli *.ml > .depend

.PHONY: clean install depend

include .depend
