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

# $Id: Makefile 9467 2009-12-09 09:52:47Z weis $

# The lexer generator
CAMLC=../boot/llamarun ../boot/llamac -strict-sequence -nostdlib -I ../boot -g
CAMLOPT=../boot/llamarun ../llamaopt -nostdlib -I ../stdlib
COMPFLAGS=-warn-error A
CAMLYACC=../boot/llamayacc
YACCFLAGS=-v
CAMLLEX=../boot/llamarun ../boot/llamalex
CAMLDEP=../boot/llamarun ../tools/llamadep


OBJS=cset.cmo syntax.cmo parser.cmo lexer.cmo table.cmo lexgen.cmo compact.cmo common.cmo output.cmo outputbis.cmo main.cmo

all: llamalex
allopt: llamalex.opt

llamalex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o llamalex $(OBJS)

llamalex.opt: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -o llamalex.opt $(OBJS:.cmo=.cmx)

clean::
	rm -f llamalex llamalex.opt
	rm -f *.cmo *.cmi *.cmx *.o *~

parser.ml parser.mli: parser.mly
	$(CAMLYACC) $(YACCFLAGS) parser.mly

clean::
	rm -f parser.ml parser.mli parser.output

beforedepend:: parser.ml parser.mli

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean::
	rm -f lexer.ml

beforedepend:: lexer.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
