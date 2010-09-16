#(***********************************************************************)
#(*                            OCamldoc                                 *)
#(*                                                                     *)
#(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
#(*                                                                     *)
#(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
#(*  en Automatique.  All rights reserved.  This file is distributed    *)
#(*  under the terms of the Q Public License version 1.0.               *)
#(*                                                                     *)
#(***********************************************************************)

include ../config/Makefile

CAMLRUN=../byterun/llamarun
CAMLC=$(CAMLRUN) ../llamac -nostdlib -I ../stdlib
CAMLDEP=$(CAMLRUN) ../tools/llamadep
CAMLLEX=$(CAMLRUN) ../lex/llamalex
CAMLYACC=../yacc/llamayacc
INCLUDES=-I ../parsing -I ../utils -I ../typing -I ../bytecomp -I ../tools
COMPFLAGS=$(INCLUDES) -warn-error A -pp './remove_DEBUG'
LINKFLAGS=$(INCLUDES)

CAMLOBJS=\
  ../utils/tbl.cmo \
  ../utils/misc.cmo \
  ../utils/config.cmo \
  ../utils/clflags.cmo \
  ../utils/warnings.cmo \
  ../utils/ccomp.cmo \
  ../utils/consistbl.cmo \
  ../parsing/linenum.cmo\
  ../parsing/location.cmo\
  ../parsing/longident.cmo \
  ../parsing/syntaxerr.cmo \
  ../parsing/parser.cmo \
  ../parsing/lexer.cmo \
  ../parsing/parse.cmo \
  ../typing/primitive.cmo \
  ../typing/base.cmo \
  ../typing/predef.cmo \
  ../typing/basics.cmo \
  ../typing/persistent.cmo \
  ../typing/modenv.cmo \
  ../typing/env.cmo \
  ../typing/oprint.cmo \
  ../typing/printtyp.cmo \
  ../typing/mutable_base.cmo \
  ../typing/resolve.cmo \
  ../typing/typify.cmo \
  ../typing/immutify.cmo \
  ../tools/depend.cmo

OBJS=\
  odoc_config.cmo \
  odoc_global.cmo \
  odoc_messages.cmo \
  odoc_types.cmo \
  odoc_misc.cmo \
  odoc_text_parser.cmo \
  odoc_text_lexer.cmo \
  odoc_text.cmo \
  odoc_name.cmo \
  odoc_parameter.cmo \
  odoc_value.cmo \
  odoc_type.cmo \
  odoc_exception.cmo \
  odoc_module.cmo \
  odoc_print.cmo  \
  odoc_str.cmo \
  odoc_args.cmo \
  odoc_comments_global.cmo \
  odoc_parser.cmo \
  odoc_lexer.cmo \
  odoc_see_lexer.cmo \
  odoc_env.cmo \
  odoc_merge.cmo \
  odoc_comments.cmo \
  odoc_sig.cmo \
  odoc_control.cmo \
  odoc_inherit.cmo \
  odoc_search.cmo \
  odoc_cross.cmo \
  odoc_dep.cmo \
  odoc_analyse.cmo \
  odoc_info.cmo \
  odoc_llamahtml.cmo \
  odoc_html.cmo \
  odoc.cmo

# main stuff

all: llamadoc
.PHONY: all

llamadoc: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o $@ $(CAMLOBJS) $(OBJS)

install:
	cp llamadoc $(BINDIR)/llamadoc$(EXE)
.PHONY: install

# lexers and parsers

.PHONY: partialclean beforedepend

odoc_text_parser.ml: odoc_text_parser.mly
odoc_text_parser.mli: odoc_text_parser.mly
partialclean::
	rm -f odoc_text_parser.ml odoc_text_parser.mli odoc_text_parser.output
beforedepend:: odoc_text_parser.ml odoc_text_parser.mli

odoc_parser.ml:	odoc_parser.mly
odoc_parser.mli: odoc_parser.mly
partialclean::
	rm -f odoc_parser.ml odoc_parser.mli odoc_parser.output
beforedepend:: odoc_parser.ml odoc_parser.mli

odoc_text_lexer.ml: odoc_text_lexer.mll
partialclean::
	rm -f odoc_text_lexer.ml
beforedepend:: odoc_text_lexer.ml

odoc_lexer.ml: odoc_lexer.mll
partialclean::
	rm -f odoc_lexer.ml
beforedepend:: odoc_lexer.ml

odoc_llamahtml.ml: odoc_llamahtml.mll
partialclean::
	rm -f odoc_llamahtml.ml
beforedepend:: odoc_llamahtml.ml

odoc_see_lexer.ml: odoc_see_lexer.mll
partialclean::
	rm -f odoc_see_lexer.ml
beforedepend:: odoc_see_lexer.ml

# generic stuff

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi
.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<
.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<
.mll.ml:
	$(CAMLLEX) $<
.mly.ml:
	$(CAMLYACC) -v $<
.mly.mli:
	$(CAMLYACC) -v $<

# final stuff

clean: partialclean
	rm -f llamadoc *.cmo *.cmi
.PHONY: clean

depend: beforedepend
	$(CAMLDEP) $(INCLUDES) *.ml *.mli > .depend
.PHONY: depend

include .depend
