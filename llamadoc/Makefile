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

# $Id: Makefile 10584 2010-06-16 11:38:22Z guesdon $

include ../config/Makefile

# Various commands and dir
##########################
CAMLRUN=../boot/llamarun
LLAMAC   = ../byterun/llamarun ../llamac
LLAMAOPT = ../llamacompopt.sh
LLAMADEP = $(CAMLRUN) ../tools/llamadep
LLAMALEX = $(CAMLRUN) ../boot/llamalex
LLAMAYACC= ../boot/llamayacc
LLAMALIB = $(LIBDIR)
LLAMABIN = $(BINDIR)

LLAMAPP=-pp './remove_DEBUG'

# For installation
##############
MKDIR=mkdir -p
CP=cp -f
LLAMADOC=llamadoc
LLAMADOC_RUN=./llamadoc.opt #sh ./runllamadoc $(SUPPORTS_SHARED_LIBRARIES)
LLAMADOC_OPT=$(LLAMADOC).opt
LLAMADOC_LIBCMA=odoc_info.cma
LLAMADOC_LIBCMI=odoc_info.cmi
LLAMADOC_LIBCMXA=odoc_info.cmxa
LLAMADOC_LIBA=odoc_info.a
INSTALL_LIBDIR=$(LLAMALIB)/llamadoc
INSTALL_CUSTOMDIR=$(INSTALL_LIBDIR)/custom
INSTALL_BINDIR=$(LLAMABIN)
INSTALL_MANODIR=$(MANDIR)/man3

INSTALL_MLIS=odoc_info.mli
INSTALL_CMIS=$(INSTALL_MLIS:.mli=.cmi)

ODOC_TEST=odoc_test.cmo


# Compilation
#############
LLAMASRCDIR=..
INCLUDES_DEP=-I $(LLAMASRCDIR)/parsing \
	-I $(LLAMASRCDIR)/utils \
	-I $(LLAMASRCDIR)/typing \
	-I $(LLAMASRCDIR)/driver \
	-I $(LLAMASRCDIR)/bytecomp \
	-I $(LLAMASRCDIR)/tools \
	-I $(LLAMASRCDIR)/toplevel/

INCLUDES_NODEP=	-I $(LLAMASRCDIR)/stdlib

INCLUDES=$(INCLUDES_DEP) $(INCLUDES_NODEP)

COMPFLAGS=$(INCLUDES) -warn-error A
LINKFLAGS=$(INCLUDES) -nostdlib

CMOFILES= odoc_config.cmo \
	odoc_global.cmo\
	odoc_messages.cmo\
	odoc_types.cmo\
	odoc_misc.cmo\
	odoc_text_parser.cmo\
	odoc_text_lexer.cmo\
	odoc_text.cmo\
	odoc_name.cmo\
	odoc_parameter.cmo\
	odoc_value.cmo\
	odoc_type.cmo\
	odoc_exception.cmo\
	odoc_module.cmo\
	odoc_print.cmo \
	odoc_str.cmo\
	odoc_args.cmo\
	odoc_comments_global.cmo\
	odoc_parser.cmo\
	odoc_lexer.cmo\
	odoc_see_lexer.cmo\
	odoc_env.cmo\
	odoc_merge.cmo\
	odoc_comments.cmo\
	odoc_sig.cmo\
	odoc_control.cmo\
	odoc_inherit.cmo\
	odoc_search.cmo\
	odoc_cross.cmo\
	odoc_dep.cmo\
	odoc_analyse.cmo\
	odoc_info.cmo


CMXFILES= $(CMOFILES:.cmo=.cmx)
CMIFILES= $(CMOFILES:.cmo=.cmi)

EXECMOFILES=$(CMOFILES) \
	odoc_llamahtml.cmo \
	odoc_html.cmo \
	odoc.cmo

EXECMXFILES= $(EXECMOFILES:.cmo=.cmx)
EXECMIFILES= $(EXECMOFILES:.cmo=.cmi)

LIBCMOFILES=$(CMOFILES)
LIBCMXFILES= $(LIBCMOFILES:.cmo=.cmx)
LIBCMIFILES= $(LIBCMOFILES:.cmo=.cmi)

# Les cmo et cmx de la distrib LLAMA
TYPING=$(LLAMASRCDIR)/typing/unused_var.cmo \
 $(LLAMASRCDIR)/typing/primitive.cmo \
 $(LLAMASRCDIR)/typing/types.cmo \
 $(LLAMASRCDIR)/typing/predef.cmo \
 $(LLAMASRCDIR)/typing/modenv.cmo \
 $(LLAMASRCDIR)/typing/env.cmo \
 $(LLAMASRCDIR)/typing/btype.cmo \
 $(LLAMASRCDIR)/typing/subst.cmo \
 $(LLAMASRCDIR)/typing/oprint.cmo \
 $(LLAMASRCDIR)/typing/context.cmo \
 $(LLAMASRCDIR)/typing/type_context.cmo \
 $(LLAMASRCDIR)/typing/printtyp.cmo \
 $(LLAMASRCDIR)/typing/typedtree_aux.cmo $(LLAMASRCDIR)/typing/ctype.cmo \
 $(LLAMASRCDIR)/typing/includecore.cmo $(LLAMASRCDIR)/typing/includemod.cmo \
 $(LLAMASRCDIR)/typing/resolve.cmo \
 $(LLAMASRCDIR)/typing/typecore.cmo \
 $(LLAMASRCDIR)/typing/typedecl.cmo $(LLAMASRCDIR)/typing/typemod.cmo


LLAMACMOFILES=$(LLAMASRCDIR)/parsing/printast.cmo \
	$(LLAMASRCDIR)/utils/tbl.cmo \
	$(LLAMASRCDIR)/utils/misc.cmo \
	$(LLAMASRCDIR)/utils/config.cmo \
	$(LLAMASRCDIR)/utils/clflags.cmo \
	$(LLAMASRCDIR)/utils/warnings.cmo \
	$(LLAMASRCDIR)/utils/ccomp.cmo \
	$(LLAMASRCDIR)/utils/consistbl.cmo \
	$(LLAMASRCDIR)/parsing/linenum.cmo\
	$(LLAMASRCDIR)/parsing/location.cmo\
	$(LLAMASRCDIR)/parsing/longident.cmo \
	$(LLAMASRCDIR)/parsing/syntaxerr.cmo \
	$(LLAMASRCDIR)/parsing/parser.cmo \
	$(LLAMASRCDIR)/parsing/lexer.cmo \
	$(LLAMASRCDIR)/parsing/parse.cmo \
        $(TYPING) \
	$(LLAMASRCDIR)/bytecomp/ident.cmo \
	$(LLAMASRCDIR)/bytecomp/lambda.cmo \
	$(LLAMASRCDIR)/bytecomp/typeopt.cmo \
	$(LLAMASRCDIR)/bytecomp/printlambda.cmo \
	$(LLAMASRCDIR)/bytecomp/pmc_pattern.cmo \
	$(LLAMASRCDIR)/bytecomp/parmatch.cmo \
	$(LLAMASRCDIR)/bytecomp/switch.cmo \
	$(LLAMASRCDIR)/bytecomp/matching.cmo \
	$(LLAMASRCDIR)/bytecomp/translcore.cmo \
	$(LLAMASRCDIR)/tools/depend.cmo

LLAMACMXFILES=$(LLAMACMOFILES:.cmo=.cmx)

STDLIB_MLIS=../stdlib/*.mli

all: exe lib

exe: $(LLAMADOC)
lib: $(LLAMADOC_LIBCMA) $(LLAMADOC_LIBCMI) # $(ODOC_TEST)

opt.opt: exeopt libopt
	$(MAKE) manpages
exeopt: $(LLAMADOC_OPT)
libopt: $(LLAMADOC_LIBCMXA) $(LLAMADOC_LIBCMI)
debug:
	make LLAMAPP=""

$(LLAMADOC): $(EXECMOFILES)
	$(LLAMAC) -o $@ -linkall $(LINKFLAGS) $(LLAMACMOFILES) $(EXECMOFILES)
$(LLAMADOC_OPT): $(EXECMXFILES)
	$(LLAMAOPT) -o $@ $(LINKFLAGS) $(LLAMACMXFILES) $(EXECMXFILES)

$(LLAMADOC_LIBCMA): $(LIBCMOFILES)
	$(LLAMAC) -a -o $@ $(LINKFLAGS) $(LLAMACMOFILES) $(LIBCMOFILES)
$(LLAMADOC_LIBCMXA): $(LIBCMXFILES)
	$(LLAMAOPT) -a -o $@ $(LINKFLAGS) $(LLAMACMXFILES) $(LIBCMXFILES)

manpages: stdlib_man/Pervasives.3o

dot: $(EXECMOFILES)
	$(LLAMADOC_RUN) -dot -dot-reduce -o llamadoc.dot $(INCLUDES) \
	odoc*.ml

# Parsers and lexers dependencies :
###################################
odoc_text_parser.ml: odoc_text_parser.mly
odoc_text_parser.mli: odoc_text_parser.mly

odoc_parser.ml:	odoc_parser.mly
odoc_parser.mli:odoc_parser.mly

odoc_text_lexer.ml: odoc_text_lexer.mll

odoc_lexer.ml:odoc_lexer.mll

odoc_llamahtml.ml: odoc_llamahtml.mll

odoc_see_lexer.ml: odoc_see_lexer.mll


# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(LLAMAC) $(LLAMAPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(LLAMAC) $(LLAMAPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(LLAMAOPT) $(LLAMAPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(LLAMALEX) $<

.mly.ml:
	$(LLAMAYACC) -v $<

.mly.mli:
	$(LLAMAYACC) -v $<

# Installation targets
######################
install: dummy
	if test -d $(INSTALL_BINDIR); then : ; else $(MKDIR) $(INSTALL_BINDIR); fi
#	if test -d $(INSTALL_LIBDIR); then : ; else $(MKDIR) $(INSTALL_LIBDIR); fi
#	if test -d $(INSTALL_CUSTOMDIR); then : ; else $(MKDIR) $(INSTALL_CUSTOMDIR); fi
	$(CP) $(LLAMADOC) $(INSTALL_BINDIR)/$(LLAMADOC)$(EXE)
#	$(CP) llamadoc.hva *.cmi $(LLAMADOC_LIBCMA) $(INSTALL_LIBDIR)
#	$(CP) $(INSTALL_MLIS) $(INSTALL_CMIS) $(INSTALL_LIBDIR)
#	if test -d $(INSTALL_MANODIR); then : ; else $(MKDIR) $(INSTALL_MANODIR); fi
#	if test -d stdlib_man; then $(CP) stdlib_man/* $(INSTALL_MANODIR); else : ; fi

installopt:
	if test -f $(LLAMADOC_OPT) ; then $(MAKE) installopt_really ; fi

installopt_really:
	if test -d $(INSTALL_BINDIR); then : ; else $(MKDIR) $(INSTALL_BINDIR); fi
	if test -d $(INSTALL_LIBDIR); then : ; else $(MKDIR) $(INSTALL_LIBDIR); fi
	$(CP) $(LLAMADOC_OPT) $(INSTALL_BINDIR)/$(LLAMADOC_OPT)$(EXE)
	$(CP) llamadoc.hva $(LLAMADOC_LIBA) $(LLAMADOC_LIBCMXA) $(INSTALL_LIBDIR)
	$(CP) $(INSTALL_MLIS) $(INSTALL_CMIS) $(INSTALL_LIBDIR)

# Testing :
###########
test: dummy
	$(MKDIR) $@
	$(LLAMADOC_RUN) -html -colorize-code -sort -d $@ $(INCLUDES) -dump $@/llamadoc.odoc odoc*.ml odoc*.mli -v

test_stdlib: dummy
	$(MKDIR) $@
	$(LLAMADOC_RUN) -html -colorize-code -sort -d $@ $(INCLUDES) -dump $@/stdlib.odoc -keep-code \
	../stdlib/pervasives.ml ../stdlib/*.mli \
	../otherlibs/unix/unix.mli \
	../otherlibs/str/str.mli

test_framed: dummy
	$(MKDIR) $@
	$(LLAMADOC_RUN) -g odoc_fhtml.cmo -sort -colorize-code -d $@ $(INCLUDES) odoc*.ml odoc*.mli

test_latex: dummy
	$(MKDIR) $@
	$(LLAMADOC_RUN) -latex -sort -o $@/test.tex -d $@ $(INCLUDES) odoc*.ml odoc*.mli test2.txt ../stdlib/*.mli ../otherlibs/unix/unix.mli

test_latex_simple: dummy
	$(MKDIR) $@
	$(LLAMADOC_RUN) -latex -sort -o $@/test.tex -d $@ $(INCLUDES) \
	-latextitle 6,subsection -latextitle 7,subsubection \
	../stdlib/hashtbl.mli \
	../stdlib/arg.mli \
	../otherlibs/unix/unix.mli \
	../stdlib/map.mli

test_man: dummy
	$(MKDIR) $@
	$(LLAMADOC_RUN) -man -sort -d $@ $(INCLUDES) odoc*.ml odoc*.mli

test_texi: dummy
	$(MKDIR) $@
	$(LLAMADOC_RUN) -texi -sort -d $@ $(INCLUDES) odoc*.ml odoc*.mli

stdlib_man/Pervasives.3o: $(STDLIB_MLIS)
	$(MKDIR) stdlib_man
	$(LLAMADOC_RUN) -man -d stdlib_man $(INCLUDES) \
	-t "Llama library" -man-mini \
	$(STDLIB_MLIS)

autotest_stdlib: dummy
	$(MKDIR) $@
	$(LLAMADOC_RUN) -g autotest/odoc_test.cmo\
	$(INCLUDES) -keep-code \
	../stdlib/pervasives.ml ../stdlib/*.mli \
	../otherlibs/unix/unix.mli \
	../otherlibs/str/str.mli

# backup, clean and depend :
############################

clean:: dummy
	@rm -f *~ \#*\#
	@rm -f $(LLAMADOC) $(LLAMADOC_OPT) *.cma *.cmxa *.cmo *.cmi *.cmx *.a *.o
	@rm -f odoc_parser.output odoc_text_parser.output
	@rm -f odoc_lexer.ml odoc_text_lexer.ml odoc_see_lexer.ml odoc_llamahtml.ml
	@rm -f odoc_parser.ml odoc_parser.mli odoc_text_parser.ml odoc_text_parser.mli
	@rm -rf stdlib_man

depend::
	$(LLAMAYACC) odoc_text_parser.mly
	$(LLAMAYACC) odoc_parser.mly
	$(LLAMALEX) odoc_text_lexer.mll
	$(LLAMALEX) odoc_lexer.mll
	$(LLAMALEX) odoc_llamahtml.mll
	$(LLAMALEX) odoc_see_lexer.mll
	$(LLAMADEP) $(INCLUDES_DEP) *.mll *.mly *.ml *.mli > .depend

dummy:

include .depend
