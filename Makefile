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

# $Id: Makefile 10566 2010-06-16 01:32:26Z garrigue $

# The main Makefile

include config/Makefile
include stdlib/StdlibModules

CAMLC=boot/llamarun boot/llamac -nostdlib -I boot
COMPFLAGS=-strict-sequence -warn-error A $(INCLUDES)
LINKFLAGS=

CAMLYACC=boot/llamayacc
YACCFLAGS=-v
CAMLLEX=boot/llamarun boot/llamalex
CAMLDEP=boot/llamarun tools/llamadep
DEPFLAGS=$(INCLUDES)
CAMLRUN=byterun/llamarun
SHELL=/bin/sh
MKDIR=mkdir -p

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I driver -I toplevel

UTILS=utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/consistbl.cmo

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/longident.cmo \
  parsing/syntaxerr.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo

TYPING=typing/unused_var.cmo typing/oprint.cmo typing/primitive.cmo \
  typing/base.cmo typing/predef.cmo \
  typing/typeutil.cmo typing/modenv.cmo \
  typing/env.cmo typing/mutable_type.cmo \
  typing/context.cmo typing/pseudoenv.cmo \
  typing/printtyp.cmo typing/resolve.cmo \
  typing/typify.cmo typing/permanent.cmo \
  typing/include.cmo typing/typemain.cmo

COMP=bytecomp/ident.cmo \
  bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/typeopt.cmo \
  bytecomp/pmc_pattern.cmo bytecomp/parmatch.cmo \
  bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translcore.cmo bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo

BYTECOMP=bytecomp/meta.cmo bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/opcodes.cmo bytecomp/emitcode.cmo \
  bytecomp/bytesections.cmo bytecomp/dll.cmo bytecomp/symtable.cmo \
  bytecomp/bytelink.cmo bytecomp/bytelibrarian.cmo

DRIVER=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  driver/main_args.cmo driver/main.cmo

TOPLEVEL=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  driver/main_args.cmo \
  toplevel/ledit_fstream.cmo toplevel/ledit_cursor.cmo \
  toplevel/ledit_char.cmo toplevel/ledit_string.cmo toplevel/ledit.cmo \
  toplevel/printer.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo toplevel/topmain.cmo

TOPLEVELLIB=toplevel/toplevellib.cma
TOPLEVELSTART=toplevel/topstart.cmo

COMPOBJS=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(DRIVER)

TOPOBJS=$(TOPLEVELLIB) $(TOPLEVELSTART)

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/base.cmo typing/predef.cmo \
  bytecomp/ident.cmo bytecomp/runtimedef.cmo bytecomp/bytesections.cmo \
  bytecomp/dll.cmo bytecomp/meta.cmo bytecomp/symtable.cmo toplevel/expunge.cmo

defaultentry: world

# Recompile the system using the bootstrap compiler
all: runtime llamac llamalex llamayacc llamatools library llama llamadoc

# Compile everything the first time
world:
	$(MAKE) coldstart
	$(MAKE) all

# Hard bootstrap how-to:
# (only necessary in some cases, for example if you remove some primitive)
#
# make coreboot     [old system -- you were in a stable state]
# <change the source>
# make core         [cross-compiler]
# make partialclean [if you get "inconsistent assumptions"]
# <debug your changes>
# make core         [cross-compiler]
# make coreboot     [new system -- now you are in a stable state]

# Core bootstrapping cycle
coreboot:
# Save the original bootstrap compiler
	$(MAKE) backup
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/llamarun and produces bytecode for
# byterun/llamarun
	$(MAKE) promote-cross
# Rebuild llamac and llamalex (run on byterun/llamarun)
	$(MAKE) partialclean
	$(MAKE) llamac llamalex llamatools
# Rebuild the library (using byterun/llamarun ./llamac)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) promote
# Rebuild the core system
	$(MAKE) partialclean
	$(MAKE) core
# Check if fixpoint reached
	$(MAKE) compare

# Bootstrap and rebuild the whole system.
# The compilation of llama will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.
bootstrap:
	$(MAKE) coreboot
	$(MAKE) all
	$(MAKE) compare

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

# Start up the system from the distribution compiler
coldstart:
	cd byterun; $(MAKE) all
	cp byterun/llamarun$(EXE) boot/llamarun$(EXE)
	cd yacc; $(MAKE) all
	cp yacc/llamayacc$(EXE) boot/llamayacc$(EXE)
	cd stdlib; $(MAKE) COMPILER=../boot/llamac all
	cd stdlib; cp $(LIBFILES) ../boot
	if test -f boot/libcamlrun.a; then :; else \
	  ln -s ../byterun/libcamlrun.a boot/libcamlrun.a; fi
	if test -d stdlib/caml; then :; else \
	  ln -s ../byterun stdlib/caml; fi

# Build the core system: the minimum needed to make depend and bootstrap
core: coldstart llamac llamalex llamayacc llamatools library

# Recompile the core system using the bootstrap compiler
coreall: llamac llamalex llamayacc llamatools library

# Save the current bootstrap compiler
MAXSAVED=boot/Saved/Saved.prev/Saved.prev/Saved.prev/Saved.prev/Saved.prev
backup:
	if test -d boot/Saved; then : ; else mkdir boot/Saved; fi
	if test -d $(MAXSAVED); then rm -r $(MAXSAVED); else : ; fi
	mv boot/Saved boot/Saved.prev
	mkdir boot/Saved
	mv boot/Saved.prev boot/Saved/Saved.prev
	cp boot/llamarun$(EXE) boot/Saved
	mv boot/llamac boot/llamalex boot/llamayacc$(EXE) boot/llamadep \
	   boot/Saved
	cd boot; cp $(LIBFILES) Saved

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
promote-cross:
	cp llamac boot/llamac
	cp lex/llamalex boot/llamalex
	cp yacc/llamayacc$(EXE) boot/llamayacc$(EXE)
	cp tools/llamadep boot/llamadep
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
promote: promote-cross
	cp byterun/llamarun$(EXE) boot/llamarun$(EXE)

# Restore the saved bootstrap compiler if a problem arises
restore:
	mv boot/Saved/* boot
	rmdir boot/Saved
	mv boot/Saved.prev boot/Saved

# Check if fixpoint reached
compare:
	@if cmp boot/llamac llamac && cmp boot/llamalex lex/llamalex \
	    && cmp boot/llamadep tools/llamadep; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

# Remove old bootstrap compilers
cleanboot:
	rm -rf boot/Saved/Saved.prev/*

# Installation
install:
	if test -d $(BINDIR); then : ; else $(MKDIR) $(BINDIR); fi
	if test -d $(LIBDIR); then : ; else $(MKDIR) $(LIBDIR); fi
	if test -d $(STUBLIBDIR); then : ; else $(MKDIR) $(STUBLIBDIR); fi
	cd byterun; $(MAKE) install
	cp llamac $(BINDIR)/llamac$(EXE)
	cp llama $(BINDIR)/llama$(EXE)
	cd stdlib; $(MAKE) install
	cp lex/llamalex $(BINDIR)/llamalex$(EXE)
	cp yacc/llamayacc$(EXE) $(BINDIR)/llamayacc$(EXE)
	cd tools; $(MAKE) install
	cd doctool; $(MAKE) install

clean:: partialclean

# The compiler

llamac: $(COMPOBJS)
	$(CAMLC) $(LINKFLAGS) -o llamac $(COMPOBJS)

partialclean::
	rm -f llamac llamacomp.sh

# The toplevel

llama: $(TOPOBJS) expunge
	$(CAMLC) $(LINKFLAGS) -linkall -o llama.tmp $(TOPOBJS)
	- $(CAMLRUN) ./expunge llama.tmp llama $(STDLIB_MODULES)
	rm -f llama.tmp

toplevel/toplevellib.cma: $(TOPLIB)
	$(CAMLC) -a -o $@ $(TOPLIB)

partialclean::
	rm -f llama

# The configuration file

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e "s|%%VERSION%%|`head -1 VERSION`|" \
	    -e 's|%%LIBDIR%%|$(LIBDIR)|' \
            -e 's|%%BYTERUN%%|$(BINDIR)/llamarun|' \
	    -e 's|%%CCOMPTYPE%%|cc|' \
	    -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)|' \
	    -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCCOMPOPTS)|' \
	    -e 's|%%PACKLD%%|$(PACKLD)|' \
	    -e 's|%%BYTECCLIBS%%|$(BYTECCLIBS)|' \
	    -e 's|%%NATIVECCLIBS%%|$(NATIVECCLIBS)|' \
	    -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
	    -e 's|%%CC_PROFILE%%|$(CC_PROFILE)|' \
	    -e 's|%%ARCH%%|$(ARCH)|' \
	    -e 's|%%MODEL%%|$(MODEL)|' \
	    -e 's|%%SYSTEM%%|$(SYSTEM)|' \
	    -e 's|%%EXT_OBJ%%|.o|' \
	    -e 's|%%EXT_ASM%%|.s|' \
	    -e 's|%%EXT_LIB%%|.a|' \
	    -e 's|%%EXT_DLL%%|.so|' \
	    -e 's|%%SYSTHREAD_SUPPORT%%|$(SYSTHREAD_SUPPORT)|' \
	    -e 's|%%ASM%%|$(ASM)|' \
	    -e 's|%%MKDLL%%|$(MKDLL)|' \
	    -e 's|%%MKEXE%%|$(MKEXE)|' \
	    -e 's|%%MKMAINDLL%%|$(MKMAINDLL)|' \
	    utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

partialclean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

# The parser

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly

partialclean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The auxiliary lexer for counting line numbers

parsing/linenum.ml: parsing/linenum.mll
	$(CAMLLEX) parsing/linenum.mll

partialclean::
	rm -f parsing/linenum.ml

beforedepend:: parsing/linenum.ml

# The numeric opcodes

bytecomp/opcodes.ml: byterun/instruct.h
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
	awk -f tools/make-opcodes > bytecomp/opcodes.ml

partialclean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# The predefined exceptions and primitives

byterun/primitives:
	cd byterun; $(MAKE) primitives

bytecomp/runtimedef.ml: byterun/primitives byterun/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' byterun/fail.h | \
	 sed -e '$$s/;$$//'; \
	 echo '|]'; \
	 echo 'let builtin_primitives = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' byterun/primitives; \
	 echo '|]') > bytecomp/runtimedef.ml

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

# The "expunge" utility

expunge: $(EXPUNGEOBJS)
	$(CAMLC) $(LINKFLAGS) -o expunge $(EXPUNGEOBJS)

partialclean::
	rm -f expunge

# The runtime system for the bytecode compiler

runtime:
	cd byterun; $(MAKE) all
	if test -f stdlib/libcamlrun.a; then :; else \
	  ln -s ../byterun/libcamlrun.a stdlib/libcamlrun.a; fi

clean::
	cd byterun; $(MAKE) clean
	rm -f stdlib/libcamlrun.a
	rm -f stdlib/caml

alldepend::
	cd byterun; $(MAKE) depend

# The library

library: llamac
	cd stdlib; $(MAKE) all

library-cross:
	cd stdlib; $(MAKE) RUNTIME=../byterun/llamarun all

partialclean::
	cd stdlib; $(MAKE) clean

alldepend::
	cd stdlib; $(MAKE) depend

# The lexer and parser generators

llamalex: llamayacc llamac
	cd lex; $(MAKE) all

partialclean::
	cd lex; $(MAKE) clean

alldepend::
	cd lex; $(MAKE) depend

llamayacc:
	cd yacc; $(MAKE) all

clean::
	cd yacc; $(MAKE) clean

# Tools

llamatools: llamac
	cd tools; $(MAKE) all

partialclean::
	cd tools; $(MAKE) clean

alldepend::
	cd tools; $(MAKE) depend

# The documentation generator

llamadoc: llamac llamayacc llamalex llamatools
	cd doctool && $(MAKE) all

partialclean::
	cd doctool && $(MAKE) clean

alldepend::
	cd doctool && $(MAKE) depend

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

partialclean::
	for d in utils parsing typing bytecomp driver toplevel tools; \
	  do rm -f $$d/*.cm[io] $$d/*.[so]; done

depend: beforedepend
	(for d in utils parsing typing bytecomp driver toplevel; \
	 do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
	 done) > .depend

alldepend:: depend

distclean:
	rm -f boot/{llamarun,llamayacc,camlheader,libcamlrun.a,*.cm*}
	rm -f config/{Makefile,s.h,m.h}

.PHONY: all backup bootstrap camlp4opt camlp4out checkstack clean
.PHONY: partialclean beforedepend alldepend cleanboot coldstart
.PHONY: compare core coreall
.PHONY: coreboot defaultentry depend distclean install installopt
.PHONY: library library-cross libraryopt ocamlbuild-mixed-boot
.PHONY: ocamlbuild.byte ocamlbuild.native ocamldebugger ocamldoc
.PHONY: ocamldoc.opt ocamllex ocamllex.opt ocamltools ocamltools.opt
.PHONY: ocamlyacc opt-core opt opt.opt otherlibraries
.PHONY: otherlibrariesopt package-macosx promote promote-cross
.PHONY: restore runtime runtimeopt makeruntimeopt world world.opt

include .depend
