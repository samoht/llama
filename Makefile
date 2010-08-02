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
CAMLOPT=boot/llamarun ./llamaopt -nostdlib -I stdlib -I otherlibs/dynlink
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

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I driver \
	 -I toplevel

UTILS=utils/config.cmo utils/clflags.cmo utils/misc.cmo utils/tbl.cmo utils/warnings.cmo utils/consistbl.cmo utils/ccomp.cmo utils/terminfo.cmo

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/syntaxerr.cmo \
 parsing/longident.cmo parsing/parser.cmo parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo

TYPING=typing/unused_var.cmo typing/primitive.cmo \
 typing/types.cmo \
 typing/predef.cmo \
 typing/module.cmo \
 typing/env.cmo \
 typing/get.cmo \
 typing/btype.cmo \
 typing/subst.cmo \
 typing/oprint.cmo \
 typing/printtyp.cmo typing/pr_decl.cmo \
 typing/typedtree_aux.cmo typing/ctype.cmo \
 typing/error.cmo \
 typing/includecore.cmo typing/includemod.cmo \
 typing/resolve.cmo \
 typing/typecore.cmo \
 typing/typedecl.cmo typing/typemod.cmo

COMP=bytecomp/ident.cmo bytecomp/identSet.cmo \
  bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/typeopt.cmo \
  bytecomp/pmc_pattern.cmo bytecomp/parmatch.cmo \
  bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translcore.cmo \
  bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo \

BYTECOMP=bytecomp/meta.cmo bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/opcodes.cmo bytecomp/emitcode.cmo \
  bytecomp/bytesections.cmo bytecomp/dll.cmo bytecomp/symtable.cmo \
  bytecomp/bytelink.cmo bytecomp/bytelibrarian.cmo

ASMCOMP=asmcomp/arch.cmo asmcomp/debuginfo.cmo \
  asmcomp/cmm.cmo asmcomp/printcmm.cmo \
  asmcomp/reg.cmo asmcomp/mach.cmo asmcomp/proc.cmo \
  asmcomp/clambda.cmo asmcomp/compilenv.cmo \
  asmcomp/closure.cmo asmcomp/cmmgen.cmo \
  asmcomp/printmach.cmo asmcomp/selectgen.cmo asmcomp/selection.cmo \
  asmcomp/comballoc.cmo asmcomp/liveness.cmo \
  asmcomp/spill.cmo asmcomp/split.cmo \
  asmcomp/interf.cmo asmcomp/coloring.cmo \
  asmcomp/reloadgen.cmo asmcomp/reload.cmo \
  asmcomp/printlinear.cmo asmcomp/linearize.cmo \
  asmcomp/schedgen.cmo asmcomp/scheduling.cmo \
  asmcomp/emitaux.cmo asmcomp/emit.cmo asmcomp/asmgen.cmo \
  asmcomp/asmlink.cmo asmcomp/asmlibrarian.cmo asmcomp/asmpackager.cmo

DRIVER=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  driver/main_args.cmo driver/main.cmo

OPTDRIVER= driver/pparse.cmo driver/opterrors.cmo driver/optcompile.cmo \
  driver/main_args.cmo driver/optmain.cmo

TOPLEVEL=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  driver/main_args.cmo toplevel/printer.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo toplevel/topmain.cmo

TOPLEVELLIB=toplevel/toplevellib.cma
TOPLEVELSTART=toplevel/topstart.cmo

COMPOBJS=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(DRIVER)

TOPLIB=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(TOPLEVEL)

TOPOBJS=$(TOPLEVELLIB) $(TOPLEVELSTART)

NATTOPOBJS=$(OPTUTILS) $(PARSING) $(TYPING) $(COMP) $(ASMCOMP) \
  driver/pparse.cmo driver/opterrors.cmo driver/optcompile.cmo \
  driver/main_args.cmo \
  toplevel/genprintval.cmo toplevel/opttoploop.cmo toplevel/opttopdirs.cmo \
  toplevel/opttopmain.cmo toplevel/opttopstart.cmo

OPTOBJS=$(OPTUTILS) $(PARSING) $(TYPING) $(COMP) $(ASMCOMP) $(OPTDRIVER)

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/ident.cmo typing/path.cmo typing/types.cmo typing/btype.cmo \
  typing/predef.cmo bytecomp/runtimedef.cmo bytecomp/bytesections.cmo \
  bytecomp/dll.cmo bytecomp/meta.cmo bytecomp/symtable.cmo toplevel/expunge.cmo

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

# For users who don't read the INSTALL file
defaultentry:
	@echo "Please refer to the installation instructions in file INSTALL."
	@echo "If you've just unpacked the distribution, something like"
	@echo "	./configure"
	@echo "	make world"
	@echo "	make opt"
	@echo "	make install"
	@echo "should work.  But see the file INSTALL for more details."

# Recompile the system using the bootstrap compiler
all: runtime llamac llamalex llamayacc llamatools library llama \
  otherlibraries llamadoc # llamabuild.byte camlp4out $(DEBUGGER)

# Compile everything the first time
world:
	$(MAKE) coldstart
	$(MAKE) all

# Compile also native code compiler and libraries, fast
world.opt:
	$(MAKE) coldstart
	$(MAKE) opt.opt

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

# Compile the native-code compiler
opt-core:
	$(MAKE) runtimeopt
	$(MAKE) llamaopt
	$(MAKE) libraryopt

opt:
	$(MAKE) runtimeopt
	$(MAKE) llamaopt
	$(MAKE) libraryopt
	$(MAKE) otherlibrariesopt
	$(MAKE) llamabuildlib.native

# Native-code versions of the tools
opt.opt: checkstack runtime core llama opt-core llamac.opt otherlibraries \
	 llamabuild.byte camlp4out $(DEBUGGER) llamadoc llamaopt.opt \
	 otherlibrariesopt \
	 llamalex.opt llamatoolsopt.opt llamabuild.native camlp4opt llamadoc.opt

base.opt: checkstack runtime core llama opt-core llamac.opt otherlibraries \
	 llamabuild.byte camlp4out $(DEBUGGER) llamadoc llamaopt.opt \
	 otherlibrariesopt

# Installation
install:
	if test -d $(BINDIR); then : ; else $(MKDIR) $(BINDIR); fi
	if test -d $(LIBDIR); then : ; else $(MKDIR) $(LIBDIR); fi
	if test -d $(STUBLIBDIR); then : ; else $(MKDIR) $(STUBLIBDIR); fi
	if test -d $(MANDIR)/man$(MANEXT); then : ; \
	  else $(MKDIR) $(MANDIR)/man$(MANEXT); fi
	cd $(LIBDIR); rm -f dllbigarray.so dlllabltk.so dllnums.so \
	  dllthreads.so dllunix.so dllgraphics.so dllmldbm.so dllstr.so \
	  dlltkanim.so
	cd byterun; $(MAKE) install
	cp llamac $(BINDIR)/llamac$(EXE)
	cp llama $(BINDIR)/llama$(EXE)
	cd stdlib; $(MAKE) install
	cp lex/llamalex $(BINDIR)/llamalex$(EXE)
	cp yacc/llamayacc$(EXE) $(BINDIR)/llamayacc$(EXE)
	cp toplevel/toplevellib.cma $(LIBDIR)/toplevellib.cma
#	cp expunge $(LIBDIR)/expunge$(EXE)
	cp typing/outcometree.cmi typing/outcometree.mli $(LIBDIR)
	cp toplevel/topstart.cmo $(LIBDIR)
	cp toplevel/toploop.cmi toplevel/topdirs.cmi toplevel/topmain.cmi \
	   $(LIBDIR)
#	cd tools; $(MAKE) install
#	-cd man; $(MAKE) install
#	for i in $(OTHERLIBRARIES); do \
#	  (cd otherlibs/$$i; $(MAKE) install) || exit $$?; \
#	done
#	for i in num graph unix; do \
#	  (cd otherlibs/$$i; $(MAKE) install) || exit $$?; \
#	done
	cd llamadoc; $(MAKE) install
	if test -f llamaopt; then $(MAKE) installopt; else :; fi
	if test -f debugger/llamadebug; then (cd debugger; $(MAKE) install); \
	   else :; fi
	cp config/Makefile $(LIBDIR)/Makefile.config
#	BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) PREFIX=$(PREFIX) \
	  ./build/partial-install.sh

# Installation of the native-code compiler
installopt:
	cd asmrun; $(MAKE) install
	cp llamaopt $(BINDIR)/llamaopt$(EXE)
	cd stdlib; $(MAKE) installopt
	cd llamadoc; $(MAKE) installopt
	for i in $(OTHERLIBRARIES); \
	  do (cd otherlibs/$$i; $(MAKE) installopt) || exit $$?; done
	if test -f llamac.opt; \
	  then cp llamac.opt $(BINDIR)/llamac.opt$(EXE); else :; fi
	if test -f llamaopt.opt; \
	  then cp llamaopt.opt $(BINDIR)/llamaopt.opt$(EXE); else :; fi
	if test -f lex/llamalex.opt; \
	  then cp lex/llamalex.opt $(BINDIR)/llamalex.opt$(EXE); else :; fi

clean:: partialclean

# The compiler

llamac: $(COMPOBJS)
	$(CAMLC) $(LINKFLAGS) -o llamac $(COMPOBJS)
	@sed -e 's|@compiler@|$$topdir/boot/llamarun $$topdir/llamac|' \
	  driver/llamacomp.sh.in > llamacomp.sh
	@chmod +x llamacomp.sh

partialclean::
	rm -f llamac llamacomp.sh

# The native-code compiler

llamaopt: $(OPTOBJS)
	$(CAMLC) $(LINKFLAGS) -o llamaopt $(OPTOBJS)
	@sed -e 's|@compiler@|$$topdir/boot/llamarun $$topdir/llamaopt|' \
	  driver/llamacomp.sh.in > llamacompopt.sh
	@chmod +x llamacompopt.sh

partialclean::
	rm -f llamaopt llamacompopt.sh

# The toplevel

llama: $(TOPOBJS) # expunge
	$(CAMLC) $(LINKFLAGS) -linkall -o llama $(TOPOBJS)
#	- $(CAMLRUN) ./expunge llama.tmp llama $(PERVASIVES)
#	rm -f llama.tmp

toplevel/toplevellib.cma: $(TOPLIB)
	$(CAMLC) -a -o $@ $(TOPLIB)

partialclean::
	rm -f llama toplevel/toplevellib.cma

# The native toplevel

llamanat: llamaopt otherlibs/dynlink/dynlink.cmxa $(NATTOPOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) otherlibs/dynlink/dynlink.cmxa -o llamanat \
	           $(NATTOPOBJS:.cmo=.cmx) -linkall

toplevel/opttoploop.cmx: otherlibs/dynlink/dynlink.cmxa

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/natdynlink.ml
	cd otherlibs/dynlink && make allopt

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

# The bytecode compiler compiled with the native-code compiler

llamac.opt: $(COMPOBJS:.cmo=.cmx)
	cd asmrun; $(MAKE) meta.o dynlink.o
	$(CAMLOPT) $(LINKFLAGS) -ccopt "$(BYTECCLINKOPTS)" -o llamac.opt \
	  $(COMPOBJS:.cmo=.cmx) \
	  asmrun/meta.o asmrun/dynlink.o -cclib "$(BYTECCLIBS)"
	@sed -e 's|@compiler@|$$topdir/llamac.opt|' \
	  driver/llamacomp.sh.in > llamacomp.sh
	@chmod +x llamacomp.sh

partialclean::
	rm -f llamac.opt

# The native-code compiler compiled with itself

llamaopt.opt: $(OPTOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o llamaopt.opt $(OPTOBJS:.cmo=.cmx)
	@sed -e 's|@compiler@|$$topdir/llamaopt.opt|' \
	  driver/llamacomp.sh.in > llamacompopt.sh
	@chmod +x llamacompopt.sh

partialclean::
	rm -f llamaopt.opt

$(OPTOBJS:.cmo=.cmx): llamaopt

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

# Choose the right machine-dependent files

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	ln -s $(ARCH)/arch.ml asmcomp/arch.ml

partialclean::
	rm -f asmcomp/arch.ml

#beforedepend:: asmcomp/arch.ml

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	ln -s $(ARCH)/proc.ml asmcomp/proc.ml

partialclean::
	rm -f asmcomp/proc.ml

#beforedepend:: asmcomp/proc.ml

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	ln -s $(ARCH)/selection.ml asmcomp/selection.ml

partialclean::
	rm -f asmcomp/selection.ml

#beforedepend:: asmcomp/selection.ml

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	ln -s $(ARCH)/reload.ml asmcomp/reload.ml

partialclean::
	rm -f asmcomp/reload.ml

#beforedepend:: asmcomp/reload.ml

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	ln -s $(ARCH)/scheduling.ml asmcomp/scheduling.ml

partialclean::
	rm -f asmcomp/scheduling.ml

#beforedepend:: asmcomp/scheduling.ml

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp tools/cvt_emit
	$(CAMLRUN) tools/cvt_emit < asmcomp/$(ARCH)/emit.mlp > asmcomp/emit.ml \
	|| { rm -f asmcomp/emit.ml; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml

#beforedepend:: asmcomp/emit.ml

tools/cvt_emit: tools/cvt_emit.mll
	cd tools; \
	$(MAKE) CAMLC="../$(CAMLRUN) ../boot/llamac -I ../stdlib" cvt_emit

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

# The runtime system for the native-code compiler

runtimeopt: makeruntimeopt
	cp asmrun/libasmrun.a stdlib/libasmrun.a

makeruntimeopt:
	cd asmrun; $(MAKE) all

#clean::
#	cd asmrun; $(MAKE) clean
#	rm -f stdlib/libasmrun.a

alldepend::
	cd asmrun; $(MAKE) depend

# The library

library: llamac
	cd stdlib; $(MAKE) all

library-cross:
	cd stdlib; $(MAKE) RUNTIME=../byterun/llamarun all

libraryopt:
	cd stdlib; $(MAKE) allopt

partialclean::
	cd stdlib; $(MAKE) clean

alldepend::
	cd stdlib; $(MAKE) depend

# The lexer and parser generators

llamalex: llamayacc llamac
	cd lex; $(MAKE) all

llamalex.opt: llamaopt
	cd lex; $(MAKE) allopt

partialclean::
	cd lex; $(MAKE) clean

alldepend::
	cd lex; $(MAKE) depend

llamayacc:
	cd yacc; $(MAKE) all

clean::
	cd yacc; $(MAKE) clean

# Tools

llamatools: llamac llamayacc llamalex # asmcomp/cmx_format.cmi
	cd tools; $(MAKE) all

llamatoolsopt.opt: llamac.opt llamayacc llamalex asmcomp/cmx_format.cmi
	cd tools; $(MAKE) opt.opt

partialclean::
	cd tools; $(MAKE) clean

alldepend::
	cd tools; $(MAKE) depend

# Llamadoc

llamadoc: llamac llamayacc llamalex otherlibraries
	cd llamadoc && $(MAKE) all

llamadoc.opt: llamac.opt llamayacc llamalex
	cd llamadoc && $(MAKE) opt.opt

partialclean::
	cd llamadoc && $(MAKE) clean

alldepend::
	cd llamadoc && $(MAKE) depend

# The extra libraries

otherlibraries: llamatools
#	for i in num graph str; do \
#	  (cd otherlibs/$$i; $(MAKE) RUNTIME=$(RUNTIME) all) || exit $$?; \
#	done
	for i in num graph unix; do \
	  (cd otherlibs/$$i; $(MAKE) RUNTIME=$(RUNTIME) all) || exit $$?; \
	done

otherlibrariesopt:
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) allopt) || exit $$?; \
	done

#partialclean::
#	for i in $(OTHERLIBRARIES); do \
#	  (cd otherlibs/$$i; $(MAKE) partialclean); \
#	done

#clean::
#	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) clean); done

alldepend::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) depend); done

# The replay debugger

llamadebugger: llamac llamayacc llamalex otherlibraries
	cd debugger; $(MAKE) all

#partialclean::
#	cd debugger; $(MAKE) clean

alldepend::
	cd debugger; $(MAKE) depend

# Camlp4

camlp4out: llamac otherlibraries llamabuild-mixed-boot llamabuild.byte
	./build/camlp4-byte-only.sh

camlp4opt: llamaopt otherlibrariesopt llamabuild-mixed-boot llamabuild.native
	./build/camlp4-native-only.sh

# Llamabuild

llamabuild.byte: llamac otherlibraries llamabuild-mixed-boot
	./build/llamabuild-byte-only.sh

llamabuild.native: llamaopt otherlibrariesopt llamabuild-mixed-boot
	./build/llamabuild-native-only.sh
llamabuildlib.native: llamaopt otherlibrariesopt llamabuild-mixed-boot
	./build/llamabuildlib-native-only.sh

llamabuild-mixed-boot: llamac otherlibraries
	./build/mixed-boot.sh

partialclean::
	rm -rf _build

# Check that the stack limit is reasonable.

checkstack:
	@if $(BYTECC) -o tools/checkstack tools/checkstack.c; \
	  then tools/checkstack; \
	  else :; \
	fi
	@rm -f tools/checkstack

# Make MacOS X package

package-macosx:
	sudo rm -rf package-macosx/root
	make PREFIX="`pwd`"/package-macosx/root install
	tools/make-package-macosx
	sudo rm -rf package-macosx/root

clean::
	rm -rf package-macosx/*.pkg package-macosx/*.dmg

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp driver toplevel tools; \
	  do rm -f $$d/*.cm[iox] $$d/*.annot $$d/*.[so] $$d/*~; done
	rm -f *~

depend: beforedepend
	(for d in utils parsing typing bytecomp driver toplevel; \
	 do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
	 done) > .depend

alldepend:: depend

distclean:
	./build/distclean.sh

.PHONY: all backup bootstrap camlp4opt camlp4out checkstack clean
.PHONY: partialclean beforedepend alldepend cleanboot coldstart
.PHONY: compare core coreall
.PHONY: coreboot defaultentry depend distclean install installopt
.PHONY: library library-cross libraryopt llamabuild-mixed-boot
.PHONY: llamabuild.byte llamabuild.native llamadebugger llamadoc
.PHONY: llamadoc.opt llamalex llamalex.opt llamatools llamatools.opt
.PHONY: llamayacc opt-core opt opt.opt otherlibraries
.PHONY: otherlibrariesopt package-macosx promote promote-cross
.PHONY: restore runtime runtimeopt makeruntimeopt world world.opt

include .depend



PUBLIC=stdlib/pervasives.mli \
  stdlib/array.mli stdlib/list.mli stdlib/char.mli stdlib/string.mli stdlib/sys.mli \
  stdlib/hashtbl.mli stdlib/sort.mli stdlib/marshal.mli \
  stdlib/int32.mli stdlib/int64.mli stdlib/nativeint.mli \
  stdlib/obj.mli stdlib/lexing.mli stdlib/parsing.mli \
  stdlib/set.mli stdlib/map.mli \
  stdlib/stack.mli stdlib/queue.mli \
  stdlib/stream.mli \
  stdlib/buffer.mli stdlib/printf.mli stdlib/format.mli \
  stdlib/arg.mli stdlib/printexc.mli stdlib/gc.mli \
  stdlib/digest.mli stdlib/random_state.mli stdlib/random.mli stdlib/callback.mli \
  stdlib/genlex.mli stdlib/weak.mli \
  stdlib/filename.mli stdlib/complex.mli stdlib/scanning.mli stdlib/scanf.mli \
  stdlib/str.mli \
  parsing/location.mli parsing/parsetree.mli parsing/parse.mli

docs: library llamac llamadoc/llamadoc
	llamadoc/llamadoc -html $(INCLUDES) -I stdlib -d doc/libref $(PUBLIC)
.PHONY: docs
