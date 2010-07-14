CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
CAMLLIBR=ocamlopt.opt -a
CAMLLEX=ocamllex.opt
CAMLYACC=ocamlyacc
CPP=/usr/bin/cpp -P -Dunix
INCLUDES=-I utils -I typing -I compiler -I linker -I librar -I toplevel
COMPFLAGS=-g $(INCLUDES) 
LINKFLAGS=-g $(INCLUDES)

UTILS=utils/version.cmx

TYPING=typing/config.cmx typing/misc.cmx typing/interntl.cmx \
 typing/const.cmx typing/prim.cmx typing/lambda.cmx typing/globals.cmx \
 typing/location.cmx typing/syntax.cmx \
 typing/modules.cmx typing/builtins.cmx typing/types.cmx \
 typing/pr_type.cmx typing/error.cmx typing/typing.cmx \
 typing/ty_decl.cmx typing/pr_decl.cmx typing/ty_intf.cmx \
 typing/tr_env.cmx typing/event.cmx typing/clauses.cmx typing/matching.cmx \
 typing/primdecl.cmx typing/lexer.cmx typing/par_aux.cmx typing/parser.cmx

COMPILER=compiler/trstream.cmx compiler/front.cmx \
 compiler/instruct.cmx compiler/back.cmx compiler/opcodes.cmx \
 compiler/prim_opc.cmx compiler/buffcode.cmx \
 compiler/labels.cmx compiler/reloc.cmx \
 compiler/emitcode.cmx compiler/emit_phr.cmx \
 compiler/compiler.cmx

LINKER=linker/caml_light_extern.o \
  linker/predef.cmx linker/prim_c.cmx linker/symtable.cmx \
  linker/patch.cmx linker/tr_const.cmx linker/link.cmx \
  linker/readword.cmx

LIBRAR=librar/librar.cmx

TOPLEVEL=toplevel/eval.cmx toplevel/fmt_type.cmx toplevel/pr_value.cmx \
  toplevel/meta.cmx toplevel/do_phr.cmx toplevel/toplevel.cmx \
  toplevel/topinit.cmx toplevel/topmain.cmx

GENSOURCES=utils/version.ml typing/lexer.ml typing/parser.ml typing/parser.mli \
 compiler/opcodes.ml linker/prim_c.ml linker/predef.ml

all: camlcomp camllink camllibr camltop the_library the_runtime testprog

testprog.zo: testprog.ml camlcomp the_library
	./camlcomp -I lib $<

testprog: testprog.zo camllink the_library the_runtime
	./camllink -I lib stdlib.zo $< -o $@
	runtime/camlrun testprog
	@ echo "Is that 10946 on the line above? Good."
	@ echo "The system is up and running."

camlcomp: $(UTILS) $(TYPING) $(COMPILER) compiler/main.cmx
	$(CAMLLINK) $(LINKFLAGS) -o camlcomp $^

camllink: $(UTILS) $(TYPING) $(COMPILER) $(LINKER) linker/main.cmx
	$(CAMLLINK) $(LINKFLAGS) -o camllink $^

camllibr: $(UTILS) $(TYPING) $(COMPILER) $(LINKER) $(LIBRAR) librar/main.cmx
	$(CAMLLINK) $(LINKFLAGS) -o camllibr $^

camltop: $(UTILS) $(TYPING) $(COMPILER) $(LINKER) $(TOPLEVEL)
	$(CAMLLINK) $(LINKFLAGS) -o camltop $^

the_library:
	cd lib && make
the_runtime:
	cd runtime && make
.PHONY: the_library the_runtime

runtime/version.h: VERSION
	echo "#define VERSION \"`head -1 VERSION`\"" > $@

utils/version.ml: utils/version.mlp VERSION
	sed -e "s|%%VERSION%%|`head -1 VERSION`|" $< > $@

typing/lexer.ml: typing/lexer.mll
	$(CAMLLEX) $<

typing/parser.ml typing/parser.mli: typing/parser.mly
	$(CAMLYACC) $<

compiler/opcodes.ml: runtime/instruct.h
	sed -n -e '/^enum/p' -e 's/,//' -e '/^  /p' $< | \
        awk -f tools/make-opcodes > $@

PRIMS=runtime/compare.c runtime/extern.c runtime/externcp.c \
  runtime/floats.c runtime/gc_ctrl.c runtime/hash.c \
  runtime/intern.c runtime/interncp.c runtime/interp.c \
  runtime/ints.c runtime/io.c runtime/lexing.c runtime/meta.c runtime/parsing.c \
  runtime/str.c runtime/sys.c

runtime/primitives : $(PRIMS)
	sed -n -e '/\/\* ML \*\//s/.* \([a-z0-9_][a-z0-9_]*\) *(.*/\1/p' \
                $(PRIMS) > primitives2
	sh -c 'if cmp -s runtime/primitives primitives2; \
        then rm primitives2; \
        else mv primitives2 runtime/primitives; \
        fi'

linker/prim_c.ml : runtime/primitives
	(echo 'let primitives_table = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' $<; \
	 echo '|];;') > $@

linker/predef.ml : runtime/globals.h runtime/fail.h
	(echo 'open Const;;'; \
         echo 'let predef_variables = ['; \
	 sed -n -e 's|.*/\* \(".*"\), *\(".*"\) \*/$$|{qual=\1; id=\2};|p' \
                $< \
           | sed -e '$$s|;$$||'; \
         echo '];;'; \
         echo 'let predef_exn = ['; \
         sed -n -e 's|.*/\* \(".*"\), *\(".*"\), *\([0-9]*\) \*/$$|({qual=\1; id=\2}, \3);|p' \
                runtime/fail.h \
           | sed -e '$$s|;$$||'; \
         echo '];;') > $@

linker/caml_light_extern.o: linker/caml_light_extern.c
	$(CAMLCOMP) -ccopt "-o $@" $<

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLCOMP) $(COMPFLAGS) $<

SUBDIRS=runtime launch lib compiler linker librar toplevel tools

# Configure the system
configure:
	cd ../config; sh autoconf "$(CC) $(OPTS) $(LIBS)"

# Build the system for the first time
world:
	cd runtime; make CC="$(CC)" OPTS="$(OPTS)" LIBS="$(LIBS)" all
	cd typing; make CPP="$(CPP)" all
	cd compiler; make CPP="$(CPP)" all
	cd linker; make CPP="$(CPP)" all
	cd librar; make CPP="$(CPP)" all
	cd lib; make CPP="$(CPP)" all
	cd toplevel; make CPP="$(CPP)" all
	runtime/camlrun launch/testprog
	@ echo "Is that 10946 on the line above? Good."
	@ echo "The Caml Dark system is up and running."

clean:
	rm -f camlcomp camllink camllibr camltop
	rm -f $(GENSOURCES)
	rm -f {utils,typing,compiler,linker,librar,toplevel}/*.{cmi,cmx,o}
	cd runtime && make clean
	cd lib && make clean
	rm -f testprog{,.zi,.zo}
.PHONY: clean

depend: $(GENSOURCES)
	ocamldep.opt $(INCLUDES) {utils,typing,compiler,linker,librar,toplevel}/*.{mli,ml} > .depend
.PHONY: depend

-include .depend
