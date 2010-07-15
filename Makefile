include config/Makefile

OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt
OCAMLLEX=ocamllex.opt
OCAMLYACC=ocamlyacc
INCLUDES=-I utils -I typing -I compiler -I linker -I toplevel
FLAGS=-g $(INCLUDES) 

UTILS=utils/config.cmx utils/clflags.cmx

TYPING=typing/misc.cmx typing/interntl.cmx \
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

TOPLEVEL=toplevel/eval.cmx toplevel/fmt_type.cmx toplevel/pr_value.cmx \
  toplevel/meta.cmx toplevel/do_phr.cmx toplevel/toplevel.cmx \
  toplevel/topinit.cmx toplevel/topmain.cmx

GENSOURCES=utils/config.ml typing/lexer.ml typing/parser.ml typing/parser.mli \
 compiler/opcodes.ml linker/prim_c.ml linker/predef.ml

all: zebra zebrac zebradep testprog runtime_dir stdlib_dir
.PHONY: all

testprog: testprog.ml runtime_dir stdlib_dir
	./zebrac -I stdlib $< -o $@
	runtime/zebrarun testprog
	@ echo "Is that 10946 on the line above? Good."
	@ echo "The Zebra system is up and running."

zebra: $(UTILS) $(TYPING) $(COMPILER) $(LINKER) $(TOPLEVEL)
	$(OCAMLOPT) $(FLAGS) -o $@ $^

zebrac: $(UTILS) $(TYPING) $(COMPILER) $(LINKER) compiler/librarian.cmx compiler/driver.cmx
	$(OCAMLOPT) $(FLAGS) -o $@ $^

%.cmx: %.ml
	$(OCAMLOPT) -c $(FLAGS) -o $@ $<

%.cmi: %.mli
	$(OCAMLOPT) -c $(FLAGS) -o $@ $<

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
            -e 's|%%BYTERUN%%|$(BINDIR)/zebrarun|' \
            -e 's|%%VERSION%%|`head -1 VERSION`|' \
            utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

typing/lexer.ml: typing/lexer.mll
	$(OCAMLLEX) $<

typing/parser.ml typing/parser.mli: typing/parser.mly
	$(OCAMLYACC) $<

compiler/opcodes.ml: runtime/instruct.h
	sed -n -e '/^enum/p' -e 's/,//' -e '/^  /p' $< | \
        awk -f tools/make-opcodes > $@

linker/prim_c.ml : runtime/primitives
	(echo 'let primitives_table = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' runtime/primitives; \
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
	$(OCAMLOPT) -c -ccopt "-o $@" $<

runtime/primitives:
	cd runtime && make primitives
runtime_dir:
	cd runtime && make
stdlib_dir:
	cd stdlib && make
.PHONY: runtime_dir stdlib_dir

clean:
	rm -f zebra zebrac zebrarun stdlib.zo
	rm -f $(GENSOURCES)
	rm -f {utils,typing,compiler,linker,toplevel}/*.{cmi,cmx,o}
	rm -f testprog{,.zi,.zo}
	cd runtime && make clean
	cd stdlib && make clean
.PHONY: clean

depend: $(GENSOURCES)
	$(OCAMLDEP) -native $(INCLUDES) {utils,typing,compiler,linker,toplevel}/*.{mli,ml} > .depend
.PHONY: depend

-include .depend

SRCDIR=/Users/jeremy/zebra
dummyconfig:
	./configure -bindir $(SRCDIR)/runtime -libdir $(SRCDIR)/stdlib
.PHONY: dummyconfig
