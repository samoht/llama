include config/Makefile

OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt
OCAMLLEX=ocamllex.opt
OCAMLYACC=ocamlyacc
INCLUDES=-I utils -I parsing -I typing -I compiler -I linker -I toplevel
FLAGS=-g $(INCLUDES)

UTILS=utils/config.cmx utils/clflags.cmx utils/misc.cmx utils/tbl.cmx

PARSING= parsing/location.cmx \
 parsing/longident.cmx parsing/parser.cmx parsing/lexer.cmx 

TYPING=typing/ident.cmx typing/path.cmx \
 typing/primitive.cmx typing/types.cmx \
 typing/btype.cmx \
 typing/modules.cmx \
 typing/predef.cmx \
 typing/env.cmx \
 typing/pr_type.cmx typing/pr_decl.cmx \
 typing/typedtree_aux.cmx typing/ctype.cmx \
 typing/error.cmx \
 typing/printtyp.cmx \
 typing/includecore.cmx typing/includemod.cmx \
 typing/resolve.cmx \
 typing/typecore.cmx \
 typing/typedecl.cmx typing/typemod.cmx typing/ty_intf.cmx

#TYPING=typing/unused_var.cmo typing/ident.cmo typing/path.cmo \
#  typing/primitive.cmo typing/types.cmo \
#  typing/btype.cmo typing/oprint.cmo \
#  typing/subst.cmo typing/predef.cmo \
#  typing/datarepr.cmo typing/env.cmo \
#  typing/typedtree.cmo typing/ctype.cmo \
#  typing/printtyp.cmo typing/includeclass.cmo \
#  typing/mtype.cmo typing/includecore.cmo \
#  typing/includemod.cmo typing/parmatch.cmo \
#  typing/typetexp.cmo typing/stypes.cmo typing/typecore.cmo \
#  typing/typedecl.cmo typing/typeclass.cmo \
#  typing/typemod.cmo

COMPILER=compiler/prim.cmx compiler/primdecl.cmx \
 compiler/lambda.cmx compiler/clauses.cmx compiler/matching.cmx \
 compiler/event.cmx \
 compiler/tr_env.cmx compiler/trstream.cmx compiler/front.cmx \
 compiler/instruct.cmx compiler/back.cmx compiler/opcodes.cmx \
 compiler/prim_opc.cmx compiler/buffcode.cmx \
 compiler/labels.cmx compiler/reloc.cmx \
 compiler/emitcode.cmx compiler/emit_phr.cmx \
 compiler/compiler.cmx

LINKER=linker/caml_light_extern.o \
  linker/more_predef.cmx linker/prim_c.cmx linker/symtable.cmx \
  linker/patch.cmx linker/tr_const.cmx linker/link.cmx \
  linker/readword.cmx

TOPLEVEL=\
  toplevel/eval.cmx toplevel/fmt_type.cmx toplevel/pr_value.cmx \
  toplevel/load_phr.cmx toplevel/do_phr.cmx toplevel/toplevel.cmx \
  toplevel/main.cmx runtime/libcaml.a toplevel/llama.o

GENSOURCES=utils/config.ml parsing/lexer.ml \
 compiler/opcodes.ml linker/prim_c.ml linker/more_predef.ml parsing/parser.ml

all: runtime_dir llama llamac llamadep testprog stdlib_dir
.PHONY: all

testprog: testprog.ml runtime_dir stdlib_dir
	./llamac -I stdlib $< -o $@
	runtime/llamarun testprog
	@ echo "Is that 10946 on the line above? Good."
	@ echo "The Llama system is up and running."

llama: $(UTILS) $(PARSING) $(TYPING) $(COMPILER) $(LINKER) $(TOPLEVEL)
	$(OCAMLOPT) $(FLAGS) -o $@ $^

llamac: $(UTILS) $(PARSING) $(TYPING) $(COMPILER) $(LINKER) compiler/librarian.cmx compiler/driver.cmx
	$(OCAMLOPT) $(FLAGS) -o $@ $^

%.cmx: %.ml
	$(OCAMLOPT) -c $(FLAGS) -o $@ $<

%.cmi: %.mli
	$(OCAMLOPT) -c $(FLAGS) -o $@ $<

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
            -e 's|%%BYTERUN%%|$(BINDIR)/llamarun|' \
            -e "s|%%VERSION%%|`head -1 VERSION`|" \
            utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

parsing/lexer.ml: parsing/lexer.mll
	$(OCAMLLEX) $<

parsing/parser.ml parsing/parser.mli: parsing/parser.mly
	$(OCAMLYACC) $<

compiler/opcodes.ml: runtime/instruct.h
	sed -n -e '/^enum/p' -e 's/,//' -e '/^  /p' $< | \
        awk -f tools/make-opcodes > $@

linker/prim_c.ml : runtime/primitives
	(echo 'let primitives_table = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' runtime/primitives; \
	 echo '|];;') > $@

linker/more_predef.ml : runtime/globals.h runtime/fail.h
	(echo 'open Types;;'; \
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

toplevel/llama.o: toplevel/llama.c
	$(OCAMLOPT) -c -ccopt "-I . -o $@" $<

runtime/primitives:
	cd runtime && make primitives
runtime/libcaml.a:
	cd runtime && make libcaml.a
runtime/libcamld.a:
	cd runtime && make libcamld.a
runtime_dir:
	cd runtime && make
stdlib_dir:
	cd stdlib && make
.PHONY: runtime_dir stdlib_dir

semiclean:
	rm -f llama llamac llamarun stdlib.zo
	rm -f $(GENSOURCES)
	rm -f {utils,parsing,typing,compiler,linker,toplevel}/*.{cmi,cmx,o}
	rm -f testprog{,.zi,.zo}
	cd stdlib && make clean
.PHONY: semiclean
clean: semiclean
	cd runtime && make clean
.PHONY: clean

depend: $(GENSOURCES)
	$(OCAMLDEP) -native $(INCLUDES) {utils,parsing,typing,compiler,linker,toplevel}/*.{mli,ml} > .depend
.PHONY: depend

include .depend

configure-in-situ:
	./configure -bindir ${PWD}/runtime -libdir ${PWD}/stdlib
.PHONY: configure-in-situ
