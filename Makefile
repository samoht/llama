include config/Makefile

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt
OCAMLLEX=ocamllex.opt
OCAMLYACC=ocamlyacc
INCLUDES=-I utils -I parsing -I typing -I cl_comp -I cl_toplevel -I bytecomp -I driver
FLAGS=-g $(INCLUDES)

UTILS=utils/config.cmx utils/clflags.cmx utils/misc.cmx utils/tbl.cmx utils/warnings.cmx utils/consistbl.cmx utils/ccomp.cmx

PARSING=parsing/location.cmx parsing/syntaxerr.cmx \
 parsing/longident.cmx parsing/parser.cmx parsing/lexer.cmx parsing/parse.cmx parsing/printast.cmx

TYPING=typing/unused_var.cmx typing/primitive.cmx \
 typing/types.cmx \
 typing/predef.cmx \
 typing/get.cmx \
 typing/module.cmx \
 typing/env.cmx typing/btype.cmx \
 typing/subst.cmx \
 typing/printtyp.cmx typing/pr_decl.cmx \
 typing/typedtree_aux.cmx typing/ctype.cmx \
 typing/error.cmx \
 typing/includecore.cmx typing/includemod.cmx \
 typing/resolve.cmx \
 typing/typecore.cmx \
 typing/typedecl.cmx typing/typemod.cmx \
 typing/parmatch.cmx

CL_COMP=cl_comp/prim.cmx cl_comp/primdecl.cmx \
 cl_comp/cl_lambda.cmx cl_comp/cl_matching.cmx \
 cl_comp/event.cmx \
 cl_comp/tr_env.cmx cl_comp/trstream.cmx cl_comp/front.cmx \
 cl_comp/cl_instruct.cmx cl_comp/back.cmx cl_comp/cl_opcodes.cmx \
 cl_comp/prim_opc.cmx cl_comp/buffcode.cmx \
 cl_comp/labels.cmx cl_comp/reloc.cmx \
 cl_comp/cl_emitcode.cmx cl_comp/emit_phr.cmx \
 cl_comp/compiler.cmx \
 cl_comp/caml_light_extern.o \
  cl_comp/more_predef.cmx cl_comp/prim_c.cmx cl_comp/cl_symtable.cmx \
  cl_comp/patch.cmx cl_comp/tr_const.cmx cl_comp/link.cmx \
  cl_comp/readword.cmx

BYTECOMP=bytecomp/ident.cmx bytecomp/identSet.cmx bytecomp/lambda.cmx bytecomp/printlambda.cmx \
  bytecomp/typeopt.cmx bytecomp/switch.cmx bytecomp/matching_aux.cmx bytecomp/matching.cmx \
  bytecomp/translcore.cmx \
  bytecomp/translmod.cmx \
  bytecomp/simplif.cmx bytecomp/runtimedef.cmx \
  bytecomp/instruct.cmx bytecomp/bytegen.cmx \
  bytecomp/printinstr.cmx bytecomp/opcodes.cmx bytecomp/emitcode.cmx \
  bytecomp/bytesections.cmx bytecomp/dll.cmx bytecomp/symtable.cmx \
  bytecomp/bytelink.cmx bytecomp/bytelibrarian.cmx

DRIVER=driver/pparse.cmx driver/errors.cmx driver/compile.cmx driver/main_args.cmx driver/main.cmx

CL_TOPLEVEL=\
  cl_toplevel/eval.cmx cl_toplevel/pr_value.cmx \
  cl_toplevel/load_phr.cmx cl_toplevel/do_phr.cmx cl_toplevel/toplevel.cmx \
  cl_toplevel/cl_topmain.cmx runtime/libcaml.a cl_toplevel/llama.o

GENSOURCES=utils/config.ml parsing/lexer.ml \
 cl_comp/cl_opcodes.ml cl_comp/prim_c.ml cl_comp/more_predef.ml parsing/parser.ml

all: runtime_dir llama llamac llamadep testprog cl_stdlib_dir llamac-new
.PHONY: all

testprog: testprog.ml runtime_dir cl_stdlib_dir
	./llamac -I cl_stdlib $< -o $@
	runtime/llamarun testprog
	@ echo "Is that 10946 on the line above? Good."
	@ echo "The Llama system is up and running."

llama: $(UTILS) $(PARSING) $(TYPING) $(CL_COMP) $(CL_TOPLEVEL)
	$(OCAMLOPT) $(FLAGS) -o $@ $^

llamac: $(UTILS) $(PARSING) $(TYPING) $(CL_COMP) cl_comp/librarian.cmx cl_comp/driver.cmx
	$(OCAMLOPT) $(FLAGS) -o $@ $^

llamac.byte: $(UTILS:.cmx=.cmo) $(PARSING:.cmx=.cmo) $(TYPING:.cmx=.cmo) $(CL_COMP:.cmx=.cmo) cl_comp/librarian.cmo cl_comp/driver.cmo
	$(OCAMLC) -custom $(FLAGS) -o $@ $^

%.cmx: %.ml
	$(OCAMLOPT) -c $(FLAGS) -o $@ $<
%.cmo: %.ml
	$(OCAMLC) -c $(FLAGS) -o $@ $<

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

cl_comp/cl_opcodes.ml: runtime/instruct.h
	sed -n -e '/^enum/p' -e 's/,//' -e '/^  /p' $< | \
        awk -f tools/make-opcodes > $@

cl_comp/prim_c.ml : runtime/primitives
	(echo 'let primitives_table = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' runtime/primitives; \
	 echo '|];;') > $@

cl_comp/more_predef.ml : runtime/globals.h runtime/fail.h
	(echo 'open Types;;'; \
         echo 'let predef_variables = ['; \
	 sed -n -e 's|.*/\* \(".*"\), *\(".*"\) \*/$$|{id_module=Module \1; id_name=\2};|p' \
                $< \
           | sed -e '$$s|;$$||'; \
         echo '];;'; \
         echo 'let predef_exn = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' runtime/fail.h | \
	 sed -e '$$s/;$$//'; \
	 echo '|];;') > $@

cl_comp/caml_light_extern.o: cl_comp/caml_light_extern.c
	$(OCAMLOPT) -c -ccopt "-o $@" $<

cl_toplevel/llama.o: cl_toplevel/llama.c
	$(OCAMLOPT) -c -ccopt "-I . -o $@" $<

runtime/primitives:
	cd runtime && make primitives
runtime/libcaml.a:
	cd runtime && make libcaml.a
runtime/libcamld.a:
	cd runtime && make libcamld.a
runtime_dir:
	cd runtime && make
cl_stdlib_dir:
	cd cl_stdlib && make
.PHONY: runtime_dir cl_stdlib_dir

semiclean:
	rm -f llama llamac llamarun stdlib.zo llamac-new
	rm -f $(GENSOURCES)
	rm -f {utils,parsing,typing,cl_comp,cl_toplevel,bytecomp,driver}/*.{cmi,cmo,cmx,o}
	rm -f testprog{,.zi,.zo}
	cd cl_stdlib && make clean
.PHONY: semiclean
clean: semiclean
	cd runtime && make clean
.PHONY: clean

depend: $(GENSOURCES)
	$(OCAMLDEP) -native $(INCLUDES) {utils,parsing,typing,cl_comp,cl_toplevel,bytecomp,driver}/*.{mli,ml} > .depend
.PHONY: depend

include .depend

configure-in-situ:
	./configure -bindir ${PWD}/runtime -libdir ${PWD}/cl_stdlib
.PHONY: configure-in-situ

llamac-new: $(UTILS) $(PARSING) $(TYPING) $(BYTECOMP) $(DRIVER)
	$(OCAMLOPT) $(FLAGS) -o $@ $^
llamac-new.byte: $(UTILS:.cmx=.cmo) $(PARSING:.cmx=.cmo) $(TYPING:.cmx=.cmo) $(BYTECOMP:.cmx=.cmo) $(DRIVER:.cmx=.cmo)
	$(OCAMLC) -custom $(FLAGS) -o $@ $^

