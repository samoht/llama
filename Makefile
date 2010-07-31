include config/Makefile

OCAMLC=boot/llamac
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt
OCAMLLEX=ocamllex.opt
OCAMLYACC=ocamlyacc
INCLUDES=-I utils -I parsing -I typing -I bytecomp -I driver -I toplevel
FLAGS=-g $(INCLUDES)

UTILS=utils/config.cmx utils/clflags.cmx utils/misc.cmx utils/tbl.cmx utils/warnings.cmx utils/consistbl.cmx utils/ccomp.cmx

PARSING=parsing/location.cmx parsing/syntaxerr.cmx \
 parsing/longident.cmx parsing/parser.cmx parsing/lexer.cmx parsing/parse.cmx parsing/printast.cmx

TYPING=typing/unused_var.cmx typing/primitive.cmx \
 typing/types.cmx \
 typing/predef.cmx \
 typing/module.cmx \
 typing/env.cmx \
 typing/get.cmx \
 typing/btype.cmx \
 typing/subst.cmx \
 typing/oprint.cmx \
 typing/printtyp.cmx typing/pr_decl.cmx \
 typing/typedtree_aux.cmx typing/ctype.cmx \
 typing/error.cmx \
 typing/includecore.cmx typing/includemod.cmx \
 typing/resolve.cmx \
 typing/typecore.cmx \
 typing/typedecl.cmx typing/typemod.cmx \
 typing/parmatch.cmx

BYTECOMP=bytecomp/ident.cmx bytecomp/identSet.cmx \
  bytecomp/lambda.cmx bytecomp/printlambda.cmx \
  bytecomp/typeopt.cmx \
  bytecomp/mpattern.cmx \
  bytecomp/optparmatch.cmx \
  bytecomp/switch.cmx bytecomp/optmatching.cmx \
  bytecomp/translcore.cmx \
  bytecomp/translmod.cmx \
  bytecomp/simplif.cmx bytecomp/runtimedef.cmx \
  bytecomp/instruct.cmx bytecomp/bytegen.cmx \
  bytecomp/printinstr.cmx bytecomp/opcodes.cmx bytecomp/emitcode.cmx \
  bytecomp/bytesections.cmx bytecomp/dll.cmx bytecomp/symtable.cmx \
  bytecomp/bytelink.cmx bytecomp/bytelibrarian.cmx

DRIVER=driver/pparse.cmx driver/errors.cmx driver/compile.cmx driver/main_args.cmx driver/main.cmx

TOPLEVEL=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  driver/main_args.cmo toplevel/printer.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo toplevel/topmain.cmo \
  toplevel/topstart.cmo

GENSOURCES=utils/config.ml parsing/lexer.ml \
 parsing/parser.ml \
 bytecomp/runtimedef.ml

all: stdlib_dir llamac llama runtime_dir testprog
.PHONY: all
promote:
	cp llamac boot/llamac
.PHONY: promote

testprog: testprog.ml byterun_dir stdlib_dir
	./llamac -I stdlib $< -o $@
	byterun/llamarun testprog
	@ echo "Is that 10946 on the line above? Good."
	@ echo "The Llama system is up and running."

llama: $(UTILS:.cmx=.cmo) $(PARSING:.cmx=.cmo) $(TYPING:.cmx=.cmo) $(BYTECOMP:.cmx=.cmo) $(TOPLEVEL)
	$(OCAMLC) $(FLAGS) -linkall -o $@ $^

llamac: $(UTILS:.cmx=.cmo) $(PARSING:.cmx=.cmo) $(TYPING:.cmx=.cmo) $(BYTECOMP:.cmx=.cmo) $(DRIVER:.cmx=.cmo)
	$(OCAMLC) $(FLAGS) -o $@ $^

%.cmx: %.ml
	$(OCAMLOPT) -c $(FLAGS) -o $@ $<

%.cmo: %.ml
	$(OCAMLC) -c $(FLAGS) -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c $(FLAGS) -o $@ $<

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

runtime/primitives:
	cd runtime && make primitives
runtime/libcaml.a:
	cd runtime && make libcaml.a
runtime/libcamld.a:
	cd runtime && make libcamld.a
runtime_dir:
	cd runtime && make
byterun_dir:
	cd byterun && make
stdlib_dir:
	cd stdlib && make
.PHONY: runtime_dir stdlib_dir byterun_dir

semiclean:
	rm -f llama llamac
	rm -f $(GENSOURCES)
	rm -f {utils,parsing,typing,bytecomp,driver,toplevel}/*.{cmi,cmo,cmx,o}
	rm -f testprog{,.cmi,.cmo}
	cd stdlib && make clean
.PHONY: semiclean
clean: semiclean
	cd runtime && make clean
	cd byterun && make clean
.PHONY: clean

depend: $(GENSOURCES)
	$(OCAMLDEP) $(INCLUDES) {utils,parsing,typing,bytecomp,driver,toplevel}/*.{mli,ml} > .depend
.PHONY: depend

include .depend

configure-in-situ:
	./configure -bindir ${PWD}/byterun -libdir ${PWD}/stdlib
.PHONY: configure-in-situ

