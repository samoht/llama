# Makefile for the Caml Light compiler.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
CAMLLEX=ocamllex.opt
CAMLYACC=ocamlyacc
CPP=/usr/bin/cpp -P -Dunix
INCLUDES=-I ../typing
COMPFLAGS=-g $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)

EXTERNOBJS=config.cmx misc.cmx interntl.cmx const.cmx prim.cmx \
 lambda.cmx globals.cmx location.cmx syntax.cmx \
 modules.cmx builtins.cmx types.cmx \
 pr_type.cmx error.cmx typing.cmx ty_decl.cmx pr_decl.cmx ty_intf.cmx \
 tr_env.cmx event.cmx clauses.cmx matching.cmx  \
 primdecl.cmx lexer.cmx par_aux.cmx parser.cmx

OBJS=trstream.cmx front.cmx \
 instruct.cmx back.cmx opcodes.cmx prim_opc.cmx buffcode.cmx \
 labels.cmx reloc.cmx \
 emitcode.cmx emit_phr.cmx \
 compiler.cmx version.cmx main.cmx

GENSOURCES=opcodes.ml version.ml

all: camlcomp

camlcomp: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camlcomp $(EXTERNOBJS) $(OBJS)

clean:
	rm -f *.cmi *.cmx *.o camlcomp
	rm -f $(GENSOURCES) lexer.mll

install:
	cp camlcomp $(LIBDIR)/camlcomp

opcodes.ml: ../runtime/instruct.h
	sed -n -e '/^enum/p' -e 's/,//' -e '/^  /p' ../runtime/instruct.h | \
        awk -f ../tools/make-opcodes > opcodes.ml

lexer.mll: lexer.mlp
	$(CPP) lexer.mlp > lexer.mll

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@

depend: $(GENSOURCES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ocamldep.opt $(INCLUDES) *.mli *.ml) > Makefile
	rm Makefile.bak

location.ml: location.mlp
config.ml: config.mlp
version.ml: version.mlp ../version.h

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
back.cmo: ../typing/prim.cmo ../typing/misc.cmo ../typing/lambda.cmo \
    instruct.cmo ../typing/const.cmo 
back.cmx: ../typing/prim.cmx ../typing/misc.cmx ../typing/lambda.cmx \
    instruct.cmx ../typing/const.cmx 
buffcode.cmo: ../typing/error.cmo 
buffcode.cmx: ../typing/error.cmx 
compiler.cmo: ../typing/typing.cmo ../typing/ty_intf.cmo \
    ../typing/ty_decl.cmo ../typing/syntax.cmo ../typing/pr_decl.cmo \
    ../typing/modules.cmo ../typing/misc.cmo ../typing/location.cmi \
    ../typing/lexer.cmi ../typing/interntl.cmo front.cmo ../typing/error.cmo \
    emit_phr.cmo back.cmo 
compiler.cmx: ../typing/typing.cmx ../typing/ty_intf.cmx \
    ../typing/ty_decl.cmx ../typing/syntax.cmx ../typing/pr_decl.cmx \
    ../typing/modules.cmx ../typing/misc.cmx ../typing/location.cmi \
    ../typing/lexer.cmi ../typing/interntl.cmx front.cmx ../typing/error.cmx \
    emit_phr.cmx back.cmx 
emit_phr.cmo: reloc.cmo ../typing/lambda.cmo labels.cmo instruct.cmo \
    ../typing/event.cmo emitcode.cmo buffcode.cmo 
emit_phr.cmx: reloc.cmx ../typing/lambda.cmx labels.cmx instruct.cmx \
    ../typing/event.cmx emitcode.cmx buffcode.cmx 
emitcode.cmo: reloc.cmo prim_opc.cmo ../typing/prim.cmo opcodes.cmo \
    ../typing/misc.cmo ../typing/lambda.cmo labels.cmo instruct.cmo \
    ../typing/event.cmo ../typing/const.cmo ../typing/config.cmi buffcode.cmo 
emitcode.cmx: reloc.cmx prim_opc.cmx ../typing/prim.cmx opcodes.cmx \
    ../typing/misc.cmx ../typing/lambda.cmx labels.cmx instruct.cmx \
    ../typing/event.cmx ../typing/const.cmx ../typing/config.cmi buffcode.cmx 
front.cmo: ../typing/types.cmo trstream.cmo ../typing/tr_env.cmo \
    ../typing/syntax.cmo ../typing/prim.cmo ../typing/modules.cmo \
    ../typing/misc.cmo ../typing/matching.cmo ../typing/lambda.cmo \
    ../typing/globals.cmo ../typing/event.cmo ../typing/error.cmo \
    ../typing/const.cmo ../typing/builtins.cmo 
front.cmx: ../typing/types.cmx trstream.cmx ../typing/tr_env.cmx \
    ../typing/syntax.cmx ../typing/prim.cmx ../typing/modules.cmx \
    ../typing/misc.cmx ../typing/matching.cmx ../typing/lambda.cmx \
    ../typing/globals.cmx ../typing/event.cmx ../typing/error.cmx \
    ../typing/const.cmx ../typing/builtins.cmx 
instruct.cmo: ../typing/prim.cmo ../typing/lambda.cmo ../typing/const.cmo 
instruct.cmx: ../typing/prim.cmx ../typing/lambda.cmx ../typing/const.cmx 
labels.cmo: ../typing/misc.cmo instruct.cmo buffcode.cmo 
labels.cmx: ../typing/misc.cmx instruct.cmx buffcode.cmx 
main.cmo: version.cmo ../typing/typing.cmo ../typing/modules.cmo \
    ../typing/misc.cmo ../typing/interntl.cmo ../typing/event.cmo \
    ../typing/config.cmi compiler.cmo 
main.cmx: version.cmx ../typing/typing.cmx ../typing/modules.cmx \
    ../typing/misc.cmx ../typing/interntl.cmx ../typing/event.cmx \
    ../typing/config.cmi compiler.cmx 
opcodes.cmo: 
opcodes.cmx: 
prim_opc.cmo: ../typing/prim.cmo opcodes.cmo ../typing/misc.cmo 
prim_opc.cmx: ../typing/prim.cmx opcodes.cmx ../typing/misc.cmx 
reloc.cmo: ../typing/const.cmo buffcode.cmo 
reloc.cmx: ../typing/const.cmx buffcode.cmx 
trstream.cmo: ../typing/tr_env.cmo ../typing/syntax.cmo ../typing/prim.cmo \
    ../typing/matching.cmo ../typing/lambda.cmo ../typing/const.cmo 
trstream.cmx: ../typing/tr_env.cmx ../typing/syntax.cmx ../typing/prim.cmx \
    ../typing/matching.cmx ../typing/lambda.cmx ../typing/const.cmx 
version.cmo: ../typing/interntl.cmo 
version.cmx: ../typing/interntl.cmx 
