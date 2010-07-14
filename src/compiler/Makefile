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
         ocamldep *.mli *.ml) > Makefile
	rm Makefile.bak

location.ml: location.mlp
config.ml: config.mlp
version.ml: version.mlp ../version.h

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
back.cmo: instruct.cmo 
back.cmx: instruct.cmx 
buffcode.cmo: 
buffcode.cmx: 
compiler.cmo: front.cmo emit_phr.cmo back.cmo 
compiler.cmx: front.cmx emit_phr.cmx back.cmx 
emit_phr.cmo: reloc.cmo labels.cmo instruct.cmo emitcode.cmo buffcode.cmo 
emit_phr.cmx: reloc.cmx labels.cmx instruct.cmx emitcode.cmx buffcode.cmx 
emitcode.cmo: reloc.cmo prim_opc.cmo opcodes.cmo labels.cmo instruct.cmo \
    buffcode.cmo 
emitcode.cmx: reloc.cmx prim_opc.cmx opcodes.cmx labels.cmx instruct.cmx \
    buffcode.cmx 
front.cmo: trstream.cmo 
front.cmx: trstream.cmx 
instruct.cmo: 
instruct.cmx: 
labels.cmo: instruct.cmo buffcode.cmo 
labels.cmx: instruct.cmx buffcode.cmx 
main.cmo: version.cmo compiler.cmo 
main.cmx: version.cmx compiler.cmx 
opcodes.cmo: 
opcodes.cmx: 
prim_opc.cmo: opcodes.cmo 
prim_opc.cmx: opcodes.cmx 
reloc.cmo: buffcode.cmo 
reloc.cmx: buffcode.cmx 
trstream.cmo: 
trstream.cmx: 
version.cmo: 
version.cmx: 
