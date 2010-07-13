# Makefile for the Caml Light compiler.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
CAMLLEX=ocamllex.opt
CAMLYACC=ocamlyacc
CPP=/usr/bin/cpp -P -Dunix

COMPFLAGS=-g
LINKFLAGS=-g

OBJS=config.cmx misc.cmx interntl.cmx const.cmx prim.cmx lambda.cmx globals.cmx \
 location.cmx syntax.cmx \
 modules.cmx builtins.cmx types.cmx \
 pr_type.cmx error.cmx typing.cmx ty_decl.cmx pr_decl.cmx ty_intf.cmx \
 tr_env.cmx event.cmx clauses.cmx matching.cmx trstream.cmx front.cmx \
 instruct.cmx back.cmx opcodes.cmx prim_opc.cmx buffcode.cmx labels.cmx reloc.cmx \
 emitcode.cmx emit_phr.cmx \
 primdecl.cmx lexer.cmx par_aux.cmx parser.cmx compiler.cmx \
 version.cmx main.cmx

GENSOURCES=lexer.ml parser.ml parser.mli opcodes.ml config.ml \
  location.ml version.ml

all: camlcomp

camlcomp: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camlcomp $(OBJS)
        
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
         ../tools/camldep *.mli *.ml) > Makefile
	rm Makefile.bak

location.ml: location.mlp
config.ml: config.mlp
version.ml: version.mlp ../version.h

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
lexer.cmi: parser.cmi 
parser.cmi: syntax.cmx 
back.cmx: prim.cmx lambda.cmx const.cmx misc.cmx instruct.cmx 
buffcode.cmx: error.cmx 
builtins.cmx: modules.cmx globals.cmx const.cmx 
clauses.cmx: lambda.cmx error.cmx location.cmi const.cmx misc.cmx syntax.cmx \
    globals.cmx types.cmx 
compiler.cmx: pr_decl.cmx typing.cmx parser.cmi error.cmx modules.cmx location.cmi \
    interntl.cmx emit_phr.cmx misc.cmx back.cmx front.cmx ty_intf.cmx syntax.cmx \
    lexer.cmi ty_decl.cmx 
config.cmx: config.cmi 
const.cmx: misc.cmx 
emit_phr.cmx: event.cmx reloc.cmx lambda.cmx emitcode.cmx buffcode.cmx \
    instruct.cmx labels.cmx 
emitcode.cmx: prim.cmx reloc.cmx prim_opc.cmx lambda.cmx config.cmi labels.cmx \
    const.cmx misc.cmx event.cmx instruct.cmx buffcode.cmx opcodes.cmx 
error.cmx: misc.cmx syntax.cmx pr_type.cmx location.cmi interntl.cmx globals.cmx \
    types.cmx const.cmx 
event.cmx: syntax.cmx lambda.cmx modules.cmx location.cmi 
front.cmx: trstream.cmx prim.cmx lambda.cmx error.cmx tr_env.cmx matching.cmx \
    modules.cmx const.cmx misc.cmx event.cmx syntax.cmx globals.cmx builtins.cmx \
    types.cmx 
globals.cmx: prim.cmx const.cmx 
instruct.cmx: prim.cmx lambda.cmx const.cmx 
interntl.cmx: misc.cmx 
labels.cmx: misc.cmx buffcode.cmx instruct.cmx 
lambda.cmx: prim.cmx globals.cmx const.cmx 
lexer.cmx: lexer.cmi parser.cmi 
location.cmx: location.cmi interntl.cmx config.cmi 
main.cmx: typing.cmx modules.cmx interntl.cmx config.cmi compiler.cmx misc.cmx \
    version.cmx event.cmx 
matching.cmx: clauses.cmx prim.cmx lambda.cmx error.cmx location.cmi const.cmx \
    misc.cmx syntax.cmx globals.cmx builtins.cmx types.cmx 
modules.cmx: misc.cmx interntl.cmx globals.cmx const.cmx 
par_aux.cmx: error.cmx modules.cmx location.cmi const.cmx misc.cmx syntax.cmx \
    globals.cmx builtins.cmx 
parser.cmx: parser.cmi primdecl.cmx par_aux.cmx syntax.cmx builtins.cmx \
    globals.cmx const.cmx 
pr_decl.cmx: misc.cmx pr_type.cmx globals.cmx const.cmx 
pr_type.cmx: modules.cmx globals.cmx types.cmx const.cmx 
prim.cmx: const.cmx 
prim_opc.cmx: misc.cmx prim.cmx opcodes.cmx 
primdecl.cmx: prim.cmx globals.cmx 
reloc.cmx: buffcode.cmx const.cmx 
syntax.cmx: location.cmi globals.cmx const.cmx 
tr_env.cmx: prim.cmx lambda.cmx error.cmx const.cmx misc.cmx syntax.cmx \
    builtins.cmx globals.cmx 
trstream.cmx: prim.cmx lambda.cmx syntax.cmx tr_env.cmx matching.cmx const.cmx 
ty_decl.cmx: typing.cmx syntax.cmx error.cmx modules.cmx builtins.cmx globals.cmx \
    types.cmx const.cmx 
ty_intf.cmx: ty_decl.cmx error.cmx modules.cmx globals.cmx types.cmx const.cmx 
types.cmx: misc.cmx modules.cmx globals.cmx const.cmx 
typing.cmx: error.cmx modules.cmx const.cmx misc.cmx syntax.cmx globals.cmx \
    builtins.cmx types.cmx 
typing.old.cmx: error.cmx modules.cmx const.cmx misc.cmx syntax.cmx globals.cmx \
    builtins.cmx types.cmx 
version.cmx: interntl.cmx 
