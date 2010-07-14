# Makefile for the Caml Light compiler.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
CAMLLIBR=ocamlopt.opt -a
CAMLLEX=ocamllex.opt
CAMLYACC=ocamlyacc
CPP=/usr/bin/cpp -P -Dunix
INCLUDES=-I ../utils
COMPFLAGS=-g $(INCLUDES) 
LINKFLAGS=-g $(INCLUDES)

OBJS=config.cmx misc.cmx interntl.cmx const.cmx prim.cmx lambda.cmx globals.cmx \
 location.cmx syntax.cmx \
 modules.cmx builtins.cmx types.cmx \
 pr_type.cmx error.cmx typing.cmx ty_decl.cmx pr_decl.cmx ty_intf.cmx \
 tr_env.cmx event.cmx clauses.cmx matching.cmx \
 primdecl.cmx lexer.cmx par_aux.cmx parser.cmx

GENSOURCES=lexer.ml parser.ml parser.mli

all: typing.cmxa

typing.cmxa: $(OBJS)
	$(CAMLLIBR) $(LINKFLAGS) -o $@ $(OBJS)

clean:
	rm -f *.cmi *.cmx *.o typing.cmxa
	rm -f $(GENSOURCES)

install:
	cp camlcomp $(LIBDIR)/camlcomp

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

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
config.cmi: 
lexer.cmi: parser.cmi 
location.cmi: 
parser.cmi: syntax.cmo 
builtins.cmo: modules.cmo globals.cmo const.cmo 
builtins.cmx: modules.cmx globals.cmx const.cmx 
clauses.cmo: types.cmo syntax.cmo misc.cmo location.cmi lambda.cmo \
    globals.cmo error.cmo const.cmo 
clauses.cmx: types.cmx syntax.cmx misc.cmx location.cmx lambda.cmx \
    globals.cmx error.cmx const.cmx 
config.cmo: config.cmi 
config.cmx: config.cmi 
const.cmo: misc.cmo 
const.cmx: misc.cmx 
error.cmo: types.cmo syntax.cmo pr_type.cmo misc.cmo location.cmi \
    interntl.cmo globals.cmo const.cmo 
error.cmx: types.cmx syntax.cmx pr_type.cmx misc.cmx location.cmx \
    interntl.cmx globals.cmx const.cmx 
event.cmo: syntax.cmo modules.cmo location.cmi lambda.cmo 
event.cmx: syntax.cmx modules.cmx location.cmx lambda.cmx 
globals.cmo: prim.cmo const.cmo 
globals.cmx: prim.cmx const.cmx 
interntl.cmo: misc.cmo 
interntl.cmx: misc.cmx 
lambda.cmo: prim.cmo globals.cmo const.cmo 
lambda.cmx: prim.cmx globals.cmx const.cmx 
lexer.cmo: parser.cmi lexer.cmi 
lexer.cmx: parser.cmx lexer.cmi 
location.cmo: interntl.cmo config.cmi location.cmi 
location.cmx: interntl.cmx config.cmx location.cmi 
matching.cmo: types.cmo syntax.cmo prim.cmo misc.cmo location.cmi lambda.cmo \
    globals.cmo error.cmo const.cmo clauses.cmo builtins.cmo 
matching.cmx: types.cmx syntax.cmx prim.cmx misc.cmx location.cmx lambda.cmx \
    globals.cmx error.cmx const.cmx clauses.cmx builtins.cmx 
misc.cmo: 
misc.cmx: 
modules.cmo: misc.cmo interntl.cmo globals.cmo const.cmo 
modules.cmx: misc.cmx interntl.cmx globals.cmx const.cmx 
par_aux.cmo: syntax.cmo modules.cmo misc.cmo location.cmi globals.cmo \
    error.cmo const.cmo builtins.cmo 
par_aux.cmx: syntax.cmx modules.cmx misc.cmx location.cmx globals.cmx \
    error.cmx const.cmx builtins.cmx 
parser.cmo: syntax.cmo primdecl.cmo par_aux.cmo globals.cmo const.cmo \
    builtins.cmo parser.cmi 
parser.cmx: syntax.cmx primdecl.cmx par_aux.cmx globals.cmx const.cmx \
    builtins.cmx parser.cmi 
pr_decl.cmo: pr_type.cmo misc.cmo globals.cmo const.cmo 
pr_decl.cmx: pr_type.cmx misc.cmx globals.cmx const.cmx 
pr_type.cmo: types.cmo modules.cmo globals.cmo const.cmo 
pr_type.cmx: types.cmx modules.cmx globals.cmx const.cmx 
prim.cmo: const.cmo 
prim.cmx: const.cmx 
primdecl.cmo: prim.cmo globals.cmo 
primdecl.cmx: prim.cmx globals.cmx 
syntax.cmo: location.cmi globals.cmo const.cmo 
syntax.cmx: location.cmx globals.cmx const.cmx 
tr_env.cmo: syntax.cmo prim.cmo misc.cmo lambda.cmo globals.cmo error.cmo \
    const.cmo builtins.cmo 
tr_env.cmx: syntax.cmx prim.cmx misc.cmx lambda.cmx globals.cmx error.cmx \
    const.cmx builtins.cmx 
ty_decl.cmo: typing.cmo types.cmo syntax.cmo modules.cmo globals.cmo \
    error.cmo const.cmo builtins.cmo 
ty_decl.cmx: typing.cmx types.cmx syntax.cmx modules.cmx globals.cmx \
    error.cmx const.cmx builtins.cmx 
ty_intf.cmo: types.cmo ty_decl.cmo modules.cmo globals.cmo error.cmo \
    const.cmo 
ty_intf.cmx: types.cmx ty_decl.cmx modules.cmx globals.cmx error.cmx \
    const.cmx 
types.cmo: modules.cmo misc.cmo globals.cmo const.cmo 
types.cmx: modules.cmx misc.cmx globals.cmx const.cmx 
typing.cmo: types.cmo syntax.cmo modules.cmo misc.cmo globals.cmo error.cmo \
    const.cmo builtins.cmo 
typing.cmx: types.cmx syntax.cmx modules.cmx misc.cmx globals.cmx error.cmx \
    const.cmx builtins.cmx 
