# Makefile for the Caml Light toplevel.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
CAMLLIBR=ocamlopt.opt -a
INCLUDES=-I ../typing -I ../compiler -I ../linker
COMPFLAGS=-g $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)
CPP=/usr/bin/cpp -P -Dunix
CPPFLAGS=

EXTERNOBJS=config.cmx misc.cmx interntl.cmx \
    const.cmx prim.cmx instruct.cmx lambda.cmx \
    globals.cmx location.cmx syntax.cmx modules.cmx builtins.cmx \
    types.cmx pr_type.cmx error.cmx typing.cmx ty_decl.cmx pr_decl.cmx \
    ty_intf.cmx tr_env.cmx event.cmx clauses.cmx matching.cmx trstream.cmx front.cmx \
    back.cmx opcodes.cmx prim_opc.cmx buffcode.cmx labels.cmx \
    reloc.cmx emitcode.cmx emit_phr.cmx \
    primdecl.cmx lexer.cmx par_aux.cmx parser.cmx compiler.cmx \
    predef.cmx prim_c.cmx symtable.cmx patch.cmx tr_const.cmx

OBJS= eval.cmx fmt_type.cmx pr_value.cmx meta.cmx do_phr.cmx toplevel.cmx \
     version.cmx topinit.cmx

PERVASIVES=baltree bool char eq exc fchar filename float format fstring fvect \
    gc genlex hashtbl int io iparsing lexing list map obj pair parsing \
    printexc printf queue random ref set sort stack stream string vect

SPECIALS=sys

GENSOURCES=version.ml pr_value.ml

all: camltop

camltop: toplib.cmxa topmain.cmx provide expunge
	$(CAMLLINK) $(LINKFLAGS) -o camltop \
		$(EXTERNOBJS) $(OBJS) topmain.cmx
#	./provide -stdlib ../lib $(PERVASIVES) > required
#	./expunge camltop1 camltop $(PERVASIVES) $(SPECIALS)
#	rm -f camltop1 required
#	cp toplevel.zi ../lib

toplib.cmxa: $(OBJS)
	$(CAMLLIBR) -o toplib.cmxa $(INCLUDES) $(EXTERNOBJS) $(OBJS)

expunge: expunge.cmx
	$(CAMLLINK) $(LINKFLAGS) -o expunge readword.cmx expunge.cmx

provide: provide.cmx
	$(CAMLLINK) $(LINKFLAGS) -o provide \
                config.cmx misc.cmx interntl.cmx modules.cmx provide.cmx

clean:
	rm -f *.zi *.cmx camltop expunge provide
	rm -f $(GENSOURCES)

install:
	cp camltop $(LIBDIR)/camltop
	cp toplevel.mli toplevel.zi $(LIBDIR)
	cp provide $(LIBDIR)/provide
	cp expunge $(LIBDIR)/expunge
	cp toplib.cmx $(LIBDIR)/toplib.cmx
	cp topmain.cmx $(LIBDIR)/topmain.cmx

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) $(CPPFLAGS) $< > $@
	@chmod a-w $@

depend: $(GENSOURCES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ocamldep.opt $(INCLUDES) meta.mli *.ml) > Makefile
	rm Makefile.bak

version.ml: version.mlp ../version.h
pr_value.ml: pr_value.mlp

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
meta.cmi: 
do_phr.cmo: ../typing/typing.cmo ../typing/types.cmo ../typing/ty_decl.cmo \
    ../typing/syntax.cmo ../linker/symtable.cmo pr_value.cmo \
    ../typing/modules.cmo ../typing/misc.cmo meta.cmi ../typing/interntl.cmo \
    ../compiler/front.cmo fmt_type.cmo eval.cmo ../typing/const.cmo \
    ../compiler/compiler.cmo ../compiler/back.cmo 
do_phr.cmx: ../typing/typing.cmx ../typing/types.cmx ../typing/ty_decl.cmx \
    ../typing/syntax.cmx ../linker/symtable.cmx pr_value.cmx \
    ../typing/modules.cmx ../typing/misc.cmx meta.cmx ../typing/interntl.cmx \
    ../compiler/front.cmx fmt_type.cmx eval.cmx ../typing/const.cmx \
    ../compiler/compiler.cmx ../compiler/back.cmx 
eval.cmo: ../typing/syntax.cmo ../linker/symtable.cmo ../typing/prim.cmo \
    ../typing/location.cmi ../typing/globals.cmo ../typing/const.cmo \
    ../typing/builtins.cmo 
eval.cmx: ../typing/syntax.cmx ../linker/symtable.cmx ../typing/prim.cmx \
    ../typing/location.cmx ../typing/globals.cmx ../typing/const.cmx \
    ../typing/builtins.cmx 
expunge.cmo: ../linker/symtable.cmo ../typing/const.cmo 
expunge.cmx: ../linker/symtable.cmx ../typing/const.cmx 
fmt_type.cmo: ../typing/types.cmo ../typing/modules.cmo ../typing/globals.cmo \
    ../typing/const.cmo 
fmt_type.cmx: ../typing/types.cmx ../typing/modules.cmx ../typing/globals.cmx \
    ../typing/const.cmx 
meta.cmo: meta.cmi 
meta.cmx: meta.cmi 
pr_value.cmo: ../typing/types.cmo ../typing/syntax.cmo ../linker/symtable.cmo \
    ../typing/modules.cmo ../typing/misc.cmo ../typing/globals.cmo \
    fmt_type.cmo eval.cmo ../typing/const.cmo ../typing/builtins.cmo 
pr_value.cmx: ../typing/types.cmx ../typing/syntax.cmx ../linker/symtable.cmx \
    ../typing/modules.cmx ../typing/misc.cmx ../typing/globals.cmx \
    fmt_type.cmx eval.cmx ../typing/const.cmx ../typing/builtins.cmx 
provide.cmo: ../typing/prim.cmo ../typing/modules.cmo ../typing/misc.cmo \
    ../typing/globals.cmo ../typing/const.cmo ../typing/config.cmi 
provide.cmx: ../typing/prim.cmx ../typing/modules.cmx ../typing/misc.cmx \
    ../typing/globals.cmx ../typing/const.cmx ../typing/config.cmx 
topinit.cmo: version.cmo ../typing/typing.cmo toplevel.cmi \
    ../linker/symtable.cmo ../typing/modules.cmo ../typing/misc.cmo \
    ../typing/interntl.cmo ../typing/config.cmi 
topinit.cmx: version.cmx ../typing/typing.cmx toplevel.cmx \
    ../linker/symtable.cmx ../typing/modules.cmx ../typing/misc.cmx \
    ../typing/interntl.cmx ../typing/config.cmx 
toplevel.cmo: ../typing/types.cmo ../linker/symtable.cmo pr_value.cmo \
    ../linker/patch.cmo ../compiler/opcodes.cmo ../typing/modules.cmo \
    ../typing/misc.cmo meta.cmi ../typing/location.cmi ../typing/interntl.cmo \
    ../typing/globals.cmo ../typing/event.cmo ../compiler/emit_phr.cmo \
    do_phr.cmo ../typing/const.cmo ../compiler/compiler.cmo \
    ../typing/builtins.cmo toplevel.cmi 
toplevel.cmx: ../typing/types.cmx ../linker/symtable.cmx pr_value.cmx \
    ../linker/patch.cmx ../compiler/opcodes.cmx ../typing/modules.cmx \
    ../typing/misc.cmx meta.cmx ../typing/location.cmx ../typing/interntl.cmx \
    ../typing/globals.cmx ../typing/event.cmx ../compiler/emit_phr.cmx \
    do_phr.cmx ../typing/const.cmx ../compiler/compiler.cmx \
    ../typing/builtins.cmx toplevel.cmi 
topmain.cmo: ../linker/symtable.cmo ../typing/misc.cmo ../typing/location.cmi \
    ../typing/interntl.cmo do_phr.cmo ../typing/config.cmi \
    ../compiler/compiler.cmo 
topmain.cmx: ../linker/symtable.cmx ../typing/misc.cmx ../typing/location.cmx \
    ../typing/interntl.cmx do_phr.cmx ../typing/config.cmx \
    ../compiler/compiler.cmx 
version.cmo: ../typing/interntl.cmo 
version.cmx: ../typing/interntl.cmx 
