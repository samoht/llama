# Makefile for the Caml Light toplevel.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
CAMLLIBR=ocamlopt.opt -a
INCLUDES=-I ../compiler -I ../linker
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

OBJS=fmt_type.cmx pr_value.cmx meta.cmx load_phr.cmx do_phr.cmx toplevel.cmx \
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
         ../tools/camldep -I ../compiler -I ../linker meta.mli *.ml) > Makefile
	rm Makefile.bak

version.ml: version.mlp ../version.h
pr_value.ml: pr_value.mlp

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
do_phr.cmx: pr_value.cmx ../compiler/typing.cmx meta.cmx ../compiler/modules.cmx \
    ../compiler/interntl.cmx ../compiler/compiler.cmx ../compiler/const.cmx \
    ../compiler/misc.cmx load_phr.cmx ../compiler/back.cmx \
    ../compiler/front.cmx fmt_type.cmx ../linker/symtable.cmx \
    ../compiler/syntax.cmx ../compiler/ty_decl.cmx ../compiler/types.cmx 
expunge.cmx: ../linker/symtable.cmx ../compiler/const.cmx 
fmt_type.cmx: ../compiler/modules.cmx ../compiler/globals.cmx \
    ../compiler/types.cmx ../compiler/const.cmx 
load_phr.cmx: pr_value.cmx ../compiler/reloc.cmx meta.cmx ../linker/tr_const.cmx \
    ../compiler/interntl.cmx ../compiler/labels.cmx ../compiler/misc.cmx \
    ../linker/symtable.cmx ../linker/patch.cmx ../compiler/instruct.cmx \
    ../compiler/buffcode.cmx ../compiler/emitcode.cmx ../compiler/builtins.cmx \
    ../compiler/opcodes.cmx
pr_value.cmx: ../compiler/modules.cmx ../compiler/const.cmx \
    ../compiler/misc.cmx fmt_type.cmx ../linker/symtable.cmx \
    ../compiler/globals.cmx ../compiler/builtins.cmx ../compiler/types.cmx 
provide.cmx: ../compiler/prim.cmx ../compiler/modules.cmx \
    ../compiler/config.cmi ../compiler/const.cmx ../compiler/misc.cmx \
    ../compiler/globals.cmx 
topinit.cmx: meta.cmi ../compiler/typing.cmx toplevel.cmx \
    ../compiler/modules.cmx ../compiler/interntl.cmx ../compiler/config.cmi \
    ../compiler/misc.cmx version.cmx ../linker/symtable.cmx 
toplevel.cmx: toplevel.cmi meta.cmi ../compiler/modules.cmx \
    ../compiler/emit_phr.cmx ../compiler/misc.cmx ../compiler/event.cmx \
    ../linker/symtable.cmx ../linker/patch.cmx ../compiler/globals.cmx \
    ../compiler/builtins.cmx pr_value.cmx ../compiler/location.cmi \
    ../compiler/interntl.cmx ../compiler/const.cmx ../compiler/compiler.cmx \
    load_phr.cmx do_phr.cmx ../compiler/types.cmx ../compiler/opcodes.cmx 
topmain.cmx: ../compiler/location.cmi ../compiler/interntl.cmx \
    ../compiler/config.cmi ../compiler/compiler.cmx ../compiler/misc.cmx \
    do_phr.cmx 
version.cmx: ../compiler/interntl.cmx 
