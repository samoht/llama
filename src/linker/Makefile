# Makefile for the Caml Light linker.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
INCLUDES=-I ../compiler
COMPFLAGS=-g $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)
CPP=/usr/bin/cpp -P -Dunix

EXTERNOBJS=config.cmx misc.cmx interntl.cmx opcodes.cmx caml_light_extern.o

OBJS=predef.cmx prim_c.cmx symtable.cmx patch.cmx tr_const.cmx link.cmx \
    readword.cmx version.cmx main.cmx

GENSOURCES=version.ml prim_c.ml predef.ml

all: camllink

camllink: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllink $(EXTERNOBJS) $(OBJS)

clean:
	rm -f *.cmi *.cmx *.o camllink
	rm -f $(GENSOURCES)

install:
	cp camllink $(LIBDIR)/camllink

prim_c.ml : ../runtime/primitives
	(echo 'let primitives_table = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' ../runtime/primitives; \
	 echo '|];;') > prim_c.ml

predef.ml : ../runtime/globals.h ../runtime/fail.h
	(echo 'open Const;;'; \
         echo 'let predef_variables = ['; \
	 sed -n -e 's|.*/\* \(".*"\), *\(".*"\) \*/$$|{qual=\1; id=\2};|p' \
                ../runtime/globals.h \
           | sed -e '$$s|;$$||'; \
         echo '];;'; \
         echo 'let predef_exn = ['; \
         sed -n -e 's|.*/\* \(".*"\), *\(".*"\), *\([0-9]*\) \*/$$|({qual=\1; id=\2}, \3);|p' \
                ../runtime/fail.h \
           | sed -e '$$s|;$$||'; \
         echo '];;') > predef.ml

caml_light_extern.o: caml_light_extern.c
	$(CAMLCOMP) $<

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
         ../tools/camldep -I ../compiler *.ml) > Makefile
	rm Makefile.bak

version.ml: version.mlp ../version.h

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
link.cmx: ../compiler/reloc.cmx ../compiler/lambda.cmx tr_const.cmx \
    ../compiler/interntl.cmx ../compiler/config.cmi ../compiler/const.cmx \
    ../compiler/misc.cmx ../compiler/emit_phr.cmx symtable.cmx patch.cmx \
    ../compiler/opcodes.cmx 
main.cmx: link.cmx ../compiler/interntl.cmx ../compiler/config.cmi \
    ../compiler/misc.cmx version.cmx symtable.cmx readword.cmx 
patch.cmx: symtable.cmx ../compiler/reloc.cmx 
predef.cmx: ../compiler/const.cmx 
symtable.cmx: predef.cmx ../compiler/misc.cmx prim_c.cmx \
    ../compiler/interntl.cmx ../compiler/const.cmx 
tr_const.cmx: symtable.cmx ../compiler/const.cmx 
version.cmx: ../compiler/interntl.cmx 
