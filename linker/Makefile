# Makefile for the Caml Light linker.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
INCLUDES=-I ../utils -I ../typing -I ../compiler
COMPFLAGS=-g $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)
CPP=/usr/bin/cpp -P -Dunix

EXTERNOBJS=config.cmx misc.cmx interntl.cmx opcodes.cmx version.cmx

OBJS=caml_light_extern.o \
    predef.cmx prim_c.cmx symtable.cmx patch.cmx tr_const.cmx link.cmx \
    readword.cmx main.cmx

GENSOURCES=prim_c.ml predef.ml

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
         ocamldep.opt $(INCLUDES) *.ml) > Makefile
	rm Makefile.bak

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
link.cmo: tr_const.cmo symtable.cmo ../compiler/reloc.cmo patch.cmo \
    ../compiler/opcodes.cmo ../typing/misc.cmo ../typing/lambda.cmo \
    ../typing/interntl.cmo ../compiler/emit_phr.cmo ../typing/const.cmo \
    ../typing/config.cmi 
link.cmx: tr_const.cmx symtable.cmx ../compiler/reloc.cmx patch.cmx \
    ../compiler/opcodes.cmx ../typing/misc.cmx ../typing/lambda.cmx \
    ../typing/interntl.cmx ../compiler/emit_phr.cmx ../typing/const.cmx \
    ../typing/config.cmx 
main.cmo: version.cmo symtable.cmo readword.cmo ../typing/misc.cmo link.cmo \
    ../typing/interntl.cmo ../typing/config.cmi 
main.cmx: version.cmx symtable.cmx readword.cmx ../typing/misc.cmx link.cmx \
    ../typing/interntl.cmx ../typing/config.cmx 
patch.cmo: symtable.cmo ../compiler/reloc.cmo 
patch.cmx: symtable.cmx ../compiler/reloc.cmx 
predef.cmo: ../typing/const.cmo 
predef.cmx: ../typing/const.cmx 
prim_c.cmo: 
prim_c.cmx: 
readword.cmo: 
readword.cmx: 
symtable.cmo: prim_c.cmo predef.cmo ../typing/misc.cmo ../typing/interntl.cmo \
    ../typing/const.cmo 
symtable.cmx: prim_c.cmx predef.cmx ../typing/misc.cmx ../typing/interntl.cmx \
    ../typing/const.cmx 
tr_const.cmo: symtable.cmo ../typing/const.cmo 
tr_const.cmx: symtable.cmx ../typing/const.cmx 
version.cmo: ../typing/interntl.cmo 
version.cmx: ../typing/interntl.cmx 
