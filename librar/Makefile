# Makefile for the Caml Light librarian.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
INCLUDES=-I ../utils -I ../typing -I ../compiler -I ../linker
COMPFLAGS=-g $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)
CPP=/usr/bin/cpp -P -Dunix

EXTERNOBJS=config.cmx misc.cmx interntl.cmx readword.cmx version.cmx

OBJS=librar.cmx main.cmx

GENSOURCES=

all: camllibr

camllibr: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllibr $(EXTERNOBJS) $(OBJS)

clean:
	rm -f *.cmi *.cmx *.o camllibr
	rm -f $(GENSOURCES)

install:
#	cat $(LIBDIR)/header camllibr > $(BINDIR)/darklibr
#	chmod a+x $(BINDIR)/darklibr
	cp camllibr $(BINDIR)/darklibr

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
librar.cmo: ../typing/misc.cmo ../typing/interntl.cmo \
    ../compiler/emit_phr.cmo 
librar.cmx: ../typing/misc.cmx ../typing/interntl.cmx \
    ../compiler/emit_phr.cmx 
main.cmo: ../linker/readword.cmo ../typing/misc.cmo librar.cmo \
    ../typing/interntl.cmo ../typing/config.cmi 
main.cmx: ../linker/readword.cmx ../typing/misc.cmx librar.cmx \
    ../typing/interntl.cmx ../typing/config.cmx 
