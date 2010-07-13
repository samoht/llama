# Makefile for the Caml Light librarian.

CAMLCOMP=ocamlopt.opt -c
CAMLLINK=ocamlopt.opt
INCLUDES=-I ../compiler -I ../linker
COMPFLAGS=-g $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)
CPP=/usr/bin/cpp -P -Dunix

EXTERNOBJS=config.cmx misc.cmx interntl.cmx readword.cmx

OBJS=librar.cmx version.cmx main.cmx

GENSOURCES=version.ml

all: camllibr

camllibr: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllibr $(EXTERNOBJS) $(OBJS)

clean:
	rm -f *.cmi *.cmx *.o camllibr
	rm -f $(GENSOURCES)

install:
	cat $(LIBDIR)/header camllibr > $(BINDIR)/camllibr
	chmod a+x $(BINDIR)/camllibr

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
         ../tools/camldep -I ../compiler -I ../linker *.ml) > Makefile
	rm Makefile.bak

version.ml: version.mlp ../version.h

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
librar.cmx: ../compiler/emit_phr.cmx ../compiler/misc.cmx \
    ../compiler/interntl.cmx 
main.cmx: librar.cmx version.cmx ../compiler/misc.cmx ../linker/readword.cmx \
    ../compiler/interntl.cmx ../compiler/config.cmi 
version.cmx: ../compiler/interntl.cmx 
