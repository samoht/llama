# The standard library

include ../config/Makefile

LLAMAC=../llamac -nostdlib -I .
LLAMADEP=ocamldep.opt
CPP=/usr/bin/cpp -P -Dunix
COMPFLAGS=-g

# The list of all interfaces. Order irrelevant.

STD_INTF= arg.zi char.zi string.zi array.zi \
    fchar.zi filename.zi fstring.zi farray.zi hashtbl.zi \
    lexing.zi list.zi obj.zi parsing.zi printexc.zi \
    sort.zi sys.zi printf.zi stream.zi stack.zi queue.zi random.zi \
    genlex.zi baltree.zi set.zi map.zi gc.zi format.zi pervasives.zi 

# The list of all implementations. In dependency order.

STD_IMPL=pervasives.zo $(OTHERS)
OTHERS=list.zo fstring.zo string.zo fchar.zo \
    char.zo farray.zo array.zo arg.zo \
    filename.zo hashtbl.zo lexing.zo parsing.zo printexc.zo sort.zo \
    printf.zo stream.zo stack.zo queue.zo format.zo random.zo \
    genlex.zo baltree.zo set.zo map.zo gc.zo

all: $(STD_INTF) stdlib.zo header

stdlib.zo: $(STD_IMPL)
	$(LLAMAC) -a -o stdlib.zo $(STD_IMPL)

header: ../config/Makefile
	echo '#!$(BINDIR)/llamarun' > header

clean:
	rm -f *.zi *.zix *.zo
	rm -f filename.ml

install:
	cp -f stdlib.zo *.zi *.zix *.mli *.ml $(LIBDIR)
#	rm $(LIBDIR)/iparsing.mli

pervasives.zi: pervasives.mli
	$(LLAMAC) -nopervasives -c $(COMPFLAGS) $<
pervasives.zo: pervasives.ml
	$(LLAMAC) -nopervasives -c $(COMPFLAGS) $<
$(OTHERS:.zo=.zi): pervasives.zi

%.zi: %.mli
	$(LLAMAC) -c $(COMPFLAGS) $<
%.zo: %.ml
	$(LLAMAC) -c $(COMPFLAGS) $<
%.ml: %.mlp
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@

filename.ml: filename.mlp

depend: filename.ml
	mv genlex.ml genlex.ml.bak
	$(LLAMADEP) *.mli *.ml > .depend
	mv genlex.ml.bak genlex.ml
	sed -i '' -e 's/cm/z/g' .depend
	echo "genlex.zo: genlex.zi fchar.zi pervasives.zi fstring.zi stream.zi hashtbl.zi list.zi" >> .depend

.PHONY: depend

-include .depend
