# The standard library

include ../config/Makefile

ZEBRAC=../zebrac -nopervasives -I .
ZEBRA_DEPEND=../zebradep
CPP=/usr/bin/cpp -P -Dunix
COMPFLAGS=-g

# The list of all interfaces. Order irrelevant.

STD_INTF= arg.zi bool.zi char.zi string.zi vect.zi eq.zi exc.zi \
    fchar.zi filename.zi float.zi fstring.zi fvect.zi hashtbl.zi \
    int.zi io.zi lexing.zi list.zi obj.zi pair.zi parsing.zi printexc.zi \
    ref.zi sort.zi sys.zi printf.zi stream.zi stack.zi queue.zi random.zi \
    genlex.zi baltree.zi set.zi map.zi gc.zi format.zi

# The list of all implementations. In dependency order.

STD_IMPL= bool.zo eq.zo exc.zo int.zo fstring.zo string.zo fchar.zo \
    char.zo float.zo list.zo fvect.zo vect.zo io.zo pair.zo arg.zo \
    filename.zo hashtbl.zo lexing.zo parsing.zo printexc.zo sort.zo \
    printf.zo stream.zo stack.zo queue.zo format.zo random.zo genlex.zo \
    baltree.zo set.zo map.zo gc.zo

all: $(STD_INTF) stdlib.zo header

stdlib.zo: $(STD_IMPL)
	$(ZEBRAC) -a -o stdlib.zo $(STD_IMPL)

header: ../config/Makefile
	echo '#!$(BINDIR)/zebrarun' > header

clean:
	rm -f *.zi *.zix *.zo
	rm -f filename.ml genlex.ml

install:
	cp -f stdlib.zo *.zi *.zix *.mli *.ml $(LIBDIR)
	rm $(LIBDIR)/iparsing.mli

%.zi: %.mli
	$(ZEBRAC) -c $(COMPFLAGS) $<

%.zo: %.ml
	$(ZEBRAC) -c $(COMPFLAGS) $<

%.ml: %.mlp
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@

filename.ml: filename.mlp
genlex.ml: genlex.mlp

depend: filename.ml genlex.ml
	$(ZEBRA_DEPEND) *.mli *.ml > .depend
.PHONY: depend

-include .depend
