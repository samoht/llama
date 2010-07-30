# The standard library

include ../config/Makefile

LLAMAC=../llamac -nostdlib -I .
LLAMADEP=ocamldep.opt
CPP=/usr/bin/cpp -P -Dunix
COMPFLAGS=-g

# The list of all interfaces. Order irrelevant.

STD_INTF= arg.cmi char.cmi string.cmi array.cmi \
    filename.cmi fstring.cmi farray.cmi hashtbl.cmi \
    lexing.cmi list.cmi obj.cmi parsing.cmi printexc.cmi \
    sort.cmi sys.cmi printf.cmi stream.cmi stack.cmi queue.cmi random.cmi \
    genlex.cmi baltree.cmi set.cmi map.cmi gc.cmi pervasives.cmi  buffer.cmi format.cmi

# The list of all implementations. In dependency order.

STD_IMPL=pervasives.zo $(OTHERS)
OTHERS=list.zo fstring.zo \
    char.zo string.zo farray.zo array.zo arg.zo \
    filename.zo hashtbl.zo lexing.zo parsing.zo printexc.zo sort.zo \
    printf.zo stream.zo stack.zo queue.zo random.zo \
    genlex.zo baltree.zo set.zo map.zo gc.zo buffer.zo \
   printf_sformat.zo printf_tformat.zo format.zo

all: $(STD_INTF) stdlib.zo header

stdlib.zo: $(STD_IMPL)
	$(LLAMAC) -a -o stdlib.zo $(STD_IMPL)

header: ../config/Makefile
	echo '#!$(BINDIR)/llamarun' > header

clean:
	rm -f *.cmi *.zo
	rm -f filename.ml

install:
	cp -f stdlib.zo *.cmi *.mli *.ml $(LIBDIR)
#	rm $(LIBDIR)/iparsing.mli

pervasives.cmi: pervasives.mli
	$(LLAMAC) -nopervasives -c $(COMPFLAGS) $<
pervasives.zo: pervasives.ml
	$(LLAMAC) -nopervasives -c $(COMPFLAGS) $<
$(OTHERS:.zo=.cmi): pervasives.cmi

%.cmi: %.mli
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
	echo "genlex.zo: genlex.cmi pervasives.cmi fstring.cmi stream.cmi hashtbl.cmi list.cmi" >> .depend

.PHONY: depend

-include .depend
