# The standard library

include ../config/Makefile

LLAMAC=../llamac -nostdlib -I .
LLAMADEP=ocamldep.opt
CPP=/usr/bin/cpp -P -Dunix
COMPFLAGS=-g

# The list of all interfaces. Order irrelevant.

STD_INTF= arg.lli char.lli string.lli array.lli \
    filename.lli fstring.lli farray.lli hashtbl.lli \
    lexing.lli list.lli obj.lli parsing.lli printexc.lli \
    sort.lli sys.lli printf.lli stream.lli stack.lli queue.lli random.lli \
    genlex.lli baltree.lli set.lli map.lli gc.lli pervasives.lli  buffer.lli format.lli

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
	rm -f *.lli *.zo
	rm -f filename.ml

install:
	cp -f stdlib.zo *.lli *.mli *.ml $(LIBDIR)
#	rm $(LIBDIR)/iparsing.mli

pervasives.lli: pervasives.mli
	$(LLAMAC) -nopervasives -c $(COMPFLAGS) $<
pervasives.zo: pervasives.ml
	$(LLAMAC) -nopervasives -c $(COMPFLAGS) $<
$(OTHERS:.zo=.lli): pervasives.lli

%.lli: %.mli
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
	echo "genlex.zo: genlex.lli pervasives.lli fstring.lli stream.lli hashtbl.lli list.lli" >> .depend

.PHONY: depend

-include .depend
