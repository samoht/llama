include ../config/Makefile
LLAMAC=../boot/llamac -nostdlib -I .
LLAMADEP=ocamldep.opt

OBJS=pervasives.cmo $(OTHERS)
OTHERS=array.cmo list.cmo char.cmo string.cmo sys.cmo \
  hashtbl.cmo sort.cmo marshal.cmo obj.cmo \
  int32.cmo int64.cmo nativeint.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo \
  lazy.cmo stream.cmo \
  buffer.cmo printf_sformat.cmo printf_tformat.cmo printf.cmo format.cmo \
  arg.cmo printexc.cmo gc.cmo \
  digest.cmo random_state.cmo random.cmo callback.cmo \
  genlex.cmo weak.cmo \
  filename.cmo complex.cmo # scanf.cmo

all: std_exit.cmo stdlib.cma camlheader

stdlib.cma: $(OBJS)
	$(LLAMAC) -a -o stdlib.cma $^

test2: test2.ml
	../llamac-new $^ && ocamlrun a.out

sys.ml: sys.mlp ../VERSION
	sed -e "s|%%VERSION%%|`sed -e 1q ../VERSION`|" sys.mlp >sys.ml

%.lli: %.mli
	$(LLAMAC) -c $<
%.cmo: %.ml
	$(LLAMAC) -c $<
pervasives.lli: pervasives.mli
	$(LLAMAC) -nopervasives -c $<
pervasives.cmo: pervasives.ml
	$(LLAMAC) -nopervasives -c $<

std_exit.cmo: pervasives.cmo

camlheader: ../config/Makefile
	echo '#!$(BINDIR)/ocamlrun' > $@

clean:
	rm -f *.lli *.cmo
.PHONY: clean

depend: sys.ml
	$(LLAMADEP) *.mli *.ml > .depend
	sed -i '' -e 's/cmi/lli/g' .depend

include .depend
