include ../config/Makefile
LLAMAC=../boot/llamac -nostdlib -I .
LLAMADEP=../boot/llamadep

OBJS=pervasives.cmo $(OTHERS)
OTHERS=array.cmo list.cmo char.cmo string.cmo sys.cmo \
  hashtbl.cmo sort.cmo marshal.cmo obj.cmo \
  int32.cmo int64.cmo nativeint.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo \
  stack.cmo queue.cmo \
  lazy.cmo stream.cmo \
  buffer.cmo printf_sformat.cmo printf_tformat.cmo printf.cmo format.cmo \
  arg.cmo printexc.cmo gc.cmo \
  digest.cmo random_state.cmo random.cmo callback.cmo \
  genlex.cmo weak.cmo \
  filename.cmo complex.cmo scanning.cmo scanf.cmo

all: std_exit.cmo stdlib.cma camlheader

stdlib.cma: $(OBJS)
	$(LLAMAC) -a -o stdlib.cma $^

sys.ml: sys.mlp ../VERSION
	sed -e "s|%%VERSION%%|`sed -e 1q ../VERSION`|" sys.mlp >sys.ml

%.cmi: %.mli
	$(LLAMAC) -c $<
%.cmo: %.ml
	$(LLAMAC) -c $<
pervasives.cmi: pervasives.mli
	$(LLAMAC) -nopervasives -c $<
pervasives.cmo: pervasives.ml
	$(LLAMAC) -nopervasives -c $<

std_exit.cmo: pervasives.cmo

camlheader: ../config/Makefile
	echo '#!$(BINDIR)/llamarun' > $@

clean:
	rm -f *.cmi *.cmo
.PHONY: clean

depend: sys.ml
	$(LLAMADEP) *.mli *.ml > .depend

include .depend
