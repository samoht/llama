include ../config/Makefile
LLAMAC=../llamac-new -nostdlib -I .

OBJS=pervasives.cmo $(OTHERS)
OTHERS=array.cmo list.cmo char.cmo string.cmo sys.cmo \
  hashtbl.cmo sort.cmo marshal.cmo obj.cmo \
  int32.cmo int64.cmo nativeint.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo \
  camlinternalLazy.cmo lazy.cmo stream.cmo \
  buffer.cmo printf.cmo format.cmo scanf.cmo \
  arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo callback.cmo \
  camlinternalOO.cmo oo.cmo camlinternalMod.cmo \
  genlex.cmo weak.cmo \
  filename.cmo complex.cmo \
  arrayLabels.cmo listLabels.cmo stringLabels.cmo moreLabels.cmo stdLabels.cmo

all: std_exit.cmo stdlib.cma

stdlib.cma: $(OBJS)
	$(LLAMAC) -a -o stdlib.cma $^

test2: test2.ml
	../llamac-new $^ && ocamlrun a.out

%.lli: %.mli
	$(LLAMAC) -c $<
%.cmo: %.ml
	$(LLAMAC) -c $<
pervasives.lli: pervasives.mli
	$(LLAMAC) -nopervasives -c $<
pervasives.cmo: pervasives.ml
	$(LLAMAC) -nopervasives -c $<

std_exit.cmo: pervasives.cmo
stream.cmo: lazy.cmo

clean:
	rm -f *.lli *.cmo
.PHONY: clean

include .depend
