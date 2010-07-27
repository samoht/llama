OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt

INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp
FLAGS=-g $(INCLUDES)

SRC=pparse.cmx errors.cmx compile.cmx main_args.cmx main.cmx

all: $(SRC)

%.cmx: %.ml
	$(OCAMLOPT) -c $(FLAGS) -o $@ $<

%.cmi: %.mli
	$(OCAMLOPT) -c $(FLAGS) -o $@ $<

depend:
	$(OCAMLDEP) -native $(INCLUDES) *.{mli,ml} > .depend
.PHONY: depend

clean:
	rm *.{cmi,cmx,o}
.PHONY: clean

include .depend
