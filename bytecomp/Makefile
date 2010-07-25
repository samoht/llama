OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt

INCLUDES=-I ../utils -I ../parsing -I ../typing
FLAGS=-g $(INCLUDES)

SRC=lambda.cmo printlambda.cmo \
  typeopt.cmo switch.cmo matching.cmo \
  translobj.cmo translcore.cmo \
  translclass.cmo translmod.cmo \
  simplif.cmo runtimedef.cmo \
  meta.cmo instruct.cmo bytegen.cmo \
  printinstr.cmo opcodes.cmo emitcode.cmo \
  bytesections.cmo dll.cmo symtable.cmo \
  bytelink.cmo bytelibrarian.cmo bytepackager.cmo

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
