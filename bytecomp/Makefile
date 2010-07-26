OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt

INCLUDES=-I ../utils -I ../parsing -I ../typing
FLAGS=-g $(INCLUDES)

SRC=lambda.cmx printlambda.cmx \
  typeopt.cmx switch.cmx matching.cmx \
  translcore.cmx \
  translmod.cmx \
  simplif.cmx runtimedef.cmx \
  meta.cmx instruct.cmx bytegen.cmx \
  printinstr.cmx opcodes.cmx emitcode.cmx \
  bytesections.cmx dll.cmx symtable.cmx \
  bytelink.cmx bytelibrarian.cmx bytepackager.cmx

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
