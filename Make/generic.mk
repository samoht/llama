.SUFFIXES: .ml .lml .lmo .cmi .cmx .cmi .mll .mly # .mli .lmi

OCAMLC_STRICT=ocamlc -strict-sequence -warn-error A -nostdlib -g
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

LLAMARUN=$(BOOTDIR)/llamarun
LLAMAC=$(LLAMARUN) $(BOOTDIR)/llamac -nostdlib
LLAMAOPT=$(LLAMARUN) $(BOOTDIR)/llamaopt -nostdlib
LLAMADEP=$(LLAMARUN) $(BOOTDIR)/llamadep
LLAMALEX=$(OCAMLLEX)   #$(LLAMARUN) $(BOOTDIR)/llamalex
LLAMAYACC=$(OCAMLYACC) #$(BOOTDIR)/llamayacc

%.lmi: %.mli
	$(LLAMAC) -c $(INCLUDES) $<
.ml.lmo:
	$(LLAMAC) -c $(INCLUDES) $<
%.cmi: %.mli
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
%.cmo: %.ml
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
.mll.ml:
	$(LLAMALEX) $<
.mly.ml:
	$(LLAMAYACC) -v $<
.mly.mli:
	$(LLAMAYACC) -v $<
