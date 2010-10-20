.SUFFIXES: .mli .ml .lmi .lml .lmo .lmx .cmi .cmo .mll .mly

OCAMLC_STRICT=ocamlc -strict-sequence -warn-error A -nostdlib
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

LLAMARUN=$(BOOTDIR)/llamarun
LLAMAC=$(LLAMARUN) $(BOOTDIR)/llamac -nostdlib
LLAMAOPT=$(LLAMARUN) $(BOOTDIR)/llamaopt -nostdlib
LLAMADEP=$(LLAMARUN) $(BOOTDIR)/llamadep
LLAMALEX=$(LLAMARUN) $(BOOTDIR)/llamalex
LLAMAYACC=$(BOOTDIR)/llamayacc

.mli.lmi:
	$(LLAMAC) -c $(INCLUDES) $<
.ml.lmo:
	$(LLAMAC) -c $(INCLUDES) $<
.ml.lmx:
	$(LLAMAOPT) -c $(INCLUDES) $<
.mli.cmi:
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
.ml.cmo:
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
.mll.ml:
	$(LLAMALEX) $< || $(OCAMLLEX) $<
.mly.ml:
	$(LLAMAYACC) -v $< || $(OCAMLYACC) -v $<
.mly.mli:
	$(LLAMAYACC) -v $< || $(OCAMLYACC) -v $<
