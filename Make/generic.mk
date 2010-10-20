.SUFFIXES: .mli .ml .lmi .lml .lmo .lmx .cmi .cmo .mll .mly

OCAMLC_STRICT=ocamlc -strict-sequence -warn-error A
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

LLAMARUN=$(BOOTDIR)/llamarun
LLAMAC=$(LLAMARUN) $(BOOTDIR)/llamac
LLAMAOPT=$(LLAMARUN) $(BOOTDIR)/llamaopt
LLAMADEP=$(LLAMARUN) $(BOOTDIR)/llamadep
LLAMALEX=$(LLAMARUN) $(BOOTDIR)/llamalex
LLAMAYACC=$(BOOTDIR)/llamayacc

.mli.lmi:
	$(LLAMAC) -c -g $(INCLUDES) $<
.ml.lmo:
	$(LLAMAC) -c -g $(INCLUDES) $<
.ml.lmx:
	$(LLAMAOPT) -c -g $(INCLUDES) $<
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
