.SUFFIXES: .ml .lml .lmo .cmi .cmx .cmi .mll .mly # .mli .lmi

OCAMLC_STRICT=ocamlopt -strict-sequence -warn-error A -nostdlib -g -p
OCAMLDEP=ocamldep -native
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

LLAMARUN=$(BOOTDIR)/llamarun
LLAMAC=$(LLAMARUN) $(BOOTDIR)/llamac -nostdlib
LLAMAOPT=$(LLAMARUN) $(BOOTDIR)/llamaopt -nostdlib
LLAMADEP=$(LLAMARUN) $(BOOTDIR)/llamadep
LLAMALEX=$(LLAMARUN) $(BOOTDIR)/llamalex
LLAMAYACC=$(BOOTDIR)/llamayacc

%.lmi: %.mli
	$(LLAMAC) -c $(INCLUDES) $<
.ml.lmo:
	$(LLAMAC) -c $(INCLUDES) $<
%.cmi: %.mli
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
.ml.cmx:
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
.mll.ml:
	$(LLAMALEX) $<
.mly.ml:
	$(LLAMAYACC) -v $<
.mly.mli:
	$(LLAMAYACC) -v $<

%.p.cmx %.p.o: %.cmx
	cp $*.cmx $*.p.cmx
	cp $*.o $*.p.o
%.p.cmxa: %.cmxa
	cp $^ $@
