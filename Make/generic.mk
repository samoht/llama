.SUFFIXES: .ml .lml .lmo .cmi .cmo .cmi .mll .mly .mli .lmi
.PRECIOUS: %.lmi %.lmo %.cmi %.cmo

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

%.lmi: %.mli %.ml
	$(LLAMAC) -c $(INCLUDES) $<
%.lmo: %.ml %.lmi
	$(LLAMAC) -c $(INCLUDES) $<
%.lmi %.lmo: %.ml
	$(LLAMAC) -c $(INCLUDES) $<

%.cmi: %.mli %.ml
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
%.cmo: %.ml %.cmi
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
%.cmi %.cmo: %.ml
	$(OCAMLC_STRICT) -c $(INCLUDES) $<

%.ml: %.mll
	$(LLAMALEX) $<
%.mli %.ml: %.mly
	$(LLAMAYACC) -v $<
