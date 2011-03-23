.SUFFIXES: .ml .lml .lmo .cmi .cmo .cmi .mll .mly # .mli .lmi

OCAMLC_STRICT=ocamlc -strict-sequence -warn-error A -nostdlib -g
OCAMLCP_STRICT=ocamlcp -strict-sequence -warn-error A -nostdlib -g -p a
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

LLAMARUN=$(BOOTDIR)/llamarun
LLAMAC=$(LLAMARUN) $(BOOTDIR)/llamac -nostdlib

LLAMADEP=$(LLAMARUN) $(BOOTDIR)/llamadep
LLAMALEX=$(LLAMARUN) $(BOOTDIR)/llamalex
LLAMAYACC=$(BOOTDIR)/llamayacc

%.lmi: %.mli
	$(LLAMAC) -c $(INCLUDES) $<
%.lmo: %.ml
	$(LLAMAC) -c $(INCLUDES) $<
%.cmi: %.mli
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
%.cmo: %.ml
	$(OCAMLC_STRICT) -c $(INCLUDES) $<
%.ml: %.mll
	$(LLAMALEX) $<
%.ml %.mli: %.mly
	$(LLAMAYACC) -v $<
