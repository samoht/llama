# Allow a library to be built with OCaml

with-ocaml: $(LIBRARY).cma
.PHONY: with-ocaml

$(LIBRARY).cma: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) -a $(INCLUDES) $^ -o $@

ocamlclean:
	rm -f $(LIBRARY).cma *.cm{i,o} .ocamldepend
.PHONY: ocamlclean

.ocamldepend: $(GENSOURCES)
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend
ocamldepend: .ocamldepend
	@
.PHONY: ocamldepend .ocamldepend

-include .ocamldepend
