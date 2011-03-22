# Allow a library to be built with OCaml

with-ocaml: $(LIBRARY).cmxa
.PHONY: with-ocaml

$(LIBRARY).cmxa: $(MODULES:%=%.cmx)
	$(OCAMLC_STRICT) -a $(INCLUDES) $^ -o $@

ocamlclean:
	rm -f $(LIBRARY).cmxa *.cmi *.cmx *.a *.cmxa *.o .ocamldepend $(GENSOURCES)
.PHONY: ocamlclean

ocamldepend: $(GENSOURCES)
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend

.PHONY: ocamldepend

-include .ocamldepend
