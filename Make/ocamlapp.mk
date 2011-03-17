# Allow an executable to be built with OCaml

with-ocaml: $(BINARY)-ocaml
.PHONY: with-ocaml

$(BINARY)-ocaml: $(MODULES:%=%.p.cmx)
	$(OCAMLC_STRICT) -ccopt -L$(RUNTIME_PATH) -cclib -lasmrunp -linkall \
	  -I $(RUNTIME_PATH) $(INCLUDES)  $(LIBRARIES:%=%.p.cmxa) $^ -o $@

ocamlinstall:
	cp $(BINARY)-ocaml $(BINDIR)/$(BINARY)
.PHONY: ocamlinstall

ocamlclean:
	rm -f $(BINARY)-ocaml *.cmi *.cmx .ocamldepend $(GENSOURCES)
.PHONY: ocamlclean

ocamldepend: $(GENSOURCES)
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend

.PHONY: ocamldepend

-include .ocamldepend