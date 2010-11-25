# Allow an executable to be built with OCaml

with-ocaml: $(BINARY)-ocaml
.PHONY: with-ocaml

$(BINARY)-ocaml: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) $(INCLUDES) $(LIBRARIES:%=%.cma) $^ -o $@

ocamlinstall:
	cp $(BINARY)-ocaml $(BINDIR)/$(BINARY)
.PHONY: ocamlinstall

ocamlclean:
	rm -f $(BINARY)-ocaml *.cm{i,o} .ocamldepend
.PHONY: ocamlclean

ocamldepend: $(GENSOURCES)
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend

.PHONY: ocamldepend

-include .ocamldepend