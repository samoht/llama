# Allow a library to be built with OCaml

with-ocaml: $(LIBRARY).cma
.PHONY: with-ocaml

$(LIBRARY).cma: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) -a $(INCLUDES) $^ -o $@

ocamlclean:
	rm -f *.cma *.cmi *.cmo *.a  *.o .ocamldepend $(GENSOURCES)
.PHONY: ocamlclean

ocamldepend: $(GENSOURCES)
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend

.PHONY: ocamldepend

-include .ocamldepend
