# Allow a library to be built with OCaml

PHONY: with-ocaml ocamlclean ocamldepend

with-ocaml: $(LIBRARY).cma

$(LIBRARY).cma: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) -a $(INCLUDES) $^ -o $@

ocamlclean:
	rm -f *.cma *.cmi *.cmo *.a  *.o $(GENSOURCES)

ocamldepend:
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend

-include .ocamldepend
