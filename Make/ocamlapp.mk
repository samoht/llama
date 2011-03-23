# Allow an executable to be built with OCaml
OCAML_INCLUDE=$(shell ocamlc -where)
OCAML_CCLIBS=-lcurses	-ltermcap -lm -ldl

with-ocaml: $(BINARY)-ocaml
.PHONY: with-ocaml

$(BINARY)-ocaml: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) -nopervasives -custom \
	  $(OCAML_CCLIBS:%=-cclib %) \
	  -ccopt -L$(RUNTIME_PATH) -cclib -lcamlrun \
	  -I $(RUNTIME_PATH) $(INCLUDES) stdlib.cma profiling.cma \
	  $(LIBRARIES:%=%.cma) $^ -o $@

ocamlinstall:
	cp $(BINARY)-ocaml $(BINDIR)/$(BINARY)
.PHONY: ocamlinstall

ocamlclean:
	rm -f $(BINARY)-ocaml *.cmi *.cmo *.a *.o *.cma .ocamldepend $(GENSOURCES) 
.PHONY: ocamlclean

ocamldepend: $(GENSOURCES)
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend

.PHONY: ocamldepend

-include .ocamldepend