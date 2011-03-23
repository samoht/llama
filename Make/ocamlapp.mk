# Allow an executable to be built with OCaml

OCAML_RUNTIME_PATH=$(shell ocamlc -where)
OCAML_CCLIBS=-lcurses	-ltermcap -lm -ldl

with-ocaml: $(BINARY)-ocaml
.PHONY: with-ocaml

$(BINARY)-ocaml: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) -nopervasives -linkall -custom \
	  $(OCAML_CCLIBS:%=-cclib %) \
	  -ccopt -L$(OCAML_RUNTIME_PATH) -cclib -lcamlrun \
	  -ccopt -L$(RUNTIME_PATH) -cclib -lllamarun \
	  -I $(RUNTIME_PATH) $(INCLUDES) $(LIBRARIES:%=%.cma) $^ -o $@


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