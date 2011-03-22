# Allow an executable to be built with OCaml

OCAML_RUNTIME_PATH=$(shell ocamlc -where)
OCAML_CCLIBS=-lcurses	-ltermcap -lm -ldl

with-ocaml: $(BINARY)-ocaml
.PHONY: with-ocaml

$(BINARY)-ocaml: $(MODULES:%=%.cmx)
	$(OCAMLC_STRICT) -nopervasives \
	  $(OCAML_CCLIBS:%=-cclib %) \
	  -ccopt -L$(RUNTIME_PATH) -cclib -lcamlrun \
	  -ccopt -L$(OCAML_RUNTIME_PATH) -cclib -lasmrun \
	  -I $(RUNTIME_PATH) $(INCLUDES) $(LIBRARIES:%=%.cmxa) $^ -o $@

ocamlinstall:
	cp $(BINARY)-ocaml $(BINDIR)/$(BINARY)
.PHONY: ocamlinstall

ocamlclean:
	rm -f $(BINARY)-ocaml *.cmi *.cmx *.a *.o *.cmxa .ocamldepend $(GENSOURCES) 
.PHONY: ocamlclean

ocamldepend: $(GENSOURCES)
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend

.PHONY: ocamldepend

-include .ocamldepend