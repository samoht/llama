# Allow an executable to be built with OCaml
OCAML_RUNTIME_PATH=$(shell ocamlc -where)
OCAML_CCLIBS=-lcurses -ltermcap -lm -ldl -lthreads

.PHONY: with-ocaml ocamlinstall ocamlclean ocamldepend

with-ocaml: $(BINARY)-ocaml

$(BINARY)-ocaml: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) -nopervasives -linkall -custom \
	  $(OCAML_CCLIBS:%=-cclib %) \
	  -ccopt -L$(OCAML_RUNTIME_PATH) -cclib -lcamlrun \
	  -ccopt -L$(RUNTIME_PATH) -cclib -lllamarun \
	  -I $(RUNTIME_PATH) $(INCLUDES) $(LIBRARIES:%=%.cma) $^ -o $@

ocamlinstall:
	cp $(BINARY)-ocaml $(BINDIR)/$(BINARY)

ocamlclean:
	rm -f $(BINARY)-ocaml *.cmi *.cmo *.a *.o *.cma $(GENSOURCES) 

ocamldepend:
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .ocamldepend

-include .ocamldepend
