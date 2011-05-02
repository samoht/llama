# Main targets for a directory containing a single library

.PHONY: default clean depend

default: $(LIBDEPS) $(LIBRARY).lma

$(LIBRARY).lma: $(MODULES:%=%.lmo)
	$(LLAMAC) -a $(INCLUDES) $(LIBRARIES:%=%.lma) $^ -o $@

clean:
	rm -f $(LIBRARY).lma *.lmi *.lml *.lmo *.lmx $(GENSOURCES)

depend:
	$(LLAMADEP) $(INCLUDES) *.ml *.mli > .depend

-include .depend
