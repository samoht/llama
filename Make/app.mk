# Main targets for a directory containing a single executable

.PHONY: install clean depend

$(BINARY): $(MODULES:%=%.lmo)
	$(LLAMAC) $(INCLUDES) $(BYTELINKFLAGS) $(LIBRARIES:%=%.lma) $^ -o $@

install:
	cp $(BINARY) $(BINDIR)

clean:
	rm -f $(BINARY) *.lmi *.lml *.lmo *.lmx $(GENSOURCES)

depend:
	$(LLAMADEP) $(INCLUDES) *.ml *.mli > .depend

-include .depend
