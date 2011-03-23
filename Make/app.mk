# Main targets for a directory containing a single executable

$(BINARY): $(MODULES:%=%.lmo)
	$(LLAMAC) $(INCLUDES) $(BYTELINKFLAGS) stdlib.lma $(LIBRARIES:%=%.lma) $^ -o $@

install:
	cp $(BINARY) $(BINDIR)
.PHONY: install

clean:
	rm -f $(BINARY) *.lmi *.lml *.lmo *.lmx depend $(GENSOURCES)
.PHONY: clean

scrapeclean: clean
	rm -f $(GENSOURCES)
.PHONY: scrapeclean

depend: $(GENSOURCES)
	$(LLAMADEP) $(INCLUDES) *.ml *.mli > depend

.PHONY: depend

-include .depend
