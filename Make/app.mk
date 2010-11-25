# Main targets for a directory containing a single executable

$(BINARY): $(MODULES:%=%.lmo)
	$(LLAMAC) $(INCLUDES) $(BYTELINKFLAGS) $(LIBRARIES:%=%.lma) $^ -o $@

install:
	cp $(BINARY) $(BINDIR)
.PHONY: install

clean:
	rm -f $(BINARY) *.lmi *.lml *.lmo *.lmx
.PHONY: clean

scrapeclean: clean
	rm -f $(GENSOURCES)
.PHONY: scrapeclean

depend: $(GENSOURCES)
	$(LLAMADEP) $(INCLUDES) *.ml *.mli > .depend
.PHONY: depend

include .depend
