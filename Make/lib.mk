# Main targets for a directory containing a single library

default: $(LIBDEPS) $(LIBRARY).lma
.PHONY: default

$(LIBRARY).lma: $(MODULES:%=%.lmo)
	$(LLAMAC) -a $(INCLUDES) $(LIBRARIES:%=%.lma) $^ -o $@

clean:
	rm -f $(LIBRARY).lma *.lm{i,l,o,x}
.PHONY: clean

scrapeclean:
	rm -f $(GENSOURCES)
.PHONY: scrapeclean

depend: $(GENSOURCES)
	$(LLAMADEP) $(INCLUDES) *.ml *.mli > .depend
.PHONY: depend

include .depend
