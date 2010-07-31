LLAMAC=../../llamac

all: docteur
.PHONY: all

docteur: docteur.ml
	$(LLAMAC) -o $@ $^

clean:
	rm -f docteur
	rm -f *.cmi *.cmo
