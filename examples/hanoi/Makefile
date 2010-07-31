LLAMAC=../../llamac

all: hanoi
.PHONY: all

hanoi: hanoi.cmo
	$(LLAMAC) -o $@ $^

clean:
	rm -f hanoi
	rm -f *.cmi *.cmo

.SUFFIXES: .mli .cmi .ml .cmo

.mli.cmi:
	$(LLAMAC) -c $<

.ml.cmo:
	$(LLAMAC) -c $<

# Generated dependencies below

loadall.cmo: hanoi.cmo 
