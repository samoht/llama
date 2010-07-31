LLAMAC=../../llamac

all: kb
.PHONY: all

kb: prelude.cmo terms.cmo equation.cmo order.cmo kb.cmo go.cmo
	$(LLAMAC) -o $@ $^

clean:
	rm -f kb
	rm -f *.cmi *.cmo

.SUFFIXES: .mli .cmi .ml .cmo

.mli.cmi:
	$(LLAMAC) -c $<

.ml.cmo:
	$(LLAMAC) -c $<

# Generated dependencies below

equation.cmo: terms.cmo prelude.cmo 
go.cmo: order.cmo terms.cmo kb.cmo 
kb.cmo: terms.cmo equation.cmo prelude.cmo 
order.cmo: terms.cmo 
