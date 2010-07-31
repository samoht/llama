LLAMAC=../../llamac

all: fib wc sieve
.PHONY: all

fib: fib.cmo
	$(LLAMAC) -o $@ $^

wc: wc.cmo
	$(LLAMAC) -o $@ $^

sieve: sieve.cmo
	$(LLAMAC) -o $@ $^

%.cmi: %.mli
	$(LLAMAC) -c $<

%.cmo: %.ml
	$(LLAMAC) -c $<

clean:
	rm -f fib wc sieve
	rm -f *.cmi *.cmo
.PHONY: clean
