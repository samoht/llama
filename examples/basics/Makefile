all: fib wc sieve
.PHONY: all

fib: fib.cmo
	llamac -o $@ $^

wc: wc.cmo
	llamac -o $@ $^

sieve: sieve.cmo
	llamac -o $@ $^

%.cmi: %.mli
	llamac -c $<

%.cmo: %.ml
	llamac -c $<

clean:
	rm -f fib wc sieve
	rm -f *.cmi *.cmo
.PHONY: clean
