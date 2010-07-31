all: calc

lexer.ml: lexer.mll
	ocamllex $<

parser.ml parser.mli: parser.mly
	ocamlyacc $<

parser.cmi: parser.mli
	llamac -c $<

%.cmo: %.ml
	llamac -c $<

calc: lexer.cmo parser.cmo calc.cmo
	llamac -o $@ $^

parser.zo: parser.cmi
lexer.zo: parser.cmi
calc.zo: parser.cmi lexer.zo

parser.cmo: parser.cmi
lexer.cmo: parser.cmi
calc.cmo: parser.cmi lexer.cmo

clean:
	rm -f lexer.ml parser.ml{,i} *.{cmi,cmo} calc
.PHONY: clean
