all: calc calcd

lexer.ml: lexer.mll
	ocamllex $<

parser.ml parser.mli: parser.mly
	ocamlyacc $<

parser.cmi: parser.mli
	ocamlc -c $<

parser.cmi: parser.mli
	llamac -c $<

%.cmo: %.ml
	ocamlc -c $<

%.zo: %.ml
	llamac -c $<

calc: lexer.zo parser.zo calc.zo
	llamac -o $@ $^

calcd: lexer.cmo parser.cmo calc.cmo
	ocamlc -o $@ $^

parser.zo: parser.cmi
lexer.zo: parser.cmi
calc.zo: parser.cmi lexer.zo

parser.cmo: parser.cmi
lexer.cmo: parser.cmi
calc.cmo: parser.cmi lexer.cmo

clean:
	rm -f lexer.ml parser.ml{,i} *.{lli,zo,cmi,cmo} calc{,d}
.PHONY: clean
