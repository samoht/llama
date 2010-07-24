all: calc calcd

lexer.ml: lexer.mll
	ocamllex $<

parser.ml parser.mli: parser.mly
	ocamlyacc $<

parser.cmi: parser.mli
	ocamlc -c $<

parser.zi: parser.mli
	llamac -c $<

%.cmo: %.ml
	ocamlc -c $<

%.zo: %.ml
	llamac -c $<

calc: lexer.zo parser.zo calc.zo
	llamac -o $@ $^

calcd: lexer.cmo parser.cmo calc.cmo
	ocamlc -o $@ $^

parser.zo: parser.zi
lexer.zo: parser.zi
calc.zo: parser.zi lexer.zo

parser.cmo: parser.cmi
lexer.cmo: parser.cmi
calc.cmo: parser.cmi lexer.cmo

clean:
	rm -f lexer.ml parser.ml{,i} *.{zi,zo,cmi,cmo} calc{,d}
.PHONY: clean
