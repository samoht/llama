LLAMAC=../../llamac
LLAMALEX=../../boot/llamalex
LLAMAYACC=../../boot/llamayacc

all: calc

lexer.ml: lexer.mll
	$(LLAMALEX) $<

parser.ml parser.mli: parser.mly
	$(LLAMAYACC) $<

parser.cmi: parser.mli
	$(LLAMAC) -c $<

%.cmo: %.ml
	$(LLAMAC) -c $<

calc: lexer.cmo parser.cmo calc.cmo
	$(LLAMAC) -o $@ $^

parser.cmo: parser.cmi
lexer.cmo: parser.cmi
calc.cmo: parser.cmi lexer.cmo

parser.cmo: parser.cmi
lexer.cmo: parser.cmi
calc.cmo: parser.cmi lexer.cmo

clean:
	rm -f calc
	rm -f lexer.ml parser.ml{,i} *.{cmi,cmo}
.PHONY: clean
