include ../config/Makefile
LLAMAC=../llamac-new -nostdlib -I .

all: pervasives.cmo

%.cmi: %.mli
	$(LLAMAC) -c $<
%.cmo: %.ml
	$(LLAMAC) -c $<
pervasives.cmi: pervasives.mli
	$(LLAMAC) -nopervasives -c $<
pervasives.cmo: pervasives.ml
	$(LLAMAC) -nopervasives -c $<

clean:
	rm *.cmi *.cmo
.PHONY: clean
