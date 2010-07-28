include ../config/Makefile
LLAMAC=../llamac-new -nostdlib -I .

all: pervasives.cmo

%.lli: %.mli
	$(LLAMAC) -c $<
%.cmo: %.ml
	$(LLAMAC) -c $<
pervasives.lli: pervasives.mli
	$(LLAMAC) -nopervasives -c $<
pervasives.cmo: pervasives.ml
	$(LLAMAC) -nopervasives -c $<

clean:
	rm *.lli *.cmo
.PHONY: clean

include .depend
