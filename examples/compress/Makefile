LLAMAC=../../llamac

all: compr
.PHONY: all

compr: esbit.cmo fileprio.cmo huffman.cmo compr.cmo
	$(LLAMAC) -o $@ $^

clean:
	rm -f compr
	rm -f *.cmi *.cmo

.SUFFIXES: .mli .cmi .ml .cmo

.mli.cmi:
	$(LLAMAC) -c $<

.ml.cmo:
	$(LLAMAC) -c $<

# Generated dependencies below

compr.cmo: huffman.cmi 
decompr.cmo: huffman.cmi 
esbit.cmo: esbit.cmi 
fileprio.cmo: fileprio.cmi 
huffman.cmo: huffman.cmi esbit.cmi fileprio.cmi 
loadall.cmo: compr.cmo 
