HEVEA=hevea -exec xxdate.exe
LLAMADOC=../llamadoc/llamadoc

INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../stdlib

INTERFACES=stdlib/pervasives.mli \
  stdlib/array.mli stdlib/list.mli stdlib/char.mli stdlib/string.mli stdlib/sys.mli \
  stdlib/hashtbl.mli stdlib/sort.mli stdlib/marshal.mli \
  stdlib/int32.mli stdlib/int64.mli stdlib/nativeint.mli \
  stdlib/obj.mli stdlib/lexing.mli stdlib/parsing.mli \
  stdlib/set.mli stdlib/map.mli \
  stdlib/stack.mli stdlib/queue.mli \
  stdlib/stream.mli \
  stdlib/buffer.mli stdlib/printf.mli stdlib/format.mli \
  stdlib/arg.mli stdlib/printexc.mli stdlib/gc.mli \
  stdlib/digest.mli stdlib/random_state.mli stdlib/random.mli stdlib/callback.mli \
  stdlib/genlex.mli stdlib/weak.mli \
  stdlib/filename.mli stdlib/complex.mli stdlib/scanning.mli stdlib/scanf.mli \
  stdlib/str.mli \
  stdlib/num.mli stdlib/big_int.mli stdlib/arith_status.mli \
  parsing/location.mli parsing/parsetree.mli parsing/parse.mli

all: manual library
.PHONY: all

manual: manual.html
	make again
.PHONY: manual

manual.html: manual.tex
	$(HEVEA) $<
again:
	$(HEVEA) manual.tex
.PHONY: again

library:
	mkdir -p libref
	$(LLAMADOC) -sort -html $(INCLUDES) -d libref $(INTERFACES:%=../%)
	mv style.css libref
.PHONY: library

clean:
	rm -f manual.{haux,htoc,html}
	rm -f libref/*
.PHONY: clean
