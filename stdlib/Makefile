# ----------------------------------------------------------------------
# Makefile for the standard library
# ----------------------------------------------------------------------

BOOTDIR=../boot
MODULES=pervasives $(NONPERVASIVES)
NONPERVASIVES=sys list array char string \
  hashtbl sort marshal obj \
  int32 int64 nativeint \
  lexing parsing \
  set map \
  stack queue \
  stream \
  buffer printf_sformat printf_tformat printf format \
  arg printexc gc \
  digest random_state random callback \
  genlex weak weak_hashtbl \
  filename complex scanning scanf str \
  int_misc nat big_int arith_flags \
  ratio num arith_status \
  unix terminfo

include ../Make/generic.mk

# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------

all: stdlib.lma std_exit.lmo
with-ocaml: stdlib.cma std_exit.cmo camlheader
.PHONY: all with-ocaml

stdlib.lma: $(MODULES:%=%.lmo)
	$(LLAMAC) -a $^ -o $@
stdlib.cma: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) -a $^ -o $@

# ----------------------------------------------------------------------
# Special
# ----------------------------------------------------------------------

camlheader:
	echo "#!`which ocamlrun`" > $@
pervasives.lmi: pervasives.mli
	$(LLAMAC) -c -nostdlib -nopervasives $<
pervasives.lml: pervasives.ml
	$(LLAMAC) -c -nostdlib -nopervasives $<
pervasives.cmi: pervasives.mli
	$(OCAMLC_STRICT) -c -nostdlib -nopervasives $<
pervasives.cmo: pervasives.ml
	$(OCAMLC_STRICT) -c -nostdlib -nopervasives $<

# ----------------------------------------------------------------------
# Depend
# ----------------------------------------------------------------------

depend:
	$(LLAMADEP) *.ml *.mli > .depend
ocamldepend:
	$(OCAMLDEP) *.ml *.mli > .ocamldepend
.PHONY: depend ocamldepend

include .depend
include .ocamldepend

pervasives.lml: pervasives.lmi
$(NONPERVASIVES:%=%.lmi): pervasives.lmi
pervasives.cmo: pervasives.cmi
$(NONPERVASIVES:%=%.cmi): pervasives.cmi

# ----------------------------------------------------------------------
# Clean
# ----------------------------------------------------------------------

clean:
	rm -f stdlib.lma *.lm{i,l,o,x}
ocamlclean:
	rm -f stdlib.cma *.cm{i,o} camlheader
.PHONY: clean ocamlclean

# ----------------------------------------------------------------------
# Install
# ----------------------------------------------------------------------

include ../config.mk

install:
	cp *.lmi stdlib.lma std_exit.lmo $(LIBDIR)
.PHONY: install
