# ----------------------------------------------------------------------
# Makefile for the standard library
# ----------------------------------------------------------------------

BOOTDIR=../boot
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
THREADS=thread mutex condition event

MODULES=pervasives $(NONPERVASIVES) $(THREADS:%=threads/%)
INCLUDES+=-I threads
include ../config.mk
include ../Make/generic.mk

# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------

all: stdlib.lma std_exit.lmo llamaheader
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
llamaheader: ../config.mk
	echo "#!$(BINDIR)/llamarun" > $@
pervasives.lmi: pervasives.mli
	$(LLAMAC) -c $(INCLUDES) -nopervasives $<
pervasives.lmo: pervasives.ml
	$(LLAMAC) -c $(INCLUDES) -nopervasives $<
pervasives.cmi: pervasives.mli
	$(OCAMLC_STRICT) -c $(INCLUDES) -nopervasives $<
pervasives.cmo: pervasives.ml
	$(OCAMLC_STRICT) -c $(INCLUDES) -nopervasives $<

# ----------------------------------------------------------------------
# Depend
# ----------------------------------------------------------------------

depend:
	$(LLAMADEP) *.ml *.mli threads/*.ml thread/*.mli > .depend
ocamldepend:
	$(OCAMLDEP) *.ml *.mli threads/*.ml threads/*.mli > .ocamldepend
.PHONY: depend ocamldepend

include .depend
include .ocamldepend

pervasives.lmo: pervasives.lmi
$(NONPERVASIVES:%=%.lmi): pervasives.lmi
pervasives.cmo: pervasives.cmi
$(NONPERVASIVES:%=%.cmi): pervasives.cmi

# ----------------------------------------------------------------------
# Clean
# ----------------------------------------------------------------------

clean:
	rm -f stdlib.lma *.lmi *.lml *.lmo *.lmx llamaheader
	rm -f threads/*.lmi threads/*.lml threads/*.lmo threads/*.lmx
ocamlclean:
	rm -f stdlib.cma *.cmi *.cmo camlheader
	rm -f threads/*.cmi threads/*.cmo
.PHONY: clean ocamlclean

# ----------------------------------------------------------------------
# Install
# ----------------------------------------------------------------------

include ../config.mk

install:
	cp *.lmi stdlib.lma std_exit.lmo llamaheader $(LIBDIR)
.PHONY: install
