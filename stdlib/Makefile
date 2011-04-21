# ----------------------------------------------------------------------
# Makefile for the standard library
# ----------------------------------------------------------------------

BOOTDIR=../boot
THREADS=tesard thread
NONPERVASIVES=sys list array char string \
  hashtbl sort marshal int32 obj \
  int64 nativeint \
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
  unix terminfo $(THREADS:%=threads/%)

MODULES=pervasives $(NONPERVASIVES)
INCLUDES+=-I threads
include ../config.mk
include ../Make/generic.mk

# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------

all: stdlib.lma std_exit.lmo llamaheader
with-ocaml: stdlib.cma std_exit.cmo camlheader profiling.cma
.PHONY: all with-ocaml

stdlib.lma: $(MODULES:%=%.lmo)
	$(LLAMAC) -a $^ -o $@
stdlib.cma: $(MODULES:%=%.cmo)
	$(OCAMLC_STRICT) -a $^ -o $@
profiling.cma: profiling.cmo
	$(OCAMLC_STRICT) -a $^ -o $@

# ----------------------------------------------------------------------
# Special
# ----------------------------------------------------------------------

camlheader:
	echo "#!`which ocamlrun`" > $@
llamaheader: ../config.mk
	echo "#!$(BINDIR)/llamarun" > $@
pervasives.lmo: pervasives.ml
	$(LLAMAC) -c $(INCLUDES) -nopervasives $<
pervasives.cmo: pervasives.ml
	$(OCAMLC_STRICT) -c $(INCLUDES) -nopervasives $<

# ----------------------------------------------------------------------
# Depend
# ----------------------------------------------------------------------

depend: *.ml threads/*.ml
	$(LLAMADEP) *.ml > depend
	$(LLAMADEP) -I threads threads/*.ml >> depend

ocamldepend:
	$(OCAMLDEP) *.ml *.mli > .ocamldepend
	$(OCAMLDEP) -I threads threads/*.ml threads/*.mli >> .ocamldepend

.PHONY: ocamldepend

-include .depend
-include .ocamldepend

$(NONPERVASIVES:%=%.lmo) std_exit.lmo: pervasives.lmo
$(NONPERVASIVES:%=%.cmo) std_exit.cmo: pervasives.cmo

# ----------------------------------------------------------------------
# Clean
# ----------------------------------------------------------------------

clean:
	rm -f stdlib.lma *.lmi *.lml *.lmo *.lmx llamaheader
	rm -f threads/*.lmi threads/*.lml threads/*.lmo threads/*.lmx
	rm -f depend
ocamlclean:
	rm -f *.cmi *.cmo camlheader *.o *.cma
	rm -f threads/*.cmi threads/*.cmo threads/*.o
	rm -f .ocamldepend
.PHONY: clean ocamlclean

# ----------------------------------------------------------------------
# Install
# ----------------------------------------------------------------------

include ../config.mk

install:
	cp *.lmi stdlib.lma std_exit.lmo llamaheader $(LIBDIR)
.PHONY: install
