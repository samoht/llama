# ----------------------------------------------------------------------
# Makefile for the standard library
# ----------------------------------------------------------------------

BOOTDIR=../boot
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
with-ocaml: stdlib.cmxa std_exit.cmx std_exit.o camlheader
.PHONY: all with-ocaml

stdlib.lma: $(MODULES:%=%.lmo)
	$(LLAMAC) -a $^ -o $@
stdlib.cmxa: $(MODULES:%=%.cmx)
	$(OCAMLC_STRICT) -a $^ -o $@

# ----------------------------------------------------------------------
# Special
# ----------------------------------------------------------------------

camlheader:
	echo "#!`which ocamlrun`" > $@
llamaheader: ../config.mk
	echo "#!$(BINDIR)/llamarun" > $@
pervasives.lmi: pervasives.ml
	$(LLAMAC) -c $(INCLUDES) -nopervasives $<
pervasives.lmo: pervasives.ml
	$(LLAMAC) -c $(INCLUDES) -nopervasives $<
pervasives.cmi: pervasives.ml
	$(OCAMLC_STRICT) -c $(INCLUDES) -nopervasives $<
pervasives.cmx: pervasives.ml
	$(OCAMLC_STRICT) -c $(INCLUDES) -nopervasives $<

# ----------------------------------------------------------------------
# Depend
# ----------------------------------------------------------------------

depend: *.ml threads/*.ml
	$(LLAMADEP) *.ml threads/*.ml > depend

ocamldepend:
	$(OCAMLDEP) *.ml *.mli threads/*.ml threads/*.mli > .ocamldepend

.PHONY: ocamldepend

-include .depend
-include .ocamldepend

pervasives.lmo: pervasives.lmi
$(NONPERVASIVES:%=%.lmi): pervasives.lmi
pervasives.cmx: pervasives.cmi
$(NONPERVASIVES:%=%.cmi): pervasives.cmi

# ----------------------------------------------------------------------
# Clean
# ----------------------------------------------------------------------

clean:
	rm -f stdlib.lma *.lmi *.lml *.lmo *.lmx llamaheader
	rm -f threads/*.lmi threads/*.lml threads/*.lmo threads/*.lmx
	rm -f depend
ocamlclean:
	rm -f stdlib.cmxa *.cmi *.cmx camlheader *.o *.cmxa
	rm -f threads/*.cmi threads/*.cmx threads/*.o
	rm -f .ocamldepend
.PHONY: clean ocamlclean

# ----------------------------------------------------------------------
# Install
# ----------------------------------------------------------------------

include ../config.mk

install:
	cp *.lmi stdlib.lma std_exit.lmo llamaheader $(LIBDIR)
.PHONY: install
