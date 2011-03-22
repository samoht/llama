# ----------------------------------------------------------------------
# Makefile for the standard library
# ----------------------------------------------------------------------

BOOTDIR=../boot
THREADS=thread mutex condition event
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
with-ocaml: stdlib.p.cmxa std_exit.p.cmx std_exit.p.o camlheader
.PHONY: all with-ocaml

stdlib.lma: $(MODULES:%=%.lmo)
	$(LLAMAC) -a $^ -o $@
stdlib.p.cmxa: $(MODULES:%=%.p.cmx)
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
	$(LLAMADEP) *.ml > depend
	$(LLAMADEP) -I threads threads/*.ml >> depend

ocamldepend:
	$(OCAMLDEP) *.ml *.mli > .ocamldepend
	$(OCAMLDEP) -I threads threads/*.ml threads/*.mli >> .ocamldepend

.PHONY: ocamldepend

-include .depend
-include .ocamldepend

pervasives.lmo: pervasives.lmi
$(NONPERVASIVES:%=%.lmi): pervasives.lmi
$(NONPERVASIVES:%=%.lmo) std_exit.lmo: pervasives.lmo
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
