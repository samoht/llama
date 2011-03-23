include config.mk

# ----------------------------------------------------------------------
# Main targets
# ----------------------------------------------------------------------

# Build the core native executables in "boot", then build everything
# using the ambient C compiler and the core system in "boot"

default: back/byterun/llamarun yacc/llamayacc
	cp $^ boot
	$(MAKE) all
.PHONY: default

# Build everything using the ambient C compiler and the core system in "boot"

all: depend
	$(MAKE) -C stdlib
	$(MAKE) -C backlib && $(MAKE) backlib/backlib.lma
	$(MAKE) -C frontlib && $(MAKE) frontlib/frontlib.lma
	$(MAKE) -C back/byterun
	$(MAKE) -C deptool && $(MAKE) deptool/llamadep
	$(MAKE) -C lex && $(MAKE) lex/llamalex
	$(MAKE) -C yacc
	$(MAKE) -C doctool && $(MAKE) doctool/llamadoc
	$(MAKE) -C back/bytelib && $(MAKE) back/bytelib/bytelib.lma
	$(MAKE) -C back/bytec && $(MAKE) back/bytec/llamac
	$(MAKE) -C back/bytetop && $(MAKE) back/bytetop/llama
.PHONY: all

# Copy the core bytecode executables to "boot"

promote:
	cp deptool/llamadep boot
	cp lex/llamalex boot
	cp back/bytec/llamac boot
.PHONY: promote

# Build an emergency core system using the ambient C and OCaml compilers

with-ocaml: ocamldepend
	$(MAKE) -C stdlib with-ocaml
	$(MAKE) -C backlib with-ocaml && $(MAKE) backlib/backlib.cma
	$(MAKE) -C frontlib with-ocaml && $(MAKE) frontlib/frontlib.cma
	$(MAKE) -C back/byterun with-ocaml
	$(MAKE) -C deptool with-ocaml && $(MAKE) deptool/llamadep-ocaml
	$(MAKE) -C lex with-ocaml && $(MAKE) lex/llamalex-ocaml
	$(MAKE) -C yacc
	$(MAKE) -C back/bytelib with-ocaml && $(MAKE) back/bytelib/bytelib.cma
	$(MAKE) -C back/bytec with-ocaml && $(MAKE) back/bytec/llamac-ocaml
.PHONY: with-ocaml

# Copy the emergency core system to "boot"
# The emergency "llamarun" is a shell script that calls ocamlrun

ocamlpromote:
	mkdir -p boot
	cp back/byterun/llamarun boot/llamarun
	cp yacc/llamayacc boot
	cp deptool/llamadep-ocaml boot/llamadep
	cp lex/llamalex-ocaml boot/llamalex
	cp back/bytec/llamac-ocaml boot/llamac
.PHONY: ocamlpromote

# Bootstrap the compiler
bootstrap:
	$(MAKE) && $(MAKE) promote && $(MAKE) mlclean && $(MAKE)

recover-bootstrap:
	git checkout boot/ && $(MAKE) mlclean && $(MAKE)

# ----------------------------------------------------------------------
# Directory rules and dependencies
# ----------------------------------------------------------------------

stdlib/stdlib.lma:
	$(MAKE) -C stdlib clean
	$(MAKE) -C stdlib

stdlib/stdlib.cma:
	$(MAKE) -C stdlib ocamlclean
	$(MAKE) -C stdlib with-ocaml

backlib/backlib.lma: stdlib/stdlib.lma
	$(MAKE) -C backlib clean
	$(MAKE) -C backlib

backlib/backlib.cma: stdlib/stdlib.cma
	$(MAKE) -C backlib ocamlclean
	$(MAKE) -C backlib with-ocaml

frontlib/frontlib.lma: backlib/backlib.lma
	$(MAKE) -C frontlib clean
	$(MAKE) -C frontlib

frontlib/frontlib.cma: backlib/backlib.cma
	$(MAKE) -C frontlib ocamlclean
	$(MAKE) -C frontlib with-ocaml

frontc/llamafrontc: frontlib/frontlib.lma
	$(MAKE) -C frontc clean
	$(MAKE) -C frontc

frontc/llamafrontc-ocaml: frontlib/frontlib.cma
	$(MAKE) -C frontc ocamlclean
	$(MAKE) -C frontc with-ocaml

deptool/llamadep: frontlib/frontlib.lma
	$(MAKE) -C deptool clean
	$(MAKE) -C deptool

deptool/llamadep-ocaml: frontlib/frontlib.cma
	$(MAKE) -C deptool ocamlclean
	$(MAKE) -C deptool ocaml

lex/llamalex: stdlib/stdlib.lma
	$(MAKE) -C lex clean
	$(MAKE) -C lex

lex/llamalex-ocaml: stdlib/stdlib.cma
	$(MAKE) -C lex ocamlclean
	$(MAKE) -C lex with-ocaml

yacc/llamayacc:
	$(MAKE) -C yacc clean
	$(MAKE) -C yacc

doctool/llamadoc:
	$(MAKE) -C doctool clean
	$(MAKE) -C doctool

back/byterun/llamarun:
	$(MAKE) -C back/byterun clean
	$(MAKE) -C back/byterun

back/bytelib/bytelib.lma: backlib/backlib.lma back/byterun/llamarun
	$(MAKE) -C back/bytelib clean
	$(MAKE) -C back/bytelib

back/bytelib/bytelib.cma: backlib/backlib.cma back/byterun/llamarun
	$(MAKE) -C back/bytelib ocamlclean
	$(MAKE) -C back/bytelib with-ocaml

back/bytec/llamac: back/bytelib/bytelib.lma
	$(MAKE) -C back/bytec clean
	$(MAKE) -C back/bytec

back/bytec/llamac-ocaml: back/bytelib/bytelib.cma
	$(MAKE) -C back/bytec ocamlclean
	$(MAKE) -C back/bytec with-ocaml

back/bytetop/llama: frontlib/frontlib.lma back/bytelib/bytelib.lma
	$(MAKE) -C back/bytetop clean
	$(MAKE) -C back/bytetop

# ----------------------------------------------------------------------

CDIRS=yacc back/byterun

CORELIBDIRS=stdlib frontlib backlib back/bytelib
COREAPPDIRS=deptool lex back/bytec
COREDIRS=$(CORELIBDIRS) $(COREAPPDIRS)

LIBDIRS=$(CORELIBDIRS)
APPDIRS=$(COREAPPDIRS) doctool back/bytetop
MLDIRS=$(LIBDIRS) $(APPDIRS)

DIRS=$(MLDIRS) $(CDIRS)

# ----------------------------------------------------------------------
# Install
# ----------------------------------------------------------------------

install:
	mkdir -p $(BINDIR) $(LIBDIR)
	for dir in $(CDIRS) $(APPDIRS) stdlib; do $(MAKE) -C $$dir install; done
.PHONY: install

# ----------------------------------------------------------------------
# Depend
# ----------------------------------------------------------------------

depend:
	for dir in $(DIRS); do $(MAKE) -C $$dir depend; done
ocamldepend:
	for dir in $(COREDIRS); do $(MAKE) -C $$dir ocamldepend; done
.PHONY: depend ocamldepend

# ----------------------------------------------------------------------
# Clean
# ----------------------------------------------------------------------

clean:
	for dir in $(DIRS); do $(MAKE) -C $$dir clean; done
mlclean:
	for dir in $(MLDIRS); do $(MAKE) -C $$dir clean; done
ocamlclean:
	for dir in $(COREDIRS); do $(MAKE) -C $$dir ocamlclean; done
.PHONY: clean mlclean ocamlclean
