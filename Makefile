include config.mk

# ----------------------------------------------------------------------
# Main targets
# ----------------------------------------------------------------------

# Build the core native executables in "boot", then build everything
# using the ambient C compiler and the core system in "boot"

default: back/byterun/llamarun yacc/llamayacc
	cp $^ boot
	make all
.PHONY: default

# Build everything using the ambient C compiler and the core system in "boot"

all:
	make -C stdlib
	make -C backlib && make backlib/backlib.lma
	make -C frontlib && make frontlib/frontlib.lma
	make -C deptool && make deptool/llamadep
	make -C lex && make lex/llamalex
	make -C yacc
	make -C doctool && make doctool/llamadoc
	make -C back/byterun
	make -C back/bytelib && make back/bytelib/bytelib.lma
	make -C back/bytec && make back/bytec/llamac
	make -C back/bytetop && make back/bytetop/llama
.PHONY: all

# Copy the core bytecode executables to "boot"

promote:
	cp deptool/llamadep boot
	cp lex/llamalex boot
	cp back/bytec/llamac boot
.PHONY: promote

# Build an emergency core system using the ambient C and OCaml compilers

with-ocaml:
	make -C stdlib with-ocaml
	make -C backlib with-ocaml && make backlib/backlib.cma
	make -C frontlib with-ocaml && make frontlib/frontlib.cma
	make -C back/byterun
	make -C deptool with-ocaml && make deptool/llamadep-ocaml
	make -C lex with-ocaml && make lex/llamalex-ocaml
	make -C yacc
	make -C back/bytelib with-ocaml && make back/bytelib/bytelib.cma
	make -C back/bytec with-ocaml && make back/bytec/llamac-ocaml
.PHONY: with-ocaml

# Copy the emergency core system to "boot"
# The emergency "llamarun" is a shell script that calls ocamlrun

ocamlpromote:
	mkdir -p boot
	cp back/byterun/llamarun-ocaml boot/llamarun
	cp yacc/llamayacc boot
	cp deptool/llamadep-ocaml boot/llamadep
	cp lex/llamalex-ocaml boot/llamalex
	cp back/bytec/llamac-ocaml boot/llamac
.PHONY: ocamlpromote

# ----------------------------------------------------------------------
# Directory rules and dependencies
# ----------------------------------------------------------------------

stdlib/stdlib.lma:
	make -C stdlib clean
	make -C stdlib

stdlib/stdlib.cma:
	make -C stdlib ocamlclean
	make -C stdlib with-ocaml

backlib/backlib.lma: stdlib/stdlib.lma
	make -C backlib clean
	make -C backlib

backlib/backlib.cma: stdlib/stdlib.cma
	make -C backlib ocamlclean
	make -C backlib with-ocaml

frontlib/frontlib.lma: backlib/backlib.lma
	make -C frontlib clean
	make -C frontlib

frontlib/frontlib.cma: backlib/backlib.cma
	make -C frontlib ocamlclean
	make -C frontlib with-ocaml

frontc/llamafrontc: frontlib/frontlib.lma
	make -C frontc clean
	make -C frontc

frontc/llamafrontc-ocaml: frontlib/frontlib.cma
	make -C frontc ocamlclean
	make -C frontc with-ocaml

deptool/llamadep: frontlib/frontlib.lma
	make -C deptool clean
	make -C deptool

deptool/llamadep-ocaml: frontlib/frontlib.cma
	make -C deptool ocamlclean
	make -C deptool ocaml

lex/llamalex: stdlib/stdlib.lma
	make -C lex clean
	make -C lex

lex/llamalex-ocaml: stdlib/stdlib.cma
	make -C lex ocamlclean
	make -C lex with-ocaml

yacc/llamayacc:
	make -C yacc clean
	make -C yacc

doctool/llamadoc:
	make -C doctool clean
	make -C doctool

back/byterun/llamarun:
	make -C back/byterun clean
	make -C back/byterun

back/bytelib/bytelib.lma: backlib/backlib.lma back/byterun/llamarun
	make -C back/bytelib clean
	make -C back/bytelib

back/bytelib/bytelib.cma: backlib/backlib.cma back/byterun/llamarun
	make -C back/bytelib ocamlclean
	make -C back/bytelib with-ocaml

back/bytec/llamac: back/bytelib/bytelib.lma
	make -C back/bytec clean
	make -C back/bytec

back/bytec/llamac-ocaml: back/bytelib/bytelib.cma
	make -C back/bytec ocamlclean
	make -C back/bytec with-ocaml

back/bytetop/llama: frontlib/frontlib.lma back/bytelib/bytelib.lma
	make -C back/bytetop clean
	make -C back/bytetop

# ----------------------------------------------------------------------

CDIRS=yacc back/byterun

CORELIBDIRS=stdlib frontlib backlib back/bytelib
COREAPPDIRS=deptool lex back/bytec
COREDIRS=$(CORELIBDIRS) $(COREAPPDIRS)

LIBDIRS=$(CORELIBDIRS)
APPDIRS=$(COREAPPDIRS) doctool back/bytetop
DIRS=$(LIBDIRS) $(APPDIRS) $(CDIRS)

# ----------------------------------------------------------------------
# Install
# ----------------------------------------------------------------------

install:
	mkdir -p $(BINDIR) $(LIBDIR)
	for dir in $(CDIRS) $(APPDIRS) stdlib; do make -C $$dir install; done
.PHONY: install

# ----------------------------------------------------------------------
# Depend
# ----------------------------------------------------------------------

depend:
	for dir in $(DIRS); do make -C $$dir depend; done
ocamldepend:
	for dir in $(COREDIRS); do make -C $$dir ocamldepend; done
.PHONY: depend ocamldepend

# ----------------------------------------------------------------------
# Clean
# ----------------------------------------------------------------------

clean:
	for dir in $(DIRS); do make -C $$dir clean; done
ocamlclean:
	for dir in $(COREDIRS); do make -C $$dir ocamlclean; done
.PHONY: clean ocamlclean
