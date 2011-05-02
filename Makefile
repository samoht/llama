include config.mk

# ----------------------------------------------------------------------
# Main targets
# ----------------------------------------------------------------------
.PHONY: default all with-ocaml

# Build the core native executables in "boot", then build everything
# using the ambient C compiler and the core system in "boot"
default: all

# Build the C parts
c:
	make -C back/byterun && cp back/byterun/llamarun boot
	make -C yacc

# Build everything using the ambient C compiler and the core system in "boot"
all: c
	make depend
	make -C stdlib
	make -C backlib
	make -C frontlib
	make -C deptool
	make -C lex
	make -C doctool
	make -C back/bytelib
	make -C back/bytec
	make -C back/bytetop

# Build an emergency core system using the ambient C and OCaml compilers
with-ocaml: c ocamldepend
	make -C stdlib       with-ocaml
	make -C backlib      with-ocaml
	make -C frontlib     with-ocaml
	make -C deptool      with-ocaml
	make -C lex          with-ocaml
	make -C doctool      with-ocaml
	make -C back/bytelib with-ocaml
	make -C back/bytec   with-ocaml
	make -C back/bytetop with-ocaml

# Bootstrap
.PHONY: promote ocamlpromote bootstrap ocamlbootstrap recover-bootstrap

# Copy the core bytecode executables to "boot"
promote:
	cp deptool/llamadep boot
	cp lex/llamalex boot
	cp back/bytec/llamac boot

ocamlpromote:
	mkdir -p boot
	cp back/byterun/llamarun boot/llamarun
	cp yacc/llamayacc boot
	cp deptool/llamadep-ocaml boot/llamadep
	cp lex/llamalex-ocaml boot/llamalex
	cp back/bytec/llamac-ocaml boot/llamac

bootstrap:
	make && make promote && make mlclean && make

ocamlbootstrap:
	make with-ocaml && make ocamlpromote && make mlclean && make

recover-bootstrap:
	git checkout boot/ && make mlclean

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

.PHONY: install ocamlinstall uninstall

install:
	mkdir -p $(BINDIR) $(LIBDIR)
	for dir in $(CDIRS) $(APPDIRS) stdlib; do make -C $$dir install; done

ocamlinstall:
	mkdir -p $(BINDIR) $(LIBDIR)
	for dir in $(CDIRS); do make -C $$dir install; done
	for dir in $(APPDIRS) stdlib; do make -C $$dir ocamlinstall; done

uninstall:
	rm -f $(BINDIR)/llama $(BINDIR)/llamac $(BINDIR)/llamadep $(BINDIR)/llamadoc \
		$(BINDIR)/llamalex $(BINDIR)/llamarun $(BINDIR)/llamayacc
	rm -rf $(LIBDIR)

# ----------------------------------------------------------------------
# Depend
# ----------------------------------------------------------------------

.PHONY: depend ocamldepend

depend:
	for dir in $(DIRS); do make -C $$dir depend; done
ocamldepend:
	for dir in $(COREDIRS); do make -C $$dir ocamldepend; done

# ----------------------------------------------------------------------
# Clean
# ----------------------------------------------------------------------

clean:
	for dir in $(DIRS); do make -C $$dir clean; done
mlclean:
	for dir in $(MLDIRS); do make -C $$dir clean; done
ocamlclean:
	for dir in $(CDIRS); do make -C $$dir clean; done
	for dir in $(MLDIRS); do make -C $$dir ocamlclean; done
.PHONY: clean mlclean ocamlclean
