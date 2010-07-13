CAMLCOMP=../compiler/camlcomp -W -stdlib ../lib
CAMLLINK=../linker/camllink -stdlib ../lib
CC=cc
OPTS=
LIBS=

all: darkc camlexec testprog # camllight camlmktop

# header cannot be generated until camlrun is installed in $(BINDIR)...
# Also, "make install" is done with root permissions, meaning that we don't
# have write permission in the current directory if NFS-mounted...

install:
	(echo "#!$(BINDIR)/camlrun"; \
	 echo "exit 2"; \
	 cat testprog) > /tmp/testscr
	chmod a+x /tmp/testscr
	sh -c 'if sh -c /tmp/testscr 2>/dev/null; \
               then echo "#!$(VBINDIR)/camlrun" > $(LIBDIR)/header; \
               else cp camlexec $(LIBDIR)/header; \
               fi'
	rm -f /tmp/testscr
#	for script in darkc camllight camlmktop; do
	for script in darkc; do \
	  cp $$script $(BINDIR)/$$script; \
	  chmod a+x $(BINDIR)/$$script; \
	done

SEDCOMMANDS=\
  -e 's|LIBDIR|$(LIBDIR)|' -e 's|CC|$(CC) $(OPTS)|' -e 's|LIBS|$(LIBS)|'

darkc: darkc.tpl ../version.h
	sed  $(SEDCOMMANDS) \
            -e s/VERSION/`sed -e 's/#define VERSION //' ../version.h`/ \
            darkc.tpl > darkc

camllight: camllight.tpl
	sed $(SEDCOMMANDS) camllight.tpl > camllight

camlmktop: camlmktop.tpl
	sed $(SEDCOMMANDS) camlmktop.tpl > camlmktop

testprog: testprog.ml
	$(CAMLCOMP) testprog.ml
	$(CAMLLINK) -o testprog stdlib.zo testprog.zo

camlexec: camlexec.c
	$(CC) $(OPTS) -o camlexec camlexec.c
	strip camlexec

clean:
	rm -f camlexec darkc camllight camlmktop testprog testprog.z[io]

depend:
