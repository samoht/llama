#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

# $Id: Makefile 9319 2009-07-20 11:51:50Z doligez $

include ../config/Makefile

CC=cc
RANLIB=ranlib # xxx
A=a
MKEXE=cc
O=o
SO=so

COMMONOBJS=\
  interp.o misc.o stacks.o fix_code.o startup.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o globroots.o \
  fail.o signals.o signals_byt.o printexc.o backtrace.o \
  compare.o ints.o floats.o str.o array.o io.o extern.o intern.o \
  hash.o sys.o meta.o parsing.o gc_ctrl.o terminfo.o md5.o obj.o \
  lexing.o callback.o debugger.o weak.o compact.o finalise.o custom.o \
  dynlink.o

PRIMS=\
  alloc.c array.c compare.c extern.c floats.c gc_ctrl.c hash.c \
  intern.c interp.c ints.c io.c lexing.c md5.c meta.c obj.c parsing.c \
  signals.c str.c sys.c terminfo.c callback.c weak.c finalise.c stacks.c \
  dynlink.c backtrace.c

PUBLIC_INCLUDES=\
  alloc.h callback.h config.h custom.h fail.h intext.h \
  memory.h misc.h mlvalues.h printexc.h signals.h compatibility.h


all:: llamarun$(EXE) libcamlrun.$(A) # ld.conf 
.PHONY: all

#ld.conf: ../config/Makefile
#	echo "$(STUBLIBDIR)" > ld.conf
#	echo "$(LIBDIR)" >> ld.conf

install::
	cp llamarun$(EXE) $(BINDIR)/llamarun$(EXE)
	cp libcamlrun.$(A) $(LIBDIR)/libcamlrun.$(A)
	cd $(LIBDIR); $(RANLIB) libcamlrun.$(A)
	if test -d $(LIBDIR)/caml; then : ; else mkdir $(LIBDIR)/caml; fi
	for i in $(PUBLIC_INCLUDES); do \
	  sed -f ../tools/cleanup-header $$i > $(LIBDIR)/caml/$$i; \
	done
#	cp ld.conf $(LIBDIR)/ld.conf
.PHONY: install


primitives : $(PRIMS)
	sed -n -e "s/CAMLprim value \([a-z0-9_][a-z0-9_]*\).*/\1/p" \
	    $(PRIMS) > primitives

prims.c : primitives
	(echo '#include "mlvalues.h"'; \
	 echo '#include "prims.h"'; \
	 sed -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive caml_builtin_cprim[] = {'; \
	 sed -e 's/.*/	&,/' primitives; \
	 echo '	 0 };'; \
	 echo 'char * caml_names_of_builtin_cprim[] = {'; \
	 sed -e 's/.*/	"&",/' primitives; \
	 echo '	 0 };') > prims.c

opnames.h : instruct.h
	sed -e '/\/\*/d' \
	    -e '/^#/d' \
	    -e 's/enum /char * names_of_/' \
	    -e 's/{$$/[] = {/' \
	    -e 's/\([[:upper:]][[:upper:]_0-9]*\)/"\1"/g' instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : instruct.h
	sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
	       -e '/^}/q' instruct.h > jumptbl.h

version.h : ../VERSION
	echo "#define OCAML_VERSION \"`sed -e 1q ../VERSION`\"" > version.h

clean ::
	rm -f llamarun$(EXE) llamarund$(EXE) *.$(O) *.$(A) *.$(SO)
	rm -f primitives prims.c opnames.h jumptbl.h # ld.conf
	rm -f version.h
.PHONY: clean

CFLAGS=-DCAML_NAME_SPACE -O $(BYTECCCOMPOPTS) $(IFLEXDIR)
DFLAGS=-DCAML_NAME_SPACE -g -DDEBUG $(BYTECCCOMPOPTS)

OBJS=$(COMMONOBJS) unix.o main.o
DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o
PICOBJS=$(OBJS:.o=.pic.o)

SHARED_LIBS_TMP=$(SUPPORTS_SHARED_LIBRARIES:%false=)
SHARED_LIBS_DEPS=$(SHARED_LIBS_TMP:%true=libcamlrun_shared.so)

all:: $(SHARED_LIBS_DEPS)

llamarun$(EXE): libcamlrun.a prims.o
	$(MKEXE) $(BYTECCLINKOPTS) -o llamarun$(EXE) \
		  prims.o libcamlrun.a $(BYTECCLIBS)

llamarund$(EXE): libcamlrund.a prims.o
	$(MKEXE) -g $(BYTECCLINKOPTS) -o llamarund$(EXE) \
		  prims.o libcamlrund.a $(BYTECCLIBS)

libcamlrun.a: $(OBJS)
	ar rc libcamlrun.a $(OBJS)
	$(RANLIB) libcamlrun.a

libcamlrund.a: $(DOBJS)
	ar rc libcamlrund.a $(DOBJS)
	$(RANLIB) libcamlrund.a

libcamlrun_shared.so: $(PICOBJS)
	$(MKDLL) -o libcamlrun_shared.so $(PICOBJS) $(BYTECCLIBS)

install::
	if test -f libcamlrun_shared.so; then \
	  cp libcamlrun_shared.so $(LIBDIR)/libcamlrun_shared.so; fi

clean::
	rm -f libcamlrun_shared.so


.SUFFIXES: .d.o .pic.o

.c.d.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(DFLAGS) $<
	mv $*.o $*.d.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

.c.pic.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(CFLAGS) $(SHAREDCCCOMPOPTS) $<
	mv $*.o $*.pic.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

depend : prims.c opnames.h jumptbl.h version.h
	-gcc -MM $(BYTECCCOMPOPTS) *.c > .depend
	-gcc -MM $(BYTECCCOMPOPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend
	-gcc -MM $(BYTECCCOMPOPTS) *.c | sed -e 's/\.o/.pic.o/' >> .depend
.PHONY: depend

include .depend
