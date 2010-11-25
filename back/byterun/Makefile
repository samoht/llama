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

CC=$(BYTECC)

UNIX=accept.c access.c addrofstr.c alarm.c bind.c chdir.c chmod.c \
  chown.c chroot.c close.c closedir.c connect.c cst2constr.c cstringv.c \
  dup.c dup2.c envir.c errmsg.c execv.c execve.c execvp.c exit.c \
  fchmod.c fchown.c fcntl.c fork.c ftruncate.c \
  getaddrinfo.c getcwd.c getegid.c geteuid.c getgid.c \
  getgr.c getgroups.c gethost.c gethostname.c getlogin.c \
  getnameinfo.c getpeername.c getpid.c getppid.c getproto.c getpw.c \
  gettimeofday.c getserv.c getsockname.c getuid.c gmtime.c \
  initgroups.c isatty.c itimer.c kill.c link.c listen.c lockf.c lseek.c \
  mkdir.c mkfifo.c nice.c open.c opendir.c pipe.c putenv.c read.c \
  readdir.c readlink.c rename.c rewinddir.c rmdir.c select.c sendrecv.c \
  setgid.c setgroups.c setsid.c setuid.c shutdown.c signals.c \
  sleep.c socket.c socketaddr.c \
  socketpair.c sockopt.c stat.c strofaddr.c symlink.c termios.c \
  time.c times.c truncate.c umask.c unixsupport.c unlink.c \
  utimes.c wait.c write.c

THREADS=scheduler.c

COMMONOBJS=\
  interp.o misc.o stacks.o fix_code.o startup.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o globroots.o \
  fail.o signals.o signals_byt.o printexc.o backtrace.o \
  compare.o ints.o floats.o str.o array.o io.o extern.o intern.o \
  hash.o sys.o meta.o parsing.o gc_ctrl.o terminfo.o md5.o obj.o \
  lexing.o callback.o debugger.o weak.o compact.o finalise.o custom.o \
  dynlink.o strstubs.o nat_stubs.o bng.o $(UNIX:%.c=unix/%.o) \
  $(THREADS:%.c=threads/%.o)

PRIMS=\
  alloc.c array.c compare.c extern.c floats.c gc_ctrl.c hash.c \
  intern.c interp.c ints.c io.c lexing.c md5.c meta.c obj.c parsing.c \
  signals.c str.c sys.c terminfo.c callback.c weak.c finalise.c stacks.c \
  dynlink.c backtrace.c strstubs.c nat_stubs.c $(UNIX:%.c=unix/%.c) \
  $(THREADS:%=threads/%)

PUBLIC_INCLUDES=\
  alloc.h callback.h config.h custom.h fail.h intext.h \
  memory.h misc.h mlvalues.h printexc.h signals.h compatibility.h

all:: llamarun$(EXE) ld.conf libcamlrun.$(A)
.PHONY: all

ld.conf: ../config/Makefile
	echo "$(STUBLIBDIR)" > ld.conf
	echo "$(LIBDIR)" >> ld.conf

install::
	cp llamarun$(EXE) $(BINDIR)/llamarun$(EXE)
	cp libcamlrun.$(A) $(LIBDIR)/libcamlrun.$(A)
	cd $(LIBDIR); $(RANLIB) libcamlrun.$(A)
	if test -d $(LIBDIR)/caml; then : ; else mkdir $(LIBDIR)/caml; fi
	for i in $(PUBLIC_INCLUDES); do \
	  sed -f cleanup-header $$i > $(LIBDIR)/caml/$$i; \
	done
	cp ld.conf $(LIBDIR)/ld.conf
.PHONY: install


primitives : $(PRIMS)
	sed -n -e "s/CAMLprim value \([a-z0-9_][a-z0-9_]*\).*/\1/p" \
	    $(PRIMS) | sort -u > primitives

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

bng.o: bng.c bng.h bng_digit.c bng_alpha.c bng_amd64.c bng_ia32.c bng_mips.c bng_ppc.c bng_sparc.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ -DBNG_ARCH_$(BNG_ARCH) -DBNG_ASM_LEVEL=$(BNG_ASM_LEVEL) $<

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
	rm -f unix/*.$(O) threads/*.$(0)
	rm -f primitives prims.c opnames.h jumptbl.h ld.conf
	rm -f version.h
.PHONY: clean

# ---------------------------------------------------------------------- #

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
	rm -f libcamlrun_shared.so .depend


.SUFFIXES: .d.o .pic.o

.c.d.o:
	$(CC) -c $(DFLAGS) $< -o $@

.c.pic.o:
	$(CC) -c $(CFLAGS) $(SHAREDCCCOMPOPTS) $< -o $@

depend : prims.c opnames.h jumptbl.h version.h
	-gcc -MM $(BYTECCCOMPOPTS) *.c > .depend
	-gcc -MM $(BYTECCCOMPOPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend
	-gcc -MM $(BYTECCCOMPOPTS) *.c | sed -e 's/\.o/.pic.o/' >> .depend

.PHONY: depend

-include .depend

boot: all
bootinstall: install
bootdepend: depend
bootclean: clean
.PHONY: boot bootinstall bootdepend bootclean
