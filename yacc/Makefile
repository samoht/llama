#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id: Makefile 10448 2010-05-21 11:28:21Z doligez $

# Makefile for the parser generator.

CFLAGS=-O -DNDEBUG

OBJS= closure.o error.o lalr.o lr0.o main.o mkpar.o output.o reader.o \
  skeleton.o symtab.o verbose.o warshall.o

all: llamayacc
boot: llamayacc
.PHONY: all boot

llamayacc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f *.o llamayacc
.PHONY: clean

closure.o: defs.h
error.o: defs.h
lalr.o: defs.h
lr0.o: defs.h
main.o: defs.h
mkpar.o: defs.h
output.o: defs.h
reader.o: defs.h
skeleton.o: defs.h
symtab.o: defs.h
verbose.o: defs.h
warshall.o: defs.h

depend:
bootdepend:
.PHONY: depend bootdepend

include ../config.mk
install:
	cp llamayacc $(BINDIR)
bootinstall: install
bootclean: clean
.PHONY: install bootinstall bootclean
