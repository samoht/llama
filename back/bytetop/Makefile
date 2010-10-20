BOOTDIR=../../boot
BINARY=llama

INCLUDES=-I ../../stdlib -I ../../backlib -I ../../frontlib -I ../bytelib
LIBRARIES=stdlib backlib frontlib bytelib

MODULES=\
  ledit_fstream ledit_cursor \
  ledit_char ledit_string ledit \
  printer toploop \
  trace topdirs topmain

BYTELINKFLAGS=-linkall

include ../../config.mk
include ../../Make/generic.mk
include ../../Make/app.mk
