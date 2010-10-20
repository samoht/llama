BOOTDIR=../boot
BINARY=llamadep
INCLUDES=-I ../stdlib -I ../backlib -I ../frontlib
LIBRARIES=stdlib backlib frontlib
MODULES=depconfig depmain
GENSOURES=depconfig.ml

include ../config.mk
include ../Make/generic.mk
include ../Make/app.mk
include ../Make/ocamlapp.mk

depconfig.ml: ../config.mk
	echo "let libdir = \"$(LIBDIR)\"" > $@
