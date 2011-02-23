BOOTDIR=../boot
BINARY=llamadep
INCLUDES=-I ../stdlib -I ../backlib -I ../frontlib
RUNTIME_PATH=-I ../back/byterun
LIBRARIES=stdlib backlib frontlib
MODULES=depconfig depmain
GENSOURES=depconfig.ml

include ../config.mk
include ../Make/generic.mk
include ../Make/app.mk
include ../Make/ocamlapp.mk

depconfig.ml: ../config.mk
	echo "let libdir = \"$(LIBDIR)\"" > $@
