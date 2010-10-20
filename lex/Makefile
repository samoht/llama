BOOTDIR=../boot
BINARY=llamalex
INCLUDES=-I ../stdlib
MODULES=cset syntax parser lexer table lexgen compact common output outputbis main
GENSOURCES=parser.ml parser.mli lexer.ml

include ../config.mk
include ../Make/generic.mk
include ../Make/app.mk
include ../Make/ocamlapp.mk

parser.ml parser.mli: parser.mly
lexer.ml: lexer.mll
