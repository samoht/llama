BOOTDIR=../boot
LIBRARY=frontlib
INCLUDES=-I ../stdlib -I ../backlib
LIBRARIES=backlib
MODULES= \
  log effect \
  frontmisc frontconfig warnings \
  linenum frontlocation longident \
  syntaxerr parsetree parser \
  lexer parse \
  unused_var outcometree oprint \
  base predef basics \
  persistent module modenv env \
  mutable_base printtyp resolve \
  typify immutify \
  include typemain \
  makeident makelambda \
  typeopt pmc_pattern parmatch switch matching \
  translcore translmod simplif \
  depend fronterrors
GENSOURCES=parser.ml parser.mli lexer.ml linenum.ml

include ../Make/generic.mk
include ../Make/lib.mk
include ../Make/ocamllib.mk
