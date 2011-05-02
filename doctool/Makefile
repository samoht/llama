BOOTDIR=../boot
BINARY=llamadoc
INCLUDES=-I ../stdlib -I ../backlib -I ../frontlib
RUNTIME_PATH=../back/byterun
LIBRARIES=stdlib backlib frontlib
MODULES=\
  config clflags misc\
  odoc_config odoc_global odoc_messages odoc_types odoc_misc \
  odoc_text_parser odoc_text_lexer odoc_text odoc_name odoc_parameter \
  odoc_value odoc_type odoc_exception odoc_module odoc_print  \
  odoc_str odoc_args odoc_comments_global odoc_parser odoc_lexer \
  odoc_see_lexer odoc_env odoc_merge odoc_comments odoc_sig \
  odoc_control odoc_inherit odoc_search odoc_cross odoc_dep \
  odoc_analyse odoc_info odoc_llamahtml odoc_html odoc
GENSOURCES=\
  odoc_text_parser.ml odoc_text_parser.mli \
  odoc_parser.ml odoc_parser.mli \
  odoc_text_lexer.ml odoc_lexer.ml \
  odoc_llamahtml.ml odoc_see_lexer.ml \
  config.ml

include ../config.mk
include ../Make/generic.mk
include ../Make/app.mk
include ../Make/ocamlapp.mk

config.ml: ../config.mk
	echo "let standard_library = \"$(LIBDIR)\"" > $@
