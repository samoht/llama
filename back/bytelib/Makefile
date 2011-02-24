BOOTDIR=../../boot
LIBRARY=bytelib
INCLUDES=-I ../../stdlib -I ../../backlib
LIBRARIES=backlib
MODULES=misc config clflags ccomp runtimedef \
  meta instruct store bytegen \
  printinstr opcodes cmo_format emitcode \
  bytesections dll symtable \
  bytelink bytelibrarian byteerrors
GENSOURCES=config.ml opcodes.ml runtimedef.ml

include ../config/Makefile
include ../../Make/generic.mk
include ../../Make/lib.mk
include ../../Make/ocamllib.mk

config.ml: config.mlp ../config/Makefile
	@rm -f config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
            -e 's|%%BYTERUN%%|$(BINDIR)/llamarun|' \
	    -e 's|%%CCOMPTYPE%%|cc|' \
	    -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)|' \
	    -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCCOMPOPTS)|' \
	    -e 's|%%PACKLD%%|$(PACKLD)|' \
	    -e 's|%%BYTECCLIBS%%|$(BYTECCLIBS)|' \
	    -e 's|%%NATIVECCLIBS%%|$(NATIVECCLIBS)|' \
	    -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
	    -e 's|%%CC_PROFILE%%|$(CC_PROFILE)|' \
	    -e 's|%%ARCH%%|$(ARCH)|' \
	    -e 's|%%MODEL%%|$(MODEL)|' \
	    -e 's|%%SYSTEM%%|$(SYSTEM)|' \
	    -e 's|%%EXT_OBJ%%|.o|' \
	    -e 's|%%EXT_ASM%%|.s|' \
	    -e 's|%%EXT_LIB%%|.a|' \
	    -e 's|%%EXT_DLL%%|.so|' \
	    -e 's|%%SYSTHREAD_SUPPORT%%|$(SYSTHREAD_SUPPORT)|' \
	    -e 's|%%ASM%%|$(ASM)|' \
	    -e 's|%%MKDLL%%|$(MKDLL)|' \
	    -e 's|%%MKEXE%%|$(MKEXE)|' \
	    -e 's|%%MKMAINDLL%%|$(MKMAINDLL)|' \
	    config.mlp > config.ml
	@chmod -w config.ml

opcodes.ml: ../byterun/instruct.h
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' $< | \
	awk -f make-opcodes > $@

runtimedef.ml: ../byterun/primitives ../byterun/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' ../byterun/fail.h | \
	 sed -e '$$s/;$$//'; \
	 echo '|]'; \
	 echo 'let builtin_primitives = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' ../byterun/primitives; \
	 echo '|]') > $@
