OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

# All Source Files, ordered with respect to their dependencies:

SOURCES=datastructure/darray.mli \
	datastructure/darray.ml \
	datastructure/util.mli \
	datastructure/util.ml \
	datastructure/position.mli  \
	datastructure/position.ml \
	datastructure/hol_type.mli \
	datastructure/hol_type.ml \
	datastructure/term.mli \
	datastructure/term.ml \
	datastructure/signature.mli \
	datastructure/signature.ml \
	datastructure/termset.mli \
	datastructure/termset.ml \
	datastructure/substitution.mli \
	datastructure/substitution.ml \
	datastructure/orderings.mli \
	datastructure/orderings.ml \
	datastructure/termsystem.mli \
	datastructure/termsystem.ml \
\
	datastructure/literal.ml \
	datastructure/clause.ml \
	datastructure/clauseset.ml \
	datastructure/main.ml \
\
	calculus/calculus.mli \
	calculus/calculus.ml \
	calculus/automation.mli \
	calculus/automation.ml \
\
	parser-hotptp/preprocess.mli \
	parser-hotptp/preprocess.ml \
	parser-hotptp/htparser.mly \
	parser-hotptp/htlexer.mll \
\
	interfaces/cmdline.mli \
	interfaces/cmdline.ml \
	interfaces/testproblems.ml \
	interfaces/interactive.mli \
	interfaces/interactive.ml

# Camlp4 syntax extensions
# EXTENSIONS=pa_timed

# All toplevel files:

TARGETS=toplevel/leo.ml \
#	test/termset-test.ml \
#	test/parser-test.ml \
#	test/index-test.ml \
#	test/uni-test.ml \
#	test/polymorphism-test.ml

MAIN_TARGET=toplevel/leo.ml

# Directories of sources, binaries, documentation and extensions:

SRCDIR=./
BINDIR=../bin/
DOCDIR=../doc/html_doc/
# EXTDIR=./extensions/

SOURCES:=$(addprefix $(SRCDIR),$(SOURCES))
TARGETS:=$(addprefix $(SRCDIR),$(TARGETS))

# EXT_ENABLE=true
OPT_P_FLAG=

# ifeq ($(EXT_ENABLE),true)
# 	EXTENSIONS:=$(addprefix $(EXTDIR),$(addsuffix _enabled.ml,$(EXTENSIONS)))
# else
# 	EXTENSIONS:=$(addprefix $(EXTDIR),$(addsuffix _disabled.ml,$(EXTENSIONS)))
# endif

MAIN_OBJS=$(addsuffix .cmo,$(basename $(filter %.ml %.mll %.mly,$(SOURCES))))
MAIN_OBJS_OPT=$(addsuffix .cmx,$(basename $(filter %.ml %.mll %.mly,$(SOURCES))))

INCLUDEDIRS=$(sort $(dir $(SOURCES) $(TARGETS)))
INCLUDES=$(addprefix -I ,$(INCLUDEDIRS))
# PREPROCESS=-pp 'camlp4o -I . $(addsuffix .cmo,$(basename $(EXTENSIONS)))'
OCAMLFLAGS=$(INCLUDES) unix.cma str.cma # $(PREPROCESS)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa str.cmxa # $(PREPROCESS) # add other options for ocamlopt here
OCAMLDOCFLAGS=$(INCLUDES) -d $(DOCDIR) -html -hide Pervasives


# $(addsuffix .cmo,$(basename $(EXTENSIONS))):
# 	$(OCAMLC) -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo -loc loc' -I +camlp4 $(EXTENSIONS)

# $(addsuffix .cmx,$(basename $(EXTENSIONS))):
# 	$(OCAMLOPT) -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo -loc loc' -I +camlp4 $(EXTENSIONS)


$(addprefix $(BINDIR),$(basename $(notdir $(TARGETS)))): $(MAIN_OBJS) $(addsuffix .cmo,$(basename $(filter %$(subst $(BINDIR),,$@).ml,$(TARGETS))))
	mkdir -p $(BINDIR)
	$(OCAMLC) -o $(BINDIR)$(basename $(notdir $@)) $(OCAMLFLAGS) $(MAIN_OBJS)   $(addsuffix .cmo,$(basename $(filter %$(subst $(BINDIR),,$@).ml,$(TARGETS))))

$(addsuffix .opt,$(addprefix $(BINDIR),$(basename $(notdir $(TARGETS))))): $(MAIN_OBJS_OPT) $(addsuffix .cmx,$(basename $(filter %$(subst $(BINDIR),,$(subst .opt,,$@)).ml,$(TARGETS))))
	mkdir -p $(BINDIR)
	$(OCAMLOPT) $(OPT_P_FLAG) -o $(addsuffix .opt,$(BINDIR)$(basename $(notdir $@))) $(OCAMLOPTFLAGS) $(MAIN_OBJS_OPT)   $(addsuffix .cmx,$(basename $(filter %$(subst $(BINDIR),,$(subst .opt,,$@)).ml,$(TARGETS))))


$(basename $(notdir $(TARGETS))):
	make $(BINDIR)$@


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.mll.cmo:
	$(OCAMLLEX) $<
	$(OCAMLC) $(OCAMLFLAGS) -c $(addsuffix .ml,$(basename $<))

.mll.cmx:
	$(OCAMLLEX) $<
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $(addsuffix .ml,$(basename $<))

.mly.cmo: $(addsuffix .cmo,$(basename $(filter %.mll,$(SOURCES))))
	$(OCAMLYACC) $<
	$(OCAMLC) $(OCAMLFLAGS) -c $(addsuffix .mli,$(basename $<))
	$(OCAMLC) $(OCAMLFLAGS) -c $(addsuffix .ml,$(basename $<))

.mly.cmx: $(addsuffix .cmo,$(basename $(filter %.mll,$(SOURCES))))
	$(OCAMLYACC) $<
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $(addsuffix .mli,$(basename $<))
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $(addsuffix .ml,$(basename $<))

.mly.cmi: $(addsuffix .cmo,$(basename $<))

# Build

all:
	make clean
	make allmain

opt:
	make clean
	make optmain

profile:
	make opt OPT_P_FLAG=-p

# allmain: $(addsuffix .cmo,$(basename $(EXTENSIONS))) $(addprefix $(BINDIR),$(basename $(notdir $(TARGETS)))) doc customtop
# optmain: $(addsuffix .cmx,$(basename $(EXTENSIONS))) $(addsuffix .opt,$(addprefix $(BINDIR),$(basename $(notdir $(TARGETS)))))
allmain: $(addprefix $(BINDIR),$(basename $(notdir $(TARGETS)))) doc customtop
optmain: $(addsuffix .opt,$(addprefix $(BINDIR),$(basename $(notdir $(TARGETS)))))

customtop: $(MAIN_OBJS) $(addsuffix .cmi,$(basename $(MAIN_OBJS))) $(addsuffix .cmi,$(basename $(MAIN_TARGET))) $(addsuffix .cmo,$(basename $(MAIN_TARGET)))
	ocamlmktop -o customtop unix.cma str.cma $(MAIN_OBJS) $(addsuffix .cmo,$(basename $(MAIN_TARGET)))
	echo "#!/bin/sh" > mytoplevel
	echo exec ./customtop $(addprefix -I ,$(sort $(dir $(SOURCES) $(MAINOBJS)))) >> mytoplevel
	chmod a+x mytoplevel

# Clean up
clean:
	rm -f $(addprefix $(BINDIR),$(basename $(notdir $(TARGETS))))
	rm -f $(addsuffix .opt,$(addprefix $(BINDIR),$(basename $(notdir $(TARGETS)))))
	rm -f $(addsuffix *.cm[iox],$(INCLUDEDIRS))
#	rm -f $(addsuffix *.cm[iox],$(EXTDIR))
	rm -f $(addsuffix *.o,$(INCLUDEDIRS))
#	rm -f $(addsuffix *.o,$(EXTDIR))
	rm -f $(addsuffix .ml,$(basename $(filter %.mll,$(SOURCES))))
	rm -f $(addsuffix .ml,$(basename $(filter %.mly,$(SOURCES))))
	rm -f $(addsuffix .mli,$(basename $(filter %.mly,$(SOURCES))))
	rm -f customtop
	rm -f mytoplevel
	rm -f .depend

cleandoc:
	rm -f $(addsuffix *.html,$(DOCDIR)) $(addsuffix *.css,$(DOCDIR))

# Documentation generation
doc:

	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(OCAMLDOCFLAGS) $(addsuffix *.mli,$(INCLUDEDIRS))

# Dependencies
# .depend: $(addsuffix .cmo,$(basename $(EXTENSIONS)))
.depend: 
	$(OCAMLDEP) $(INCLUDES) $(addsuffix *.ml,$(INCLUDEDIRS)) $(addsuffix *.mli,$(INCLUDEDIRS))> .depend

#	$(OCAMLDEP) $(INCLUDES) $(PREPROCESS) $(addsuffix *.ml,$(INCLUDEDIRS)) $(addsuffix *.mli,$(INCLUDEDIRS))> .depend

include .depend

