# Copyright 2014, INRIA
# Julia Lawall
# This file is part of Cgen.  Much of it comes from Coccinelle, which is
# also available under the GPLv2 license
#
# Cgen is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, according to version 2 of the License.
#
# Cgen is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Cgen.  If not, see <http://www.gnu.org/licenses/>.
#
# The authors reserve the right to distribute this or future versions of
# Cgen under other licenses.


# Copyright 2013, Inria
# Suman Saha, Julia Lawall, Gilles Muller
# This file is part of Cgum.
#
# Cgum is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, according to version 2 of the License.
#
# Cgum is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Cgum.  If not, see <http://www.gnu.org/licenses/>.
#
# The authors reserve the right to distribute this or future versions of
# Cgum under other licenses.


include ../make_config

##############################################################################
# Variables
##############################################################################

TARGET=commons

# note: if you add a file (a .mli or .ml), dont forget to redo a 'make depend'
MYSRC=	commands.ml                              \
	common.ml common_extra.ml                \
	objet.ml                                 \
	ocollection.ml                           \
	seti.ml                                  \
	oset.ml oassoc.ml osequence.ml ograph.ml \
	ocollection/oseti.ml                     \
	ocollection/oseth.ml                     \
	ocollection/osetb.ml                     \
	ocollection/osetpt.ml                    \
	ocollection/oassocb.ml			 \
	ocollection/oassoch.ml			 \
	ocollection/oassoc_buffer.ml		 \
	ocollection/oassoc_cache.ml		 \
	ocollection/oassocid.ml			 \
	oarray.ml				 \
	ocollection/ograph2way.ml		 \
	ograph_simple.ml

# src from other authors, got from the web or caml hump
SRC=ocamlextra/dumper.ml
SRC+=ocamlextra/ANSITerminal.ml
SRC+=ocamlextra/setb.ml ocamlextra/mapb.ml # defunctorized version of standard set/map
SRC+=ocamlextra/setPt.ml
SRC+=$(MYSRC)
SRC+=ocamlextra/enum.ml ocamlextra/dynArray.ml
SRC+=ocamlextra/suffix_tree.ml ocamlextra/suffix_tree_ext.ml

SYSLIBS=str.cma bigarray.cma unix.cma

INCLUDEDIRS=ocamlextra ocollection
SUBDIRS=ocamlextra ocollection

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %) $(INCLUDESEXTRA)

##############################################################################
# Generic ocaml variables
##############################################################################

# This flag can also be used in subdirectories so don't change its name here.
# For profiling use:  -p -inline 0
OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

# The OPTBIN variable is here to allow to use ocamlc.opt instead of
# ocaml, when it is available, which speeds up compilation. So
# if you want the fast version of the ocaml chain tools, set this var
# or setenv it to ".opt" in your startup script.
OPTBIN ?= #.opt

# The OCaml tools.
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

##############################################################################
# Top rules
##############################################################################
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


ifneq ($(FEATURE_OCAMLBUILD),yes)
all: $(LIB)
all.opt:
	@$(MAKE) $(OPTLIB) BUILD_OPT=yes
opt: all.opt
top: $(TARGET).top

$(LIB): $(OBJS)
	$(OCAMLC_CMD) -a -o $@ $^

$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT_CMD) -a -o $@ $^

$(TARGET).top: $(OBJS)
	$(OCAMLMKTOP_CMD) -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET).top
else
all:
	cd .. && $(OCAMLBUILD) commons/commons.cma
all.opt:
	cd .. && $(OCAMLBUILD) commons/commons.cmxa
clean::
	cd .. && $(OCAMLBUILD) -clean
endif

##############################################################################
# Other commons libs target
##############################################################################

all_libs:

##############################################################################
# The global "features" lib wrapper
##############################################################################

features: commons_features.cma
features.opt: commons_features.cmxa

commons_features.cma: features.cmo
	$(OCAMLC_CMD) -a -o $@ $^

commons_features.cmxa: features.cmx
	$(OCAMLOPT_CMD) -a -o $@ $^


##############################################################################
# Developer rules
##############################################################################

tags:
	otags -no-mli-tags -r  .

clean::
	rm -f gmon.out

forprofiling:
	$(MAKE) OPTFLAGS="-p -inline 0 " opt

dependencygraph:
	ocamldep *.mli *.ml > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps

dependencygraph2:
	find  -name "*.ml" |grep -v "scripts" | xargs ocamldep -I commons -I globals -I ctl -I parsing_cocci -I parsing_c -I engine -I popl -I extra > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps


##############################################################################
# Generic rules
##############################################################################
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD)  -c $<
.mli.cmi:
	$(OCAMLC_CMD)  -c $<
.ml.cmx:
	$(OCAMLOPT_CMD)  -c $<

clean::
	rm -f *.cm[iox] *.o *.a *.cma *.cmxa *.annot
	rm -f *~ .*~ #*#

clean::
	for i in $(SUBDIRS); do (cd $$i; \
        rm -f *.cm[iox] *.o *.a *.cma *.cmxa *.annot *~ .*~ ; \
        cd ..; ) \
	done
	rm -f .depend

distclean: clean
	rm -f commands.ml

.PHONEY: depend
.depend depend:
	$(OCAMLDEP_CMD) *.mli *.ml  > .depend
	for i in $(SUBDIRS); do $(OCAMLDEP_CMD) $$i/*.ml $$i/*.mli >> .depend; done

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif
