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


#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

VERSION=$(shell cat globals/config.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')

##############################################################################
# Variables
##############################################################################
TARGET=cgum


SRC=flag_cocci.ml main.ml

OPTLIBFLAGS=

SEXPSYSCMA=bigarray.cma nums.cma

SYSLIBS=str.cma unix.cma $(SEXPSYSCMA)
LIBS=commons/commons.cma \
     globals/globals.cma \
     parsing_c/parsing_c.cma

#used for clean: and depend: and a little for rec & rec.opt
MAKESUBDIRS=commons \
 globals \
 parsing_c
INCLUDEDIRS=commons commons/ocamlextra \
 globals \
 parsing_c

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)

OBJS=    $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

EXEC=$(TARGET)

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLCFLAGS= -g # -dtypes # -w A

# for profiling add  -p -inline 0
# but 'make forprofiling' below does that for you.
# This flag is also used in subdirectories so don't change its name here.
# To enable backtrace support for native code, you need to put -g in OPTFLAGS
# to also link with -g, but even in 3.11 the backtrace support seems buggy so
# not worth it.
OPTFLAGS=-g
# the following is essential for Coccinelle to compile under gentoo
# but is now defined above in this file

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)

# can also be set via 'make static'
STATIC= #-ccopt -static

# can also be unset via 'make purebytecode'
BYTECODE_STATIC=-custom

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt byte opt top clean distclean configure
.PHONY:: $(MAKESUBDIRS) $(MAKESUBDIRS:%=%.opt) subdirs subdirs.opt

all: Makefile.config byte

opt: all.opt
all.opt: opt-compil

world:
	$(MAKE) byte
	$(MAKE) opt-compil

byte: .depend
	$(MAKE) subdirs
	$(MAKE) $(EXEC)

opt-compil: .depend
	$(MAKE) subdirs.opt
	$(MAKE) $(EXEC).opt

top: $(EXEC).top

subdirs:
	$(MAKE) -C commons OCAMLCFLAGS="$(OCAMLCFLAGS)" OPTFLAGS="$(OPTFLAGS)"
	+for D in $(MAKESUBDIRS); do $(MAKE) $$D || exit 1 ; done

subdirs.opt:
	$(MAKE) -C commons all.opt OCAMLCFLAGS="$(OCAMLCFLAGS)" OPTFLAGS="$(OPTFLAGS)"
	+for D in $(MAKESUBDIRS); do $(MAKE) $$D.opt || exit 1 ; done

$(MAKESUBDIRS):
	$(MAKE) -C $@ OCAMLCFLAGS="$(OCAMLCFLAGS)" OPTFLAGS="$(OPTFLAGS)" all

$(MAKESUBDIRS:%=%.opt):
	$(MAKE) -C $(@:%.opt=%) OCAMLCFLAGS="$(OCAMLCFLAGS)" OPTFLAGS="$(OPTFLAGS)" all.opt

#dependencies:
# commons:
# globals:
# menhirlib:
# parsing_cocci: commons globals menhirlib
# parsing_c:parsing_cocci
# ctl:globals commons
# engine: parsing_cocci parsing_c ctl
# popl09:engine
# extra: parsing_cocci parsing_c ctl

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done

$(LIBS): $(MAKESUBDIRS)
$(LIBS:.cma=.cmxa): $(MAKESUBDIRS:%=%.opt)

$(OBJS):$(LIBS)
$(OPTOBJS):$(LIBS:.cma=.cmxa)

$(EXEC): $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS)  $^

$(EXEC).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS)
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $(OPTLIBFLAGS)  $^

$(EXEC).top: $(LIBS) $(OBJS)
	$(OCAMLMKTOP) -custom -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top


.PHONY:: configure

configure:
	./configure

Makefile.config:    
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1

static:
	rm -f spatch.opt spatch
	$(MAKE) STATIC="-ccopt -static" spatch.opt
	cp spatch.opt spatch

purebytecode:
	rm -f spatch.opt spatch
	$(MAKE) BYTECODE_STATIC="" spatch

##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC)    -c $<
.mli.cmi:
	$(OCAMLC)    -c $<
.ml.cmx:
	$(OCAMLOPT)  -c $<

.ml.mldepend:
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ *.exe #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done

.depend::
	@if [ ! -f .depend ] ; then $(MAKE) depend ; fi

-include .depend

-include Makefile.local

TOLICENSIFY=$(MAKESUBDIRS)

licensify:
	ocaml str.cma tools/licensify.ml
	set -e; for i in $(TOLICENSIFY); do cd $$i; ocaml str.cma ../tools/licensify.ml; cd ..; done
