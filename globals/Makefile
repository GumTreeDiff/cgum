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
TARGET=globals

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

SRC=config.ml flag.ml

LIBS=
INCLUDEDIRS= ../commons

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

OBJS= $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

##############################################################################
# Top rules
##############################################################################
ifneq ($(FEATURE_OCAMLBUILD),yes)
all: $(TARGET).cma

all.opt:
	@$(MAKE) $(TARGET).cmxa BUILD_OPT=yes

$(TARGET).cma: $(LIBS) $(OBJS)
	$(OCAMLC_CMD) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(LIBS:.cma=.cmxa) $(OPTOBJS)
	$(OCAMLOPT_CMD) -a -o $(TARGET).cmxa $(OPTOBJS)
else
all:
	cd .. && $(OCAMLBUILD) globals/globals.cma

all.opt:
	cd .. && $(OCAMLBUILD) globals/globals.cmxa

clean:
	cd .. && $(OCAMLBUILD) -clean
endif

##############################################################################
# Developer rules
##############################################################################
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<
.mli.cmi:
	$(OCAMLC_CMD) -c $<
.ml.cmx:
	$(OCAMLOPT_CMD) -c $<

.ml.mldepend:
	$(OCAMLC_CMD) -i $<

clean:
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ gmon.out #*#
	rm -f .depend

distclean: clean

.PHONEY: depend
.depend depend:
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif
