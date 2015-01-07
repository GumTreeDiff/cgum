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
TARGET=parsing_c

# - type_cocci.ml ast_cocci.ml  # + unparse_hrule
SRC= token_annot.ml flag_parsing_c.ml parsing_stat.ml \
 token_c.ml ast_c.ml \
 visitor_c.ml lib_parsing_c.ml \
 pretty_print_c.ml xmlify.ml \
 semantic_c.ml lexer_parser.ml parser_c.ml lexer_c.ml \
 parse_string_c.ml token_helpers.ml token_views_c.ml \
 cpp_token_c.ml  \
 parsing_hacks.ml \
 cpp_analysis_c.ml \
 parsing_recovery_c.ml parsing_consistency_c.ml \
 parse_c.ml type_c.ml \
 cpp_ast_c.ml \
 type_annoter_c.ml uncommentify.ml

LIBS=../commons/commons.cma ../globals/globals.cma

INCLUDESDEP= -I ../commons -I ../commons/ocamlextra -I ../commons/ocollection \
  -I ../globals

INCLUDES=$(INCLUDESDEP) $(TARZANINCLUDE)

SYSLIBS= str.cma unix.cma num.cma

##############################################################################
# Generic variables
##############################################################################

#for warning:  -w A
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDESDEP)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
ifneq ($(FEATURE_OCAMLBUILD),yes)
all: $(TARGET).cma
all.opt:
	@$(MAKE) $(TARGET).cmxa BUILD_OPT=yes

$(TARGET).cma: $(OBJS)
	$(OCAMLC_CMD) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT_CMD) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP_CMD) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top

lexer_c.ml: lexer_c.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_c.ml

parser_c.ml parser_c.mli: parser_c.mly
	$(OCAMLYACC) -v $<
clean::
	rm -f parser_c.ml parser_c.mli parser_c.output
else
all:
	cd .. && $(OCAMLBUILD) parsing_c/parsing_c.cma

all.opt:
	cd .. && $(OCAMLBUILD) parsing_c/parsing_c.cmxa

clean::
	cd .. && $(OCAMLBUILD) -clean
endif


##############################################################################
# Generic rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<
.mli.cmi:
	$(OCAMLC_CMD) -c $<
.ml.cmx:
	$(OCAMLOPT_CMD) -c $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ gmon.out #*#
	rm -f .depend

distclean: clean

.depend depend: lexer_c.ml parser_c.ml parser_c.mli
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif
