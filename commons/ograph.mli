(*
 * Copyright 2014, INRIA
 * Julia Lawall
 * This file is part of Cgen.  Much of it comes from Coccinelle, which is
 * also available under the GPLv2 license
 *
 * Cgen is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Cgen is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Cgen.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Cgen under other licenses.
 *)


# 0 "./ograph.mli"
(*
 * Copyright 2013, Inria
 * Suman Saha, Julia Lawall, Gilles Muller
 * This file is part of Cgum.
 *
 * Cgum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Cgum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Cgum.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Cgum under other licenses.
 *)


# 0 "./ograph.mli"
class virtual ['a] ograph :
object ('o)
  method virtual empty : 'o

  method virtual add_node : 'a -> 'o
  method virtual del_node : 'a -> 'o

  method virtual add_arc : 'a * 'a -> 'o
  method virtual del_arc : 'a * 'a -> 'o


  method virtual nodes : 'a Oset.oset
  method virtual predecessors : 'a -> 'a Oset.oset
  method virtual successors : 'a -> 'a Oset.oset

  method virtual ancestors : 'a Oset.oset -> 'a Oset.oset
  method virtual brothers : 'a -> 'a Oset.oset
  method virtual children : 'a Oset.oset -> 'a Oset.oset

  method mydebug : ('a * 'a list) list
end
