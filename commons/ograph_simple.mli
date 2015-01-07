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


# 0 "./ograph_simple.mli"
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


# 0 "./ograph_simple.mli"
open Common


(* essentially a convenient way to access a hash and its reverse hash *)

class ['key, 'node, 'edge] ograph_mutable :
object ('o)
  method add_node : 'key -> 'node -> unit
  method del_node : 'key -> unit
  method replace_node : 'key -> 'node -> unit


  method add_arc : ('key * 'key) -> 'edge -> unit
  method del_arc : ('key * 'key) -> 'edge -> unit

  method nodes : ('key, 'node) Oassoc.oassoc

  method successors : 'key -> ('key * 'edge) Oset.oset
  method predecessors : 'key -> ('key * 'edge) Oset.oset
  method allsuccessors : ('key, ('key * 'edge) Oset.oset) Oassoc.oassoc


  method del_leaf_node_and_its_edges : 'key -> unit

  method ancestors : 'key -> 'key Oset.oset
  method leaf_nodes : unit -> 'key Oset.oset

end

val print_ograph_generic:
  str_of_key:('key -> string) ->
  str_of_node:('key -> 'node -> string) ->
  Common.filename ->
  ('key, 'node,'edge) ograph_mutable ->
  unit
