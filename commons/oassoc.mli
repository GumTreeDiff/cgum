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


# 0 "./oassoc.mli"
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


# 0 "./oassoc.mli"

class virtual ['a, 'b] oassoc :
object ('o)
  inherit ['a * 'b] Ocollection.ocollection

  method virtual assoc : 'a -> 'b
  method virtual delkey : 'a -> 'o

  (* may raise NotFound *)
  method find : 'a -> 'b
  method find_opt: 'a -> 'b option

  method haskey : 'a -> bool
  method replkey : 'a * 'b -> 'o

  (* better to implement it yourself *)
  method virtual keys: 'a list

  method apply : 'a -> ('b -> 'b) -> 'o
  method apply_with_default : 'a -> ('b -> 'b) -> (unit -> 'b) -> 'o

  (* effect version *)
  method apply_with_default2 : 'a -> ('b -> 'b) -> (unit -> 'b) -> unit

end
