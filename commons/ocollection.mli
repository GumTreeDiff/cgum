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


# 0 "./ocollection.mli"
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


# 0 "./ocollection.mli"
type ('a, 'b) view =
  | Empty
  | Cons of 'a * 'b

class virtual ['a] ocollection :
object ('o)
  inherit Objet.objet

  method virtual empty : 'o
  method virtual add : 'a -> 'o

  method virtual iter : ('a -> unit) -> unit
  method virtual view : ('a, 'o) view

  (* no need virtual, but better to force redefine for efficiency *)
  method virtual del : 'a -> 'o
  method virtual mem : 'a -> bool
  method virtual null : bool

  (* effect version *)
  method add2: 'a -> unit
  method del2: 'a -> unit
  method clear: unit


  method fold : ('c -> 'a -> 'c) -> 'c -> 'c

  method fromlist : 'a list -> 'o
  method tolist : 'a list

  method exists : ('a -> bool) -> bool
  method filter : ('a -> bool) -> 'o

  method length : int

  method getone : 'a
  method others : 'o
end
