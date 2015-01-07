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


# 0 "./oset.mli"
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


# 0 "./oset.mli"
class virtual ['a] oset :
object ('o)
  inherit ['a] Ocollection.ocollection

  method cardinal : int

  method virtual inter : 'o -> 'o
  method virtual minus : 'o -> 'o
  method virtual union : 'o -> 'o

  method is_singleton : bool
  method is_subset_of : 'o -> bool
  method is_equal : 'o -> bool

  method virtual toset : 'd
  method tosetb : 'a Setb.t
  method toseti : Seti.seti
  method tosetpt : SetPt.t
end

val ( $??$ )  : 'a -> < mem : 'a -> bool; .. > -> bool
val ( $++$ )  : < union : 'a -> 'o; .. > -> 'a -> 'o
val ( $**$ )  : < inter : 'a -> 'o; .. > -> 'a -> 'o
val ( $--$ )  : < minus : 'a -> 'o; .. > -> 'a -> 'o
val ( $<<=$ ) : < is_subset_of : 'a -> bool; .. > -> 'a -> bool
val ( $==$ )  : < is_equal : 'a -> bool; .. > -> 'a -> bool

val mapo : ('a -> 'o) -> 'o oset -> 'a oset -> 'o oset
