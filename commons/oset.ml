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


# 0 "./oset.ml"
open Common

open Ocollection

class virtual ['a] oset =
object(o: 'o)
  inherit ['a] ocollection

  (* no need virtual, but better to redefine (efficiency) *)
  method virtual union: 'o -> 'o
  method virtual inter: 'o -> 'o
  method virtual minus: 'o -> 'o

  (* allow binary methods tricks, generate exception when not good type *)
  method  tosetb: 'a Setb.t = raise (Impossible 10)
  method  tosetpt: SetPt.t = raise (Impossible 11)
  method  toseti: Seti.seti    = raise (Impossible 12)
  method virtual toset: 'b. 'b (* generic (not safe) tricks *)

  (* is_intersect, equal, subset *)
  method is_subset_of: 'o -> bool = fun o2 ->
    ((o2#minus o)#cardinal >= 0) && ((o#minus o2)#cardinal =|= 0)

  method is_equal: 'o -> bool = fun o2 ->
    ((o2#minus o)#cardinal =|= 0) && ((o#minus o2)#cardinal =|= 0)


  method is_singleton: bool = (* can be short circuited *)
    o#length =|= 1
  method cardinal: int = (* just to keep naming conventions *)
    o#length
      (* dont work:
         method big_union: 'b. ('a -> 'b oset) -> 'b oset = fun f -> todo()
      *)

end

let ($??$) e xs = xs#mem e
let ($++$) xs ys = xs#union ys
let ($**$) xs ys = xs#inter ys
let ($--$) xs ys = xs#minus ys
let ($<<=$) xs ys = xs#is_subset_of ys
let ($==$) xs ys = xs#is_equal ys

(* todo: pas beau le seed.  I dont put the type otherwise have to
 * put explicit :>
 *)
let (mapo: ('a -> 'b) -> 'b oset -> 'a oset -> 'b oset) = fun f seed xs ->
  xs#fold (fun acc x -> acc#add (f x)) seed

