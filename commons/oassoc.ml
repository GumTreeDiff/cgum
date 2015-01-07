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


# 0 "./oassoc.ml"
open Common

open Ocollection

(* assoc, also called map or dictionnary *)
class virtual ['a,'b] oassoc =
object(o: 'o)
  inherit ['a * 'b] ocollection

  method virtual assoc: 'a -> 'b
  method virtual delkey: 'a -> 'o

  (* pre: must be in *)
  method replkey: ('a * 'b) -> 'o =
    fun (k,v) -> o#add (k,v)

  (* pre: must not be in *)
  (* method add: ('a * 'b) -> 'o = *)

  (*
    method keys =
    List.map fst (o#tolist)
  *)
  method virtual keys: 'a list (* or 'a oset ? *)

  method find: 'a -> 'b = fun k ->
    o#assoc k

  method find_opt: 'a -> 'b option = fun k ->
    try
      let res = o#assoc k in
      Some res
    with Not_found -> None

  method haskey: 'a -> bool = fun k ->
    try (ignore(o#assoc k); true)
    with Not_found -> false

  method apply: 'a -> ('b -> 'b) -> 'o = fun k f ->
    let old = o#assoc k in
    o#replkey (k, f old)

  (* apply default, assoc_default, take in class parameters a default value *)
  method apply_with_default: 'a -> ('b -> 'b) -> (unit -> 'b) -> 'o =
    fun k f default ->
      let old =
        try o#assoc k
        with Not_found -> default ()
      in
      o#replkey (k, f old)

  method apply_with_default2 = fun k f default ->
    o#apply_with_default k f default +> ignore


end
