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


# 0 "./oarray.ml"
open Common

open Osequence

(* growing array ? initialise with None,
 * and generate exception when not defined or have an arraydefault
 * update: can use dynArray ?
 *)

(* !!take care!!, this is not a pure data structure *)
class ['a] oarray n el =
object(o: 'o)
  inherit ['a] osequence

  val data = Array.make n el

  method empty = raise Todo
  method add (i,v)  =
    Array.set data i v;
    o

  method iter f =
    Array.iteri (curry f) data
  method view = raise Todo

  method assoc i =
    Array.get data i

  method null = raise Todo
  method nth = raise Todo
  method mem = raise Todo
  method last = raise Todo
  method first = raise Todo
  method delkey = raise Todo

  method keys = raise Todo

  method del = raise Todo
  method fromlist = raise Todo
  method length =
    Array.length data

  (* method create: int -> 'a -> 'o =
    raise Todo
  *)
  (* method put: make more explicit the fact that array do side effect *)
end

