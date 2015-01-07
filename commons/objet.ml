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


# 0 "./objet.ml"
(* Yoann Padioleau, Julia Lawall
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2007, 2008, 2009 University of Urbana Champaign and DIKU
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common

(* TypeClass via objects. Cf also now interfaces.ml
 *
 * todo? get more inspiration from Java to put fundamental interfaces
 * here ? such as cloneable, equaable, showable, debugable, etc
 *)

class virtual objet =
object(o:'o)
  method invariant: unit -> unit = fun () ->
    raise Todo
  (* method check: unit -> unit = fun () ->
    assert(o#invariant());
  *)

  method of_string: string -> unit =
    raise Todo
  method to_string: unit -> string =
    raise Todo
  method debug: unit -> unit =
    raise Todo

  method misc_op_hook: unit -> 'o =
    raise Todo

  method misc_op_hook2: unit =
    ()
end
