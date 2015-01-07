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


# 0 "./flag.ml"
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

let sgrep_mode2 = ref false

let show_misc = ref true

let show_transinfo = ref false

let show_trying = ref false

let track_iso_usage = ref false

type scanner = IdUtils | Glimpse | Grep | Google of string | NoScanner
let scanner = ref Grep

let pyoutput = ref "coccilib.output.Console"

(*"Some" value is the path with respect to which the patch should be created*)
let patch = ref (None : string option)

let make_hrule = ref (None : string (*dir*) option)
let hrule_per_file = ref true (* if false, then a rule per function *)

let currentfile = ref (None : string option)

let current_element = ref ""
let dir = ref ""

let defined_virtual_rules = ref ([] : string list)
let defined_virtual_env = ref ([] : (string*string) list)

let set_defined_virtual_rules s =
  match Str.split (Str.regexp "=") s with
    [name;vl] -> defined_virtual_env := (name,vl) :: !defined_virtual_env
  | _ -> defined_virtual_rules := s :: !defined_virtual_rules

let c_plus_plus = ref false

(* was in main *)
let include_headers = ref false

let type_info = ref false
