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


# 0 "./token_annot.ml"
(* Provides a dictionary of possible annotations on tokens, indexed by keys.
 *
 * The purpose of these annotations is to direct the pretty printing of
 * tokens. The annotations can be set by AST transformations.
 *
 * Assumptions: only a few tokens have annotations, and those have only
 * a few of them.
 *)

type annot_key =
    Exclude_start
  | Exclude_end

type annot_val =
  Unit

(* A linked list should offer a good tradeoff between space usage
 * and lookup overhead given our assumptions.
 *)
type annots = (annot_key * annot_val) list

let empty = []

let get_annot anns key =
  if List.mem_assoc key anns
  then Some (List.assoc key anns)
  else None

let put_annot key value anns =
  (key, value) :: anns
