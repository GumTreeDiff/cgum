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


# 0 "./lexer_parser.mli"

val _handle_typedef : bool ref

val enable_typedef  : unit -> unit
val disable_typedef : unit -> unit
val is_enabled_typedef : unit -> bool



(* private *)
type identkind = TypeDefI | IdentI
val _typedef : (string, identkind) Common.scoped_h_env ref

val add_ident   : string -> unit
val add_typedef : string -> unit

val add_typedef_root : string -> unit

val new_scope : unit -> unit
val del_scope : unit -> unit

val is_typedef : string -> bool

val lexer_reset_typedef :
    (string, identkind) Common.scoped_h_env option (* known typedefs *) -> unit

val _old_state : (string, identkind) Common.scoped_h_env ref
val save_typedef_state : unit -> unit
val restore_typedef_state : unit -> unit


type context =
  | InTopLevel
  | InFunction
  | InStruct
  | InParameter
  | InInitializer
  | InEnum

type lexer_hint = {
  mutable context_stack: context Common.stack;
 }

val _lexer_hint : lexer_hint ref
val current_context: unit -> context
val push_context: context -> unit
val pop_context: unit -> unit

val default_hint : unit -> lexer_hint

val is_top_or_struct : context -> bool

