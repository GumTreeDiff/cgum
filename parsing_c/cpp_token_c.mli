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


# 0 "./cpp_token_c.mli"
(* Expanding or extracting macros, at the token level *)

(* corresponds to what is in the yacfe configuration file (e.g. standard.h) *)
type define_def = string * define_param * define_body
 and define_param =
   | NoParam
   | Params of define_arg list
 and define_arg = FixedArg of string | VariadicArg of string
 and define_body =
   | DefineBody of Parser_c.token list
   | DefineHint of parsinghack_hint

   (* strongly corresponds to the TMacroXxx in the grammar and lexer and the
    * MacroXxx in the ast.
    *)
   and parsinghack_hint =
     | HintIterator
     | HintDeclarator
     | HintMacroString
     | HintMacroStatement
     | HintAttribute
     | HintMacroIdentBuilder

val string_of_define_def: define_def -> string

(* This function work by side effect and may generate new tokens
 * in the new_tokens_before field of the token_extended in the
 * paren_grouped list. So don't forget to recall
 * Token_views_c.rebuild_tokens_extented after this call, as well
 * as probably insert_virtual_positions as new tokens
 * are generated.
 *
 * note: it does not do some fixpoint, so the generated code may also
 * contain some macros names.
 *)
val apply_macro_defs:
  msg_apply_known_macro:(string -> unit) ->
  msg_apply_known_macro_hint:(string -> unit) ->
  ?evaluate_concatop:bool ->
  ?inplace_when_single:bool ->
  (string, define_def) Hashtbl.t ->
  Token_views_c.paren_grouped list -> unit

(* extracting define_def, e.g. from a standard.h; assume have called
 * fix_tokens_define before to have the TDefEol *)
val extract_macros :
  Parser_c.token list -> (string, define_def) Common.assoc
