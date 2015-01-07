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


# 0 "./token_helpers.mli"

val is_space               : Parser_c.token -> bool
val is_just_comment        : Parser_c.token -> bool
val is_just_comment_or_space    : Parser_c.token -> bool

val is_comment             : Parser_c.token -> bool
val is_not_comment         : Parser_c.token -> bool

val is_real_comment        : Parser_c.token -> bool
val is_fake_comment        : Parser_c.token -> bool
val is_not_in_ast          : Parser_c.token -> bool


val is_cpp_instruction     : Parser_c.token -> bool
val is_gcc_token           : Parser_c.token -> bool

val is_eof                 : Parser_c.token -> bool
val is_eom                 : Parser_c.token -> bool

val is_statement           : Parser_c.token -> bool
val is_start_of_something  : Parser_c.token -> bool
val is_binary_operator     : Parser_c.token -> bool
val is_stuff_taking_parenthized : Parser_c.token -> bool

val is_opar : Parser_c.token -> bool
val is_cpar : Parser_c.token -> bool
val is_obrace : Parser_c.token -> bool
val is_cbrace : Parser_c.token -> bool

val is_ident_like: Parser_c.token -> bool

(* ---------------------------------------------------------------------- *)
val info_of_tok : Parser_c.token -> Ast_c.info

val visitor_info_of_tok :
  (Ast_c.info -> Ast_c.info) -> Parser_c.token -> Parser_c.token

(* ---------------------------------------------------------------------- *)
val linecol_of_tok : Parser_c.token -> int * int
val col_of_tok     : Parser_c.token -> int
val line_of_tok    : Parser_c.token -> int
val pos_of_tok     : Parser_c.token -> int
val str_of_tok     : Parser_c.token -> string
val file_of_tok    : Parser_c.token -> Common.filename
val pinfo_of_tok   : Parser_c.token -> Ast_c.parse_info

(* val mark_of_tok    : Parser_c.token -> Ast_c.mark_token *)
val is_origin : Parser_c.token -> bool
val is_expanded : Parser_c.token -> bool
val is_fake : Parser_c.token -> bool
val is_abstract : Parser_c.token -> bool

val is_same_line_or_close: int -> Parser_c.token -> bool
