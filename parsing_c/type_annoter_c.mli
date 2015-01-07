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


# 0 "./type_annoter_c.mli"
type namedef =
  | VarOrFunc    of string * Ast_c.exp_type
  | EnumConstant of string * string option

  | TypeDef      of string * Ast_c.fullType
  | StructUnionNameDef of string *
      (Ast_c.structUnion * Ast_c.structType) Ast_c.wrap

  | Macro of string * (Ast_c.define_kind * Ast_c.define_val)

(* have nested scope, so nested list*)
type environment = namedef list list

(* can be set with init_env *)
val initial_env : environment ref
(* ex: config/envos/environment_unix.h, seems to be unused *)
val init_env_unused : Common.filename -> unit



val annotate_type_and_localvar :
  environment -> Ast_c.toplevel list ->
  (Ast_c.toplevel * environment Common.pair) list

(* julia: cocci *)
val annotate_test_expressions :
  Ast_c.toplevel list -> unit



(* !!Annotate via side effects!!. Fill in the type
 * information that was put to None during parsing.
 *)
val annotate_program :
  environment -> Ast_c.toplevel list ->
  (Ast_c.toplevel * environment Common.pair) list

