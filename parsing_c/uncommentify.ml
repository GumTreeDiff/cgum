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


# 0 "./uncommentify.ml"
(* Yoann Padioleau, Julia Lawall
 *
 * Copyright (C) 2012, INRIA.
 * Copyright (C) 2010, 2011, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2006, 2007, 2008, 2009 Ecole des Mines de Nantes and DIKU
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *
 *
 * Extracted from coccinelle/parsing_c/unparse_c.ml
 *)
open Common

module TH = Token_helpers

(* should keep comments and directives in between adjacent deleted terms,
but not comments and directives within deleted terms.  should use the
labels found in the control-flow graph *)



(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = mk_pr2_wrappers Flag_parsing_c.verbose_unparsing

(*****************************************************************************)
(* Types used during the intermediate phases of the unparsing *)
(*****************************************************************************)

type token1 =
  | Fake1 of Ast_c.info
  | T1 of Parser_c.token

(* The cocci_tag of the token should always be a NOTHING. The mark of
 * the token can only be OriginTok or ExpandedTok. Why not get rid of
 * token and get something simpler ? because we need to know if the
 * info is a TCommentCpp or TCommentSpace, etc for some of the further
 * analysis so easier to keep with the token.
 *
 * This type contains the whole information. Have all the tokens with this
 * type.
 *)
type min =
  | Ctx

type token2 =
  | T2 of Parser_c.token * min
        * int option (* orig index, abstracting away comments and space *)
        * unit option
  | Fake2 of Ast_c.info * min
  | C2 of string
  | Comma of string

(* not used yet *)
type token3 =
  | T3 of Parser_c.token
  | C3 of string


(* similar to the tech in parsing_hack *)
type token_extended =
  { tok2 : token2;
    str  : string;
    idx  : int option; (* to know if 2 tokens were consecutive in orig file *)
    mutable new_tokens_before : token2 list;
    mutable remove : bool;
  }


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let info_of_token1 t =
  match t with
  | Fake1 info -> info
  | T1 tok -> TH.info_of_tok tok

let print_token1 = function
  | T1 tok -> TH.str_of_tok tok
  | Fake1 info -> "fake"

let str_of_token2 = function
  | T2 (t,_,_,_) -> TH.str_of_tok t
  | C2 s -> s
  | Comma s -> s
  | Fake2 _ -> ""

let print_token2 = function
  | T2 (t,b,_,_) ->
    let t_str =
      match t with
      | Parser_c.TCommentSpace _ -> " sp "
      | Parser_c.TCommentNewline _ -> " nl "
      | Parser_c.TCommentCpp _ -> " cp "
      | Parser_c.TCommentMisc _ -> " misc "
      | Parser_c.TComment _ -> " comment "
      | _ -> "" in
    let b_str =
      match b with
      | Ctx -> "" in
    "T2:"^b_str^t_str^TH.str_of_tok t
  | Fake2 (_,b) ->
    let b_str =
      match b with
      | Ctx -> "" in
    b_str^"fake"
  | C2 s -> "C2:"^s
  | Comma s -> "Comma:"^s

let str_of_token3 = function
  | T3 t -> TH.str_of_tok t
  | C3 s -> s

let simple_print_all_tokens pr s l =
  Printf.printf "%s\n" s;
  List.iter (function x -> Printf.printf "|%s| " (pr x)) l;
  Printf.printf "\n"
let simple_print_all_tokens1 = simple_print_all_tokens print_token1
let simple_print_all_tokens2 = simple_print_all_tokens print_token2
let simple_print_all_tokens3 = simple_print_all_tokens str_of_token3

let mk_token_extended x =
  let origidx =
    match x with
    | T2 (_,_,idx,_) -> idx
    | _ -> None in
  { tok2 = x;
    str = str_of_token2 x;
    idx = origidx;
    new_tokens_before = [];
    remove = false;
  }

let rebuild_tokens_extented toks_ext =
  let _tokens = ref [] in
  toks_ext +> List.iter (fun tok ->
    tok.new_tokens_before +> List.iter (fun x -> push2 x _tokens);
    if not tok.remove
    then push2 tok.tok2 _tokens);
  let tokens = List.rev !_tokens in
  (tokens +> List.map mk_token_extended)

(*****************************************************************************)
(* Last fix on the ast *)
(*****************************************************************************)

(* Because of the ugly trick to handle initialiser, I generate fake ','
 * for the last initializer element, but if there is nothing around it,
 * I don't want in the end to print it.
 *)

let remove_useless_fakeInfo_struct program =
  let bigf = { Visitor_c.default_visitor_c_s with
    Visitor_c.kini_s = (fun (k,bigf) ini ->
      match k ini with
      | Ast_c.InitList args, ii ->
        (match ii with
        | [_;_] -> ini
        | i1 :: i2 :: iicommaopt :: tl when
            (Ast_c.is_fake iicommaopt) ->
          (* sometimes the guy put a normal iicommaopt *)
          Ast_c.InitList args, (i1 :: i2 :: tl)
        | ii -> Ast_c.InitList args, ii
        )
      | x -> x)
    } in
  Visitor_c.vk_toplevel_s bigf program


(*****************************************************************************)
(* Tokens1 generation *)
(*****************************************************************************)

let get_fakeInfo_and_tokens celem toks =

  let toks_in  = ref toks in
  let toks_out = ref [] in

  (* todo? verify good order of position ? *)
  let pr_elem info =
    match Ast_c.pinfo_of_info info with
    | Ast_c.FakeTok _ ->
      push2 (Fake1 info) toks_out
    | Ast_c.OriginTok _ | Ast_c.ExpandedTok _ ->
      (* get the associated comments/space/cppcomment tokens *)
      let (before, x, after) =
        !toks_in +> split_when (fun tok ->
          info =*= TH.info_of_tok tok)
      in
      assert(info =*= TH.info_of_tok x);
      (*old: assert(before +> List.for_all (TH.is_comment)); *)
      before +> List.iter (fun x ->
        if not (TH.is_comment x)
        then pr2 ("WEIRD: not a comment:" ^ TH.str_of_tok x)
        (* case such as  int asm d3("x"); not yet in ast *)
        );
      before +> List.iter (fun x -> push2 (T1 x) toks_out);

      push2 (T1 x) toks_out;
      toks_in := after;
    | Ast_c.AbstractLineTok _ ->
      (* can be called on type info when for instance use -type_c *)
      if !Flag_parsing_c.pretty_print_type_info
      then push2 (Fake1 info) toks_out
      else raise (Impossible 134) (* at this stage *)
  in

  let pr_space _ = () in (* use the spacing that is there already *)

  Pretty_print_c.pp_program_gen pr_elem pr_space celem;

  if not (null !toks_in)
  then failwith "WEIRD: unparsing not finished";

  List.rev !toks_out

(* Fake nodes that have BEFORE code or are - should be moved over any subsequent
whitespace and newlines, but not any comments, to get as close to the affected
code as possible.  Similarly, fake nodes that have AFTER code should be moved
backwards.  No fake nodes should have both before and after code. *)

let displace_fake_nodes toks =
  let is_fake = function Fake1 _ -> true | _ -> false in
  let rec loop toks =
    let fake_info =
      try Some (split_when is_fake toks)
      with Not_found -> None in
    match fake_info with
    | Some(bef,((Fake1 info) as fake),aft) ->
        bef @ fake :: (loop aft)
    | None -> toks
    | _ -> raise (Impossible 135) in
  loop toks

(*****************************************************************************)
(* Tokens2 generation *)
(*****************************************************************************)

let comment2t2 = function
  | (Token_c.TCommentCpp
  (* not sure iif the following list is exhaustive or complete *)
    (Token_c.CppAttr|Token_c.CppMacro|Token_c.CppPassingCosWouldGetError),
    (info : Token_c.info)) ->
    C2(info.Common.str)
  | (Token_c.TCommentCpp x,(info : Token_c.info)) ->
    C2("\n"^info.Common.str^"\n")
  | x -> failwith (Printf.sprintf "unexpected comment %s" (Dumper.dump x))

let expand_mcode toks =
  let toks_out = ref [] in

  let index = ref 0 in

  let add_elem t minus =
    match t with
    | Fake1 info ->
      let str = Ast_c.str_of_info info in
      (* don't add fake string if the thing should be removed *)
      if str =$= ""
      then push2 (Fake2 (info,minus)) toks_out
      (* fx the fake "," at the end of a structure or enum.
      no idea what other fake info there can be... *)
      else push2 (Comma str) toks_out

    | T1 tok ->
      let tok' = tok in

      let optindex =
        if TH.is_origin tok && not (TH.is_real_comment tok)
        then
          begin
            incr index;
            Some !index
          end
        else None
      in

      push2 (T2 (tok', minus, optindex, None)) toks_out
  in

  let expand_info t =
        add_elem t Ctx
  in

  toks +> List.iter expand_info;
  List.rev !toks_out

(*****************************************************************************)
(* Tokens2 processing, filtering, adjusting *)
(*****************************************************************************)

let drop_expanded xs =
  xs +> exclude (function
    | T2 (t,_,_,_) when TH.is_expanded t -> true
    | _ -> false
  )

let drop_fake xs =
  xs +> exclude (function
    | Fake2 _ -> true
    | _ -> false
  )

let nonl s =
  try let _ = Str.search_forward (Str.regexp "\n") s in false
  with Not_found -> true
let hasnl s =
  try let _ = Str.search_forward (Str.regexp "\n") s in true
  with Not_found -> false

let drop_comments xs =
  let rec loop = function
      [] -> []
    | T2(Parser_c.TCommentSpace _,_,_,_) ::
      T2((Parser_c.TComment _|Parser_c.TCommentCpp _
         |Parser_c.TCommentMisc _) as t,_,_,_)::
      ((T2((Parser_c.TCommentSpace _|Parser_c.TCommentNewline _),_,_,_)) as a)
      :: xs when nonl (TH.str_of_tok t) -> loop (a::xs)
    | T2(Parser_c.TCommentSpace _,_,_,_) ::
      T2((Parser_c.TComment _|Parser_c.TCommentCpp _
         |Parser_c.TCommentMisc _) as t,a,b,c) :: xs
      when hasnl(TH.str_of_tok t) ->
	let str = TH.str_of_tok t in
	let str = Str.split_delim (Str.regexp "\n") str in
	C2(String.concat "" (List.tl(List.map (function x -> "\n") str)))::
	loop xs
    | T2((Parser_c.TComment _|Parser_c.TCommentCpp _
         |Parser_c.TCommentMisc _) as t,a,b,c) :: xs ->
	     let str = TH.str_of_tok t in
	     let str = Str.split_delim (Str.regexp "\n") str in
	     C2(String.concat "" (List.tl(List.map (function x -> "\n") str)))::
	     loop xs
    | t::xs -> t::loop xs in
  loop xs

let is_ident_like s = s ==~ regexp_alpha

(*****************************************************************************)
(* Final unparsing (and debugging support) *)
(*****************************************************************************)

(* for debugging *)
type kind_token2 = KFake | KC | KExpanded | KOrigin

let kind_of_token2 = function
  | Fake2 _ -> KFake
  | C2 _ -> KC
  | Comma _ -> KC
  | T2 (t,_,_,_) ->
    (match TH.pinfo_of_tok t with
    | Ast_c.ExpandedTok _ -> KExpanded
    | Ast_c.OriginTok _ -> KOrigin
    | Ast_c.FakeTok _ -> raise (Impossible 139) (* now a Fake2 *)
    | Ast_c.AbstractLineTok _ -> raise (Impossible 140) (* now a KC *)
    )

let end_mark = "!"

let start_mark = function
  | KFake -> "!F!"
  | KC -> "!A!"
  | KExpanded -> "!E!"
  | KOrigin -> ""

let print_all_tokens2 pr xs =
  if !Flag_parsing_c.debug_unparsing
  then
    let current_kind = ref KOrigin in
    xs +> List.iter (fun t ->
      let newkind = kind_of_token2 t in
      if newkind =*= !current_kind
      then pr (str_of_token2 t)
      else
        begin
          pr (end_mark);
          pr (start_mark newkind);
          pr (str_of_token2 t);
          current_kind := newkind
        end
    );
  else
    let to_whitespace s =
      let r = String.copy s in (* TODO check depreciation warning *)
      for i = 1 to String.length r do
        let c = String.get r (i-1) in
        match c with
        | ' ' | '\t' | '\r' | '\n' -> ()
        | _ -> Bytes.set (Bytes.of_string r) (i-1) ' ' (* TODO is this efficient? *)
      done;
      r in
    let hiding_level = ref 0 in
    let handle_token t =
      let s = str_of_token2 t in
      let hide_current =
        match t with
        | T2 (t,_,_,_) ->
          let i = TH.info_of_tok t in
          (match Ast_c.get_annot_info i Token_annot.Exclude_start with
          | None   -> ()
          | Some _ -> hiding_level := !hiding_level + 1
          );
          let hide_current = !hiding_level > 0 in
          (match Ast_c.get_annot_info i Token_annot.Exclude_end with
          | None   -> ()
          | Some _ -> hiding_level := max (!hiding_level - 1) 0
          );
          hide_current
        | _ -> !hiding_level > 0 in
      if hide_current then to_whitespace s else s in
    xs +> List.iter (fun x -> pr (handle_token x))





(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* old: PPviatok was made in the beginning to allow to pretty print a
 * complete C file, including a modified C file by transformation.ml,
 * even if we don't handle yet in pretty_print_c.ml, ast_to_flow (and
 * maybe flow_to_ast) all the cases. Indeed we don't need to do some
 * fancy stuff when a function was not modified at all. Just need to
 * print the list of token as-is. But now pretty_print_c.ml handles
 * almost everything so maybe less useful. Maybe PPviatok allows to
 * optimize a little the pretty printing.
 *
 * update: now have PPviastr which goes even faster than PPviatok, so
 * PPviatok has disappeared.
 *)

type ppmethod = PPnormal | PPviastr




(* The pp_program function will call pretty_print_c.ml with a special
 * function to print the leaf components, the tokens. When we want to
 * print a token, we need to print also maybe the space and comments that
 * were close to it in the original file (and that was omitted during the
 * parsing phase), and honor what the cocci-info attached to the token says.
 * Maybe we will not print the token if it's a MINUS-token, and maybe we will
 * print it and also print some cocci-code attached in a PLUS to it.
 * So we will also maybe call unparse_cocci. Because the cocci-code may
 * contain metavariables, unparse_cocci will in fact sometimes call back
 * pretty_print_c (which will this time don't call back again unparse_cocci)
 *)

let pp_program2 xs outfile  =
  with_open_outfile outfile (fun (pr,chan) ->
    let pr s =
      if !Flag_parsing_c.debug_unparsing
      then begin pr2_no_nl s; flush stderr end
      else pr s
(* flush chan; *)
(* Common.pr2 ("UNPARSING: >" ^ s ^ "<"); *)
    in

    xs +> List.iter (fun ((e,(str, toks_e)), ppmethod) ->
      (* here can still work on ast *)
      let e = remove_useless_fakeInfo_struct e in

      match ppmethod with
      | PPnormal ->
        (* now work on tokens *)
        (* phase1: just get all the tokens, all the information *)
        assert(toks_e +> List.for_all (fun t ->
          TH.is_origin t || TH.is_expanded t
        ));
        let toks = get_fakeInfo_and_tokens e toks_e in
        let toks = displace_fake_nodes toks in
        (* assert Origin;ExpandedTok;Faketok *)
        let toks = expand_mcode toks in

        (* assert Origin;ExpandedTok; + Cocci + C (was AbstractLineTok)
         * and no tag endparen, just NOTHING. *)

        let toks = drop_comments (drop_expanded(drop_fake toks)) in

        (* in theory here could reparse and rework the ast! or
         * apply some SP. Not before cos julia may have generated
         * not parsable file. Need do unparsing_tricks call before
	 * being ready to reparse. *)
        print_all_tokens2 pr toks;

      | PPviastr -> pr str
    )
  )

let pp_program a b =
  profile_code "C unparsing" (fun () -> pp_program2 a b)


let pp_program_default xs outfile =
  let xs' = xs +> List.map (fun x -> x, PPnormal) in
  pp_program xs' outfile
