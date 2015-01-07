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


# 0 "./main.ml"
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

open Common

let test_type_c uncommentify infile =
  Printf.fprintf stderr "handling %s\n" infile;
  flush stderr;
  let (program2, _stat) =
    Common.profile_code "parsing"
      (fun () -> Parse_c.parse_c_and_cpp false infile) in
  let _program2 =
      Common.profile_code "type inference" (fun () -> 
    program2 
    +> Common.unzip 
    +> (fun (program, infos) -> 
      Type_annoter_c.annotate_program !Type_annoter_c.initial_env 
        program +> List.map fst,
      infos
    )
    +> Common.uncurry Common.zip)
  in
  let (program, infos) = Common.unzip program2 in
  match uncommentify with
    Some outfile ->
      Uncommentify.pp_program_default program2 outfile
  | None ->
      try Xmlify.pp_program program
      with e ->
	Printf.eprintf "failure on %s\n" infile;
	Printf.eprintf "%s\n" (Printexc.to_string e);
	Printf.eprintf "%s" (Printexc.get_backtrace ())

let cat new_code = ()
(*  pretty_print_to_file new_code *)
 (*   (function fl -> Common.command2 (Printf.sprintf "cat %s" fl)) *)

let diff = ref false
let real_diff = ref false
let verbose = ref false
let macros = ref ""
let uncommentify = ref (None : string option)

let options = [
(* if you want command line arguments, put them here:
   "option name", operation described in man Arg, "description"; *)
  "--profile", Arg.Unit (function () -> Common.profile := Common.PALL) ,
  "   gather timing information about the main coccinelle functions";
  "-macro_file_builtins", Arg.Set_string macros,
  "  macro definition file";
  "-uncommentify", Arg.String (fun x -> uncommentify := Some x),
  "  remove comments in a C file";
  "--type-info", Arg.Set Flag.type_info, "  include type information";
]

let file = ref ""

let anonymous str = file := str

let _ =
  Arg.parse options anonymous "";
 let home = Sys.getenv "HOME" in
 (if !macros = "" then macros := (home^"/cgum/standard.h"));
 Parse_c.init_defs_builtins !macros;
 Common.print_to_stderr := !verbose;
 Flag_parsing_c.verbose_lexing := !verbose;
 Flag_parsing_c.verbose_parsing := !verbose;
 Flag_parsing_c.verbose_type := !verbose;


  let files =
    if Sys.is_directory !file
    then Common.cmd_to_list ("find "^ !file ^" -name \"*.[ch]\"")
    else [!file] in
  Common.main_boilerplate (fun () ->  
    List.iter (test_type_c !uncommentify) files)
