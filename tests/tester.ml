let process_output_to_list2 = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)
let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l
let process_output_to_list = cmd_to_list
let cmd_to_list_and_status = process_output_to_list2

(* --------------------------------------------------------------------- *)

let process_one curdir dir commit file out precommit base basec str =
  Printf.printf "running: %s\n"
    (Printf.sprintf "cd %s; git show %s:%s > %s/%s/%s/%s_%s"
       dir commit file curdir out precommit str base);
  let _ =
    Sys.command
      (Printf.sprintf "cd %s; git show %s:%s > %s/%s/%s/%s_%s"
	 dir commit file curdir out precommit str base) in
  let _ =
    Sys.command
      (Printf.sprintf "../cgum %s/%s/%s_%s > %s/%s/%s_%s.xml"
	 out precommit str base out precommit str basec) in
  let _ =
    Sys.command
      (Printf.sprintf "../cgum %s/%s/%s_%s -uncommentify %s/%s/%s_%s_nocomm.c"
	 out precommit str base out precommit str basec) in
  ()

let process_both out commit base basec =
  let _ =
    let before = Printf.sprintf "%s/%s/before_%s_nocomm.c" out commit basec in
    let after = Printf.sprintf "%s/%s/after_%s_nocomm.c" out commit basec in
    Sys.command
      (Printf.sprintf
	 "diff -u -b -w -B %s %s | diffstat > %s/%s/diff_%s" before after
	 out commit basec) in
  let _ =
    let res = Printf.sprintf "%s/%s/gdiff_%s" out commit basec in
    Sys.command
      (Printf.sprintf
	 "../tools/diffstat/diffstat after_%s_nocomm.c < %s_res/%s.xml > %s"
	 basec basec commit res) in
  ()

(* --------------------------------------------------------------------- *)

let dir = ref ""
let file = ref ""
let out = ref ""

let options = []

let anonymous s =
  if !dir = "" then dir := s else if !file = "" then file := s else out := s

let _ =
  Arg.parse options anonymous "usage: dir file";
  let base = Filename.basename !file in
  let basec = Filename.chop_extension base in
  let curdir = Sys.getcwd() in
  let _ = Sys.command (Printf.sprintf "/bin/rm -rf %s; mkdir %s" !out !out) in
  let _ = Sys.command (Printf.sprintf "echo %s > %s/README" !file !out) in
  let commits =
    cmd_to_list (Printf.sprintf "cd %s; git log --oneline %s" !dir !file) in
  List.iter
    (function commit ->
      let commit = List.hd (Str.split (Str.regexp " ") commit) in
      let _ = Sys.command (Printf.sprintf "mkdir %s/%s" !out commit) in
      process_one curdir
	!dir (commit^"^") !file !out commit base basec "before";
      process_one curdir
	!dir commit !file !out commit base basec "after";
      process_both !out commit base basec)
    commits

