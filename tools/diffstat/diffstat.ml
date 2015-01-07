exception MalformedXML

let fail from msg exn =
  Printf.fprintf stderr "%s: %s\n" from msg;
  raise exn

(* ----------------------------------------------------------------------- *)
(* Generic accessor functions *)

let get_type attrs =
  snd (List.find (fun (key, value) -> key = "type") attrs)

let get_file nm attrs =
  try snd (List.find (fun (key, value) -> key = "file") attrs)
  with Not_found -> nm

let get_beginline attrs =
  let str = snd (List.find (fun (key, value) -> key = "begin_line") attrs) in
  int_of_string str

let get_begincol attrs =
  let str = snd (List.find (fun (key, value) -> key = "begin_col") attrs) in
  int_of_string str

let get_endline attrs =
  let str = snd (List.find (fun (key, value) -> key = "end_line") attrs) in
  int_of_string str

let get_endcol attrs =
  let str = snd (List.find (fun (key, value) -> key = "end_col") attrs) in
  int_of_string str

type pos = string * int * int * int * int

let get_pos nm attributes =
  (get_file nm attributes,
   get_beginline attributes,
   get_begincol attributes,
   get_endline attributes,
   get_endcol attributes)

let get_before nm children =
  let elem = List.find (fun xml ->
    match xml with
	Xml.Element("before", _, _) -> true
      | _ -> false
  ) children in
  match elem with
      Xml.Element(_, attrs, positions) ->
	get_pos nm attrs
    | _ -> raise MalformedXML

let get_after nm children =
  let elem = List.find (fun xml ->
    match xml with
	Xml.Element("after", _, _) -> true
      | _ -> false
  ) children in
  match elem with
      Xml.Element(_, attrs, positions) ->
	get_pos nm attrs
    | _ -> raise MalformedXML

(* ----------------------------------------------------------------------- *)

type ast = Insert of pos | Move of pos * pos | Delete of pos

let parse_action nm xml =
  match xml with
    Xml.Element("action", attributes, children) ->
      (match get_type attributes with
	"Insert" -> Insert (get_after nm children)
      | "Move" | "Update" ->
	  Move (get_before nm children, get_after nm children)
      | "Delete" -> Delete (get_pos nm attributes)
      | _ -> fail "parse_action" (Xml.to_string xml) MalformedXML)
  | _ -> fail "parse_action" (Xml.to_string xml) MalformedXML

let parse_actions nm xml =
  match xml with
    Xml.Element("actions", attributes, children) ->
      List.map (parse_action nm) children
  | _ -> fail "parse_action" (Xml.to_string xml) MalformedXML

(* ----------------------------------------------------------------------- *)

let files = ref []
let insert_table = Hashtbl.create 101
let delete_table = Hashtbl.create 101

let update_lines tbl file bl el =
  (if not (List.mem file !files) then files := file :: !files);
  let rec loop bl =
    if bl > el
    then ()
    else
      begin
	let cell =
	  try Hashtbl.find tbl file
	  with Not_found ->
	    let cell = ref [] in
	    Hashtbl.add tbl file cell;
	    cell in
	(if not (List.mem bl !cell)
	then cell := bl :: !cell);
	loop (bl+1)
      end in
  loop bl

let process_action = function
    Insert((file,bl,_,el,_)) ->
      update_lines insert_table file bl el
  | Move((file1,bl1,_,el1,_),(file2,bl2,_,el2,_)) ->
      update_lines delete_table file1 bl1 el1;
      update_lines insert_table file2 bl2 el2
  | Delete((file,bl,_,el,_)) ->
      update_lines delete_table file bl el

(* ----------------------------------------------------------------------- *)

let result p =
  let files = List.sort compare !files in
  let i = ref 0 in
  let d = ref 0 in
  List.iter
    (function file ->
      let fl =
	match p with
	  None -> Filename.basename file
	| Some n ->
	    let l = Str.split (Str.regexp "/") file in
	    let rec loop fl = function
		0 -> String.concat "/" fl
	      |	n ->
		  (match fl with
		    [f] -> f
		  | _ -> loop (List.tl fl) (n-1)) in
	    loop l n in
      let inserted =
	try List.length (!(Hashtbl.find insert_table file))
	with Not_found -> 0 in
      let deleted =
	try List.length (!(Hashtbl.find delete_table file))
	with Not_found -> 0 in
      i := inserted + !i;
      d := deleted + !d;
      Printf.printf " %s | \t%d %s%s\n" fl (inserted + deleted)
	(String.make inserted '+') (String.make deleted '-'))
    files;
  let pr str n code =
    match n with
      0 -> ""
    | 1 -> Printf.sprintf ", 1 %s(%s)" str code
    | n -> Printf.sprintf ", %d %ss(%s)" n str code in
  Printf.printf " %d file%s changed%s%s\n"
    (List.length files) (match files with [_] -> "" | _ -> "s")
    (pr "insertion" !i "+") (pr "deletion" !d "-")

(* ----------------------------------------------------------------------- *)
(* entry point *)

let _ =
  let (p,nm) =
    match Array.to_list Sys.argv with
      [] | [_] -> (None,"a_file")
    | [_;p] ->
	(match Str.split_delim (Str.regexp "-p") p with
	  ["";n] -> (Some (int_of_string n),"a_file")
	| [nm] -> (None,nm)
	| _ -> failwith "bad -p argument")
    | [_;p;nm] ->
	(Some(int_of_string(List.hd(Str.split_delim (Str.regexp "-p") p))),nm)
    | _ -> failwith "wrong number of arguments, only -p{n} allowed" in
  let data = Xml.parse_in stdin in
  let data = parse_actions nm data in
  List.iter process_action data;
  result p
