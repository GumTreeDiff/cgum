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


# 0 "./common_extra.ml"
(* I put those functions here and not in common.ml to try to avoid
 * as much as possible dependencies in common.ml so I can more easily
 * make ocaml script that just do a load common.ml without the need
 * to load many other files (like dumper.ml, or ANSITerminal.ml and
 * other recursive dependencies).
 *
 * Note that you can still use the functions below from an open Common.
 * You don't need to do a 'open Common_extra'; loading the commons.cma is
 * enough to make the connexions.
 *)


(* how to use it ? ex in LFS:
 *  Common.execute_and_show_progress (w.prop_iprop#length) (fun k ->
 *  w.prop_iprop#iter (fun (p, ip) ->
 *     k ();
 *     ...
 *  ));
 *
 *)

let execute_and_show_progress len f =
  let _count = ref 0 in
  (* kind of continuation passed to f *)
  let continue_pourcentage () =
    incr _count;
    ANSITerminal.set_cursor 1 (-1);
    ANSITerminal.printf [] "%d / %d" !_count len; flush stdout;
  in
  let nothing () = () in

  ANSITerminal.printf [] "0 / %d" len; flush stdout;
  if !Common._batch_mode
  then f nothing
  else f continue_pourcentage
  ;
  Common.pr2 ""


let set_link () =
  Common._execute_and_show_progress_func := execute_and_show_progress


let _init_execute =
  set_link ()
