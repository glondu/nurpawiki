(* Copyright (c) 2006-2008 Janne Hellsten <jjhellst@gmail.com> *)

(*
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.  You should have received
 * a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

open Eliom_content.Html.F
open Eliom_parameter
open Eliom_service

open Config
open Types

open Lwt

let wiki_view_page =
  create ~path:(Path ["view"]) ~meth:(Get ((string "p")
                        ** (opt (bool "printable"))
                        ** (opt (int "r"))
                        ** (opt (bool "force_login")))) ()

let wiki_start = Eliom_registration.Redirection.create ~path:(Path []) ~meth:(Get unit)
  (fun () () -> return (Eliom_registration.Redirection (preapply ~service:wiki_view_page (Config.site.cfg_homepage, (None, (None, None))))))

let wiki_edit_page = create ~path:(Path ["edit"]) ~meth:(Get (string "p")) ()

let scheduler_page = create ~path:(Path ["scheduler"]) ~meth:(Get unit) ()

let edit_todo_get_page = create ~path:(Path ["edit_todo"])
  ~meth:(Get ((user_type
      et_cont_of_string string_of_et_cont "src_service") **
     (opt (int "tid")))) ()

let edit_todo_page =
  create_attached_post
    ~fallback:edit_todo_get_page
    ~post_params:any ()

let history_page = create ~path:(Path ["history"]) ~meth:(Get (opt (int "nth_p"))) ()

let search_page = create ~path:(Path ["search"]) ~meth:(Get (string "q")) ()

let benchmark_page = create ~path:(Path ["benchmark"]) ~meth:(Get (string "test")) ()

let user_admin_page = create ~path:(Path ["user_admin"]) ~meth:(Get unit) ()

let edit_user_page = create ~path:(Path ["edit_user"])
  ~meth:(Get (opt (string "caller") ** (string "user_to_edit"))) ()

let disconnect_page = create ~path:(Path ["disconnect"]) ~meth:(Get unit) ()

let about_page = create ~path:(Path ["about"]) ~meth:(Get unit) ()

let page_revisions_page = create ~path:(Path ["page_revisions"]) ~meth:(Get (string "p")) ()

let task_side_effect_complete_action =
  create ~path:No_path ~meth:(Get (int "task_id")) ()

let task_side_effect_undo_complete_action =
  create ~path:No_path ~meth:(Get (int "task_id")) ()

let task_side_effect_mod_priority_action =
  create ~path:No_path ~meth:(Get ((int "task_id") ** bool "dir")) ()
