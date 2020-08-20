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

module OrdInt = struct type t = int let compare a b = compare a b end
module IMap = Map.Make (OrdInt)

(* Sides-effects of Eliom actions *)
let action_completed_task : int Polytables.key = Polytables.make_key ()
let action_task_priority_changed : int Polytables.key = Polytables.make_key ()

type user =
    {
      user_id : int;
      user_login : string;
      user_passwd : string;
      user_real_name : string;
      user_email : string;
    }

type todo =
    {
      t_id : int;
      t_descr : string;
      t_completed : bool;
      t_priority : int;
      t_activation_date : string;
      t_owner : owner option;
    }
and owner =
    {
      owner_id : int;
      owner_login : string;
    }

type page =
    {
      p_id : int;
      p_descr : string;
    }

type page_revision =
    {
      pr_revision : int;
      pr_created : string;
      pr_owner_id : int option;
      pr_owner_login : string option;
    }

type activity_type =
    AT_create_todo
  | AT_complete_todo
  | AT_work_on_todo
  | AT_create_page
  | AT_edit_page
  | AT_uncomplete_todo

type activity =
    {
      a_id : int;
      a_activity : activity_type;
      a_date : string;
      a_todo_descr : string option;
      a_changed_by : string option;
    }

type search_result_type = SR_page | SR_todo

type search_result =
    {
      sr_id : int;
      sr_headline : string;
      sr_result_type : search_result_type;
      sr_page_descr : string option;
    }

type et_cont =
    ET_scheduler
  | ET_view of string

let et_cont_view_re = Pcre.regexp "^v_(.*)$"

let string_of_et_cont = function
    ET_scheduler -> "s"
  | ET_view src_page -> "v_"^src_page

let match_pcre_option rex s =
  try Some (Pcre.extract ~rex s) with Not_found -> None

let et_cont_of_string s =
  if s = "s" then
    ET_scheduler
  else
    begin
      match match_pcre_option et_cont_view_re s with
        None ->
          raise (Failure "et_cont_of_string")
      | Some s ->
          ET_view s.(1)
    end

let int_of_activity_type = function
    AT_create_todo -> 1
  | AT_complete_todo -> 2
  | AT_work_on_todo -> 3
  | AT_create_page -> 4
  | AT_edit_page -> 5
  | AT_uncomplete_todo -> 6

let activity_type_of_int = function
    1 -> AT_create_todo
  | 2 -> AT_complete_todo
  | 3 -> AT_work_on_todo
  | 4 -> AT_create_page
  | 5 -> AT_edit_page
  | 6 -> AT_uncomplete_todo
  | _ -> assert false
