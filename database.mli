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

type connection

val with_conn : (connection -> 'a) -> 'a Lwt.t
val guarded_exec : conn:connection -> string -> Postgresql.result
val insert_save_page_activity :
  user_id:int -> int -> unit Lwt.t
val query_todos_by_ids : int list -> Types.todo list Lwt.t
val query_todo : int -> Types.todo option Lwt.t
val todo_exists : int -> bool Lwt.t
val update_todo_activation_date :
  int -> string -> unit Lwt.t
val update_todo_descr : int -> string -> unit Lwt.t
val update_todo_owner_id : int -> int option -> unit Lwt.t
val query_all_active_todos :
  current_user_id:int option -> unit -> Types.todo list Lwt.t
val query_upcoming_todos :
  current_user_id:int option -> int option * int option -> Types.todo list Lwt.t
val new_todo : conn:connection -> int -> int -> string -> string
val todos_in_pages :
  int list -> Types.page list Types.IMap.t Lwt.t
val query_activity_in_pages :
  min_id:int -> max_id:int -> Types.page list Types.IMap.t Lwt.t
val query_highest_activity_id : unit -> int Lwt.t
val query_page_todos : int -> Types.todo Types.IMap.t Lwt.t
val update_page_todos : int -> int list -> unit Lwt.t
val complete_task :
  user_id:int -> Types.IMap.key -> unit Lwt.t
val uncomplete_task :
  user_id:int -> Types.IMap.key -> unit Lwt.t
val up_task_priority : int -> unit Lwt.t
val down_task_priority : int -> unit Lwt.t
val new_wiki_page : user_id:int -> string -> int Lwt.t
val save_wiki_page :
  int -> user_id:int -> string list -> unit Lwt.t
val find_page_id : string -> int option Lwt.t
val page_id_of_page_name : string -> int Lwt.t
val wiki_page_exists : string -> bool Lwt.t
val load_wiki_page :
  ?revision_id:int option -> int -> string Lwt.t
val query_page_revisions :
  string -> Types.page_revision list Lwt.t
val query_past_activity :
  min_id:int -> max_id:int -> Types.activity list Lwt.t
val search_wikipage :
  string -> Types.search_result list Lwt.t
val query_users : unit -> Types.user list Lwt.t
val query_user : string -> Types.user option Lwt.t
val add_user :
  conn:connection ->
  login:string -> passwd:string -> real_name:string -> email:string -> unit
val update_user :
  conn:connection ->
  user_id:int ->
  passwd:string option -> real_name:string -> email:string -> unit
val nurpawiki_schema_version : int
