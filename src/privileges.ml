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

(* Logic to handle user privileges.  Instead of cluttering HTML
   generation and other logic with privilege handling, abstract it behind
   a tight interface.  This interface also allows for a later addition of
   a more fine-grained access control. *)

open Types

(** with_can_create_user [user f on_fail] calls [f ()] if user is
    privileged enough to perform the operation.  Otherwise call
    [on_fail error] to handle the error case. *)
let with_can_create_user cur_user f ~on_fail =
  if cur_user.user_login = "admin" then
    f ()
  else
    on_fail ("User '"^cur_user.user_login^"' is not permitted to create new users")

let can_view_users cur_user =
  cur_user.user_login = "admin"

(** with_can_view_users [user f] calls [f ()] if user is privileged
    enough to view a list of all users.  Otherwise return an error
    message. *)
let with_can_view_users cur_user f ~on_fail =
  if can_view_users cur_user  then
    f ()
  else
    on_fail ("User '"^cur_user.user_login^"' is not permitted to view other users")

(** with_can_edit_user [user cur_user user_to_edit f] calls [f ()] if
    user is privileged enough to perform the operation.  Otherwise
    return an error message. *)
let with_can_edit_user cur_user target f ~on_fail =
  if cur_user.user_login = "admin" || cur_user.user_login = target.user_login then
    f ()
  else
    on_fail ("User '"^cur_user.user_login^"' is not permitted to edit users other than self")

(** Privileged enough to schedule tasks for all users? *)
let can_schedule_all_tasks cur_user =
  cur_user.user_login = "admin"

let user_owns_task_or_is_admin todo cur_user =
  if cur_user.user_login = "admin" then
    true
  else
    match todo.t_owner with
      Some o -> o.owner_id = cur_user.user_id
    | None -> false

let can_edit_task todo cur_user =
  user_owns_task_or_is_admin todo cur_user

let can_complete_task task_id cur_user =
  let%lwt todo = Database.query_todo task_id in
  match todo with
    Some t ->
      Lwt.return (user_owns_task_or_is_admin t cur_user)
  | None -> Lwt.return false

let can_modify_task_priority task_id cur_user =
  let%lwt todo = Database.query_todo task_id in
  match todo with
    Some t ->
      Lwt.return (user_owns_task_or_is_admin t cur_user)
  | None -> Lwt.return false
