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

open Lwt
open Eliom_content.Html.F
open Eliom_content.Html.F.Form
open Eliom_service
open Eliom_parameter

open Services
open Types

open Config

module Db = Database
module Dbu = Database_upgrade

let seconds_in_day = 60.0 *. 60.0 *. 24.0

let scope_hierarchy = Eliom_common.create_scope_hierarchy "nurpawiki_session_data"
let scope = `Session scope_hierarchy

let login_eref = Eliom_reference.eref
  ~scope
  ~persistent:"login_info" None

(* Set password & login into session.  We set the cookie expiration
   into 24h from now so that the user can even close his browser
   window, re-open it and still retain his logged in status. *)
let set_password_in_session login_info =
  let open Eliom_state in
  let cookie_scope = scope in
  set_service_state_timeout ~cookie_scope None;
  set_persistent_data_state_timeout ~cookie_scope None >>= fun () ->
  set_persistent_data_cookie_exp_date ~cookie_scope (Some 3153600000.0) >>= fun () ->
  Eliom_reference.set login_eref (Some login_info)

let upgrade_page = create ~path:(Path ["upgrade"]) ~meth:(Get unit) ()

let schema_install_page = create ~path:(Path ["schema_install"]) ~meth:(Get unit) ()

let connect_action =
  create ~path:No_path
    ~meth:(Post (unit, (string "login") ** (string "passwd")))
    ()


let link_to_nurpawiki_main sp =
  a ~service:wiki_view_page
    [txt "Take me to Nurpawiki"]
    (Config.site.cfg_homepage,(None,(None,None)))

(* Get logged in user as an option *)
let get_login_user () =
  Eliom_reference.get login_eref

let db_upgrade_warning () =
  [h1 [txt "Database Upgrade Warning!"];
   p
     [txt "An error occured when Nurpawiki was trying to access database.";
      br ();
      strong [
        txt "You might be seeing this for a couple of reasons:";
        br ()];
      br ();
      txt "1) You just installed Nurpawiki and this is the first time you're running Nurpawiki on your database!"; br ();
      txt "2) You have upgraded an existing Nurpawiki installation and this is the first time you're running it since upgrade."; br ();
      br ();
      txt "In order to continue, your DB needs to be upgraded. ";
      txt "If you have valuable data in your DB, please take a backup of it before proceeding!";
      br ();
      br ();
      a ~service:upgrade_page [txt "Upgrade now!"] ()]]

let db_installation_error () =
  [div
     [h1 [txt "Database schema not installed"];
      br ();
      p [txt "It appears you're using your Nurpawiki installation for the first time. "; br (); br ();
         txt "In order to complete Nurpawiki installation, your Nurpawiki database schema needs to be initialized."];
      p [txt "Follow this link to complete installation:"; br (); br ();
         a ~service:schema_install_page [txt "Install schema!"] ()]]]


let login_html ~err =
  let help_text =
    [br (); br ();
     strong [txt "Please read "];
     Raw.a ~a:[a_id "login_help_url"; a_href (uri_of_string (fun () -> "https://github.com/glondu/nurpawiki/blob/master/doc/Tutorial.md"))] [txt "Nurpawiki tutorial"];
     txt " if you're logging in for the first time.";
     br ()] in

  Html_util.html_stub ~title:"Login"
    [div ~a:[a_id "login_outer"]
       [div ~a:[a_id "login_align_middle"]
          [post_form connect_action
             (fun (loginname,passwd) ->
                [table ~a:[a_class ["login_box"]]
                   [tr [td ~a:[a_class ["login_text"]]
                          (txt "Welcome to Nurpawiki!"::help_text)];
                    tr [td [txt ""]];
                    tr [td ~a:[a_class ["login_text_descr"]]
                          [txt "Username:"]];
                    tr [td [input ~input_type:`Text ~name:loginname Form.string]];
                    tr [td ~a:[a_class ["login_text_descr"]]
                          [txt "Password:"]];
                    tr [td [input ~input_type:`Password ~name:passwd Form.string]];
                    tr [td [input ~input_type:`Submit ~value:"Login" Form.string]]];
                 p err]) ()]]]


let with_db_installed f =
  (* Check if the DB is installed.  If so, check that it doesn't need
     an upgrade. *)
  let%lwt b = Dbu.is_schema_installed () in
  if not b then
    return (Html_util.html_stub ~title:"Database installation error" (db_installation_error ()))
  else
    let%lwt v = Dbu.db_schema_version () in
    if v < Db.nurpawiki_schema_version then
      return (Html_util.html_stub ~title:"Database upgrade warning" (db_upgrade_warning ()))
    else f ()

(** Wrap page service calls inside with_user_login to have them
    automatically check for user login and redirect to login screen if
    not logged in. *)
let with_user_login ?(allow_read_only=false) f =
  let login () =
    get_login_user ()
    >>= function
      | Some (login,passwd) ->
          begin
            Db.query_user login
            >>= function
              | Some user -> begin
                    (* Authenticate user against his password *)
                    match Password.check user.user_passwd passwd with
                    | Result.Ok (update, auth) ->
                      if not auth then
                        return
                          (login_html
                             [Html_util.error ("Wrong password given for user '"^login^"'")])
                      else begin
                        (if update then
                          Db.with_conn (fun conn ->
                            Db.update_user ~conn ~user_id:user.user_id
                                           ~passwd:(Some (Password.salt passwd))
                                           ~real_name:user.user_real_name
                                           ~email:user.user_email)
                         else return_unit)
                        >>= fun () ->
                        f user
                      end
                    | Result.Error e -> failwith e (*TODO: change this *)
                end
              | None ->
                  return
                    (login_html
                       [Html_util.error ("Unknown user '"^login^"'")])
          end
      | None ->
          if allow_read_only && Config.site.cfg_allow_ro_guests then
            let guest_user =
              {
                user_id = 0;
                user_login = "guest";
                user_passwd = "";
                user_real_name = "Guest";
                user_email = "";
              } in
            f guest_user
          else
            return (login_html [])
  in
  with_db_installed login

(* Either pretend to be logged in as 'guest' (if allowed by config
   options) or require a proper login.

   If logging in as 'guest', we setup a dummy user 'guest' that is not
   a real user.  It won't have access to write to any tables. *)
let with_guest_login f =
 with_user_login ~allow_read_only:true f

(* Same as with_user_login except that we can't generate HTML for any
   errors here.  Neither can we present the user with a login box.  If
   there are any errors, just bail out without doing anything
   harmful. *)
let action_with_user_login f =
  let%lwt db_version = Dbu.db_schema_version () in
  if db_version = Db.nurpawiki_schema_version then
    get_login_user ()
    >>= function
      | Some (login,passwd) ->
          begin
            Db.query_user login
            >>= function
              | Some user -> begin
                (* Authenticate user against his password *)
                match Password.check user.user_passwd passwd with
                | Result.Ok (_, auth) ->
                    if auth then
                      f user
                    else
                      return ()
                | Result.Error _ -> return ()
                end
              | None ->
                  return ()
          end
      | None -> return ()
 else
   return ()


let update_session_password login new_password =
  Eliom_state.discard ~scope () >>= fun () ->
  set_password_in_session (login, new_password)

(* Check session to see what happened during page servicing.  If any
   actions were called, some of them might've set values into session
   that we want to use for rendering the current page. *)
let any_complete_undos () =
  let table = Eliom_request_info.get_request_cache () in
  try
    Some (Polytables.get ~table ~key:action_completed_task)
  with Not_found ->
    None

(* Same as any_complete_undos except we check for changed task
   priorities. *)
let any_task_priority_changes () =
  let table = Eliom_request_info.get_request_cache () in
  try
    Some (Polytables.get ~table ~key:action_task_priority_changed)
  with Not_found ->
    None

let connect_action_handler () login_nfo =
  Eliom_state.discard ~scope () >>= fun () ->
    set_password_in_session login_nfo >>= fun () ->
      return ()

let () =
  Eliom_registration.Action.register ~service:connect_action connect_action_handler

(* /schema_install initializes the database schema (if needed) *)
let _ =
  Eliom_registration.Html.register schema_install_page
    (fun () () ->
       Database_schema.install_schema ();%lwt
       let title = "Database installation completed" in
       return
         (Html_util.html_stub ~title
            [h1 [txt title];
             p [br ();
                link_to_nurpawiki_main ()]]))

(* /upgrade upgrades the database schema (if needed) *)
let _ =
  Eliom_registration.Html.register upgrade_page
    (fun () () ->
       let%lwt msg = Dbu.upgrade_schema () in
       let title = "Upgrade database schema" in
       return
         (Html_util.html_stub ~title
            [h1 [txt title];
             (pre [txt msg]);
             p [br ();
                link_to_nurpawiki_main ()]]))

let _ =
  Eliom_registration.Html.register disconnect_page
    (fun () () ->
       Eliom_state.discard ~scope () >>= fun () ->
        return
          (Html_util.html_stub ~title:"Logout"
             [h1 [txt "Logged out!"];
              p [br ();
                 link_to_nurpawiki_main ()]]))
