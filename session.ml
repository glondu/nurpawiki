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
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Services
open Types

open Config

module Db = Database
module Dbu = Database_upgrade

let seconds_in_day = 60.0 *. 60.0 *. 24.0

let login_table = Eliom_sessions.create_persistent_table "login_table"

(* Set password & login into session.  We set the cookie expiration
   into 24h from now so that the user can even close his browser
   window, re-open it and still retain his logged in status. *)
let set_password_in_session sp login_info =
  set_service_session_timeout ~sp None;

  set_persistent_data_session_timeout ~sp None >>= fun () ->
    set_persistent_data_session_cookie_exp_date ~sp (Some 3153600000.0) >>= fun () ->
      set_persistent_session_data ~table:login_table ~sp login_info

let upgrade_page = new_service ["upgrade"] unit ()

let schema_install_page = new_service ["schema_install"] unit ()

let connect_action = 
  Eliom_services.new_post_coservice'
    ~post_params:((string "login") ** (string "passwd"))
    ()
    

let link_to_nurpawiki_main sp = 
  a ~sp ~service:wiki_view_page 
    [pcdata "Take me to Nurpawiki"] 
    (Config.site.cfg_homepage,(None,(None,None)))

(* Get logged in user as an option *)
let get_login_user sp =
  Eliom_sessions.get_persistent_session_data login_table sp () >>=
    fun session_data ->
      match session_data with
        Eliom_sessions.Data user -> Lwt.return (Some user)
      | Eliom_sessions.No_data 
      | Eliom_sessions.Data_session_expired -> Lwt.return None

let db_upgrade_warning sp = 
  [h1 [pcdata "Database Upgrade Warning!"];
   p
     [pcdata "An error occured when Nurpawiki was trying to access database.";
      br ();
      strong [
        pcdata "You might be seeing this for a couple of reasons:";
        br ()];
      br ();
      pcdata "1) You just installed Nurpawiki and this is the first time you're running Nurpawiki on your database!"; br ();
      pcdata "2) You have upgraded an existing Nurpawiki installation and this is the first time you're running it since upgrade."; br ();
      br ();
      pcdata "In order to continue, your DB needs to be upgraded. ";
      pcdata "If you have valuable data in your DB, please take a backup of it before proceeding!";
      br ();
      br ();
      a ~service:upgrade_page ~sp [pcdata "Upgrade now!"] ()]]

let db_installation_error sp = 
  [div
     [h1 [pcdata "Database schema not installed"];
      br ();
      p [pcdata "It appears you're using your Nurpawiki installation for the first time. "; br (); br ();
         pcdata "In order to complete Nurpawiki installation, your Nurpawiki database schema needs to be initialized."];
      p [pcdata "Follow this link to complete installation:"; br (); br ();
         a ~service:schema_install_page ~sp [pcdata "Install schema!"] ()]]]
     

let login_html sp ~err =
  let help_text = 
    [br (); br (); 
     strong [pcdata "Please read "];
     XHTML.M.a ~a:[a_id "login_help_url"; a_href (uri_of_string "http://code.google.com/p/nurpawiki/wiki/Tutorial")] [pcdata "Nurpawiki tutorial"]; 
     pcdata " if you're logging in for the first time.";
     br ()] in

  Html_util.html_stub sp 
    [div ~a:[a_id "login_outer"]
       [div ~a:[a_id "login_align_middle"]
          [Eliom_predefmod.Xhtml.post_form connect_action sp
             (fun (loginname,passwd) ->
                [table ~a:[a_class ["login_box"]]
                   (tr (td ~a:[a_class ["login_text"]]
                          (pcdata "Welcome to Nurpawiki!"::help_text)) [])
                   [tr (td [pcdata ""]) [];
                    tr (td ~a:[a_class ["login_text_descr"]] 
                          [pcdata "Username:"]) [];
                    tr (td [string_input ~input_type:`Text ~name:loginname ()]) [];
                    tr (td ~a:[a_class ["login_text_descr"]] 
                          [pcdata "Password:"]) [];
                    tr (td [string_input ~input_type:`Password ~name:passwd ()]) [];
                    tr (td [string_input ~input_type:`Submit ~value:"Login" ()]) []];
                 p err]) ()]]]


let with_db_installed sp f =
  (* Check if the DB is installed.  If so, check that it doesn't need
     an upgrade. *)
  Db.with_conn
    (fun conn ->
       if not (Dbu.is_schema_installed ~conn) then
         Some (Html_util.html_stub sp (db_installation_error sp))
       else if Dbu.db_schema_version ~conn < Db.nurpawiki_schema_version then
         Some (Html_util.html_stub sp (db_upgrade_warning sp))
       else None)
  >>= function
    | Some x -> return x
    | None -> f ()

(** Wrap page service calls inside with_user_login to have them
    automatically check for user login and redirect to login screen if
    not logged in. *)
let with_user_login ?(allow_read_only=false) sp f =
  let login () =
    get_login_user sp
    >>= function
      | Some (login,passwd) ->
          begin
            Db.with_conn (fun conn -> Db.query_user ~conn login)
            >>= function
              | Some user ->
                  let passwd_md5 = Digest.to_hex (Digest.string passwd) in
                  (* Autheticate user against his password *)
                  if passwd_md5 <> user.user_passwd then
                    return
                      (login_html sp
                         [Html_util.error ("Wrong password given for user '"^login^"'")])
                  else
                    f user sp
              | None ->
                  return
                    (login_html sp
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
            f guest_user sp
          else 
            return (login_html sp [])
  in
  with_db_installed sp login

(* Either pretend to be logged in as 'guest' (if allowed by config
   options) or require a proper login.
   
   If logging in as 'guest', we setup a dummy user 'guest' that is not
   a real user.  It won't have access to write to any tables. *)
let with_guest_login sp f =
 with_user_login ~allow_read_only:true sp f

(* Same as with_user_login except that we can't generate HTML for any
   errors here.  Neither can we present the user with a login box.  If
   there are any errors, just bail out without doing anything
   harmful. *)
let action_with_user_login sp f =
  Db.with_conn (fun conn -> Dbu.db_schema_version conn) >>= fun db_version ->
  if db_version = Db.nurpawiki_schema_version then
    get_login_user sp
    >>= function
      | Some (login,passwd) ->
          begin
            Db.with_conn (fun conn -> Db.query_user ~conn login)
            >>= function
              | Some user ->
                  let passwd_md5 = Digest.to_hex (Digest.string passwd) in
                  (* Autheticate user against his password *)
                  if passwd_md5 = user.user_passwd then
                    f user
                  else
                    return ()
              | None ->
                  return ()
          end
      | None -> return ()
 else
   return ()


let update_session_password sp login new_password =
  ignore
    (Eliom_sessions.close_session  ~sp () >>= fun () -> 
       set_password_in_session sp (login,new_password))
  

(* Check session to see what happened during page servicing.  If any
   actions were called, some of them might've set values into session
   that we want to use for rendering the current page. *)
let any_complete_undos sp =
  let table = Eliom_sessions.get_request_cache sp in
  try
    Some (Polytables.get ~table ~key:action_completed_task)
  with Not_found ->
    None

(* Same as any_complete_undos except we check for changed task
   priorities. *)
let any_task_priority_changes sp =
  let table = Eliom_sessions.get_request_cache sp in
  try
    Some (Polytables.get ~table ~key:action_task_priority_changed)
  with Not_found ->
    None

let connect_action_handler sp () login_nfo =
  Eliom_sessions.close_session  ~sp () >>= fun () -> 
    set_password_in_session sp login_nfo >>= fun () ->
      return ()

let () =
  Eliom_predefmod.Action.register ~service:connect_action connect_action_handler

(* /schema_install initializes the database schema (if needed) *)
let _ =
  register schema_install_page
    (fun sp () () ->
       Db.with_conn (fun conn -> Database_schema.install_schema ~conn) >>= fun _ ->
       return
         (Html_util.html_stub sp
            [h1 [pcdata "Database installation completed"];
             p [br ();
                link_to_nurpawiki_main sp]]))

(* /upgrade upgrades the database schema (if needed) *)
let _ =
  register upgrade_page
    (fun sp () () ->
       Db.with_conn (fun conn -> Dbu.upgrade_schema ~conn) >>= fun msg ->
       return
         (Html_util.html_stub sp
            [h1 [pcdata "Upgrade DB schema"];
             (pre [pcdata msg]);
             p [br ();
                link_to_nurpawiki_main sp]]))

let _ =
  register disconnect_page
    (fun sp () () ->
       (Eliom_sessions.close_session  ~sp () >>= fun () ->
        return
          (Html_util.html_stub sp 
             [h1 [pcdata "Logged out!"];
              p [br ();
                 link_to_nurpawiki_main sp]])))
