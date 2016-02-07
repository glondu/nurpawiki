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
open Eliom_content.Html5.F
open Eliom_parameter
open Eliom_service
open Eliom_service.Http

open Services
open Types

module Db = Database

let service_create_new_user = 
  post_service
    ~fallback:user_admin_page
    ~post_params:((string "login") ** 
                    (string "pass") ** 
                    (string "pass2") **  (* re-type *)
                    (string "name") **
                    (string "email"))
    ()

let service_save_user_edit =
  post_service
    ~fallback:edit_user_page
    ~post_params:((string "pass") ** 
                    (string "pass2") **  (* re-type *)
                    (string "name") **
                    (string "email"))
    ()


let rec view_user_admin_page ~err ~cur_user =
  lwt users = Db.query_users () in
  let users_table = 
    table @@
      (tr 
         [th [pcdata "Id"];
          th [pcdata "Login"];
          th [pcdata "Real Name"];
          th [pcdata "E-mail"]]) ::
      (List.map 
         (fun user ->
            tr 
              [td [pcdata (string_of_int user.user_id)];
               td [pcdata user.user_login];
               td [pcdata user.user_real_name];
               td [pcdata user.user_email];
               td [a ~service:edit_user_page [pcdata "Edit"]
                     (Some "user_admin", user.user_login)]])
         users) in

  return
    (Html_util.html_stub
       (Html_util.navbar_html ~cur_user
          ([h1 [pcdata "Edit users"];
            users_table] @
             err @
            [post_form ~service:service_create_new_user
               (fun (login,(passwd,(passwd2,(name,email)))) ->
                  [h2 [pcdata "Create a new user"];
                   (table
                      [tr
                         [td [pcdata "Login:"];
                          td [string_input ~input_type:`Text ~name:login ()]];
                       tr
                         [td [pcdata "Password:"];
                          td [string_input ~input_type:`Password ~name:passwd ()]];

                       tr
                         [td [pcdata "Re-type password:"];
                          td [string_input ~input_type:`Password ~name:passwd2 ()]];

                       tr
                         [td [pcdata "Name:"];
                          td [string_input ~input_type:`Text ~name:name ()]];

                       tr
                         [td [pcdata "E-mail address:"];
                          td [string_input ~input_type:`Text ~name:email ()]];

                       tr
                         [td [string_input ~input_type:`Submit ~value:"Add User" ()]]
                         ])]) ()])))

(* Only allow certain types of login names to avoid surprises *)
let sanitize_login_name name =
  let rex = Pcre.regexp "^[a-zA-Z_][a-zA-Z0-9_]*$" in
  try Some (Pcre.extract ~rex name).(0) with Not_found -> None

let save_user ~update_user ~login ~passwd ~passwd2 ~real_name ~email =
  let sanitized_login = sanitize_login_name login in
  match sanitized_login with
    None -> 
      return [Html_util.error ("Only alphanumeric chars are allowed in login name!  Got '"^login^"'")]
  | Some login ->
      Db.query_user login >>= fun old_user ->
      if not update_user && old_user <> None then
        return [Html_util.error ("User '"^login^"' already exists!")]
      else if login = "guest" then
        return [Html_util.error ("Cannot create '"^login^"' user.  The login name 'guest' is reserved for internal use!")]
      else if passwd <> passwd2 then
        return [Html_util.error "Re-typed password doesn't match your password!"]
      else 
        begin
          let passwd_md5 = Digest.to_hex (Digest.string passwd) in
          if update_user then
            begin
              match old_user with
                Some u ->
                  (* If no password was entered, set it to old value: *)
                  let new_passwd_md5 = 
                    if passwd = "" then None else Some passwd_md5 in
                  Db.with_conn
                    (fun conn ->
                       Db.update_user ~conn
                         ~user_id:u.user_id ~passwd:new_passwd_md5 ~real_name ~email)
                  >>= fun _ -> return []
              | None ->
                  assert false 
            end
          else
            Db.with_conn
              (fun conn ->
                 Db.add_user ~conn ~login ~passwd:passwd_md5 ~real_name ~email)
            >>= fun _ -> return []
        end

let _ =
  Eliom_registration.Html5.register service_create_new_user
    (fun () (login, (passwd, (passwd2, (real_name, email))))  ->
       Session.with_user_login
         (fun cur_user ->
            Privileges.with_can_create_user cur_user 
              (fun () ->
                 save_user ~update_user:false
                   ~login ~passwd ~passwd2 ~real_name ~email >>= fun err ->
                 view_user_admin_page ~err ~cur_user)
              ~on_fail:(fun e -> return (Html_util.error_page e))))


let save_user_prefs c_passwd c_passwd2 (c_name,old_name) (c_email,old_email) =
  (table
     [tr
        [td [pcdata "New Password:"];
         td [string_input ~input_type:`Password ~name:c_passwd ()];
        ];
      tr
        [td [pcdata "Re-type Password:"];
         td [string_input ~input_type:`Password ~name:c_passwd2 ()]];

      tr 
        [td [pcdata "Name:"];
         td [string_input ~input_type:`Text ~name:c_name
               ~value:old_name ()]];

      tr 
        [td [pcdata "E-mail Address:"];
         td [string_input ~input_type:`Text ~name:c_email
               ~value:old_email ()]];

      tr
        [td [string_input ~input_type:`Submit ~value:"Save User" ()]]
        ])

let _ =
  Eliom_registration.Html5.register user_admin_page
    (fun _ () ->
       Session.with_user_login
         (fun cur_user ->
            Privileges.with_can_view_users cur_user
              (fun () ->
                 view_user_admin_page ~err:[] ~cur_user)
              ~on_fail:(fun e -> return (Html_util.error_page e))))


let rec view_edit_user_page caller ~err ~cur_user user_to_edit =
  Html_util.html_stub
    (Html_util.navbar_html ~cur_user
       ([h1 [pcdata "Edit User"]] @
          err @
          [post_form ~service:service_save_user_edit
             (fun (passwd,(passwd2,(name,email))) ->
                [h2 [pcdata ("Edit User '"^user_to_edit.user_login^"'")];
                 save_user_prefs passwd passwd2 
                   (name,user_to_edit.user_real_name) 
                   (email,user_to_edit.user_email)]) 
             (caller, user_to_edit.user_login)]))


let _ =
  Eliom_registration.Html5.register service_save_user_edit
    (fun (caller, login) (passwd, (passwd2, (real_name, email)))  ->
       Session.with_user_login
         (fun cur_user ->
            Db.query_user login
            >>= function
              | Some user_to_edit ->
                  Privileges.with_can_edit_user cur_user user_to_edit
                    (fun () ->
                         save_user 
                           ~update_user:true 
                           ~login:login
                           ~passwd ~passwd2 ~real_name ~email >>= fun err ->
                       (* Update password in the session if we're editing current
                          user: *)
                       (if err = [] && passwd <> "" && cur_user.user_login = login
                        then Session.update_session_password login passwd
                        else return ()
                       ) >>= fun () ->
                       Session.with_user_login
                         (fun cur_user ->
                            match caller with
                                Some "user_admin" ->
                                  view_user_admin_page ~err ~cur_user
                              | Some _ -> 
                                  return (Html_util.error_page "Invalid caller service!")
                              | None ->
                                  Db.query_user login
                                  >>= function
                                    | Some user ->
                                        return (view_edit_user_page caller ~err ~cur_user user)
                                    | None ->
                                        return (Html_util.error_page "Invalid user!")))
                    ~on_fail:(fun e -> return (Html_util.error_page e))
              | None ->
                  return (Html_util.error_page ("Trying to edit unknown user '"^login^"'"))))


let _ =
  Eliom_registration.Html5.register edit_user_page
    (fun (caller, editing_login) () ->
       Session.with_user_login
         (fun cur_user ->
            Db.query_user editing_login
            >>= function
              | Some user_to_edit ->
                  Privileges.with_can_edit_user cur_user user_to_edit
                    (fun () ->
                       return (view_edit_user_page caller ~err:[] ~cur_user user_to_edit))
                    ~on_fail:(fun e -> return (Html_util.error_page e))
              | None ->
                  return (Html_util.error_page ("Unknown user '"^editing_login^"'"))))
