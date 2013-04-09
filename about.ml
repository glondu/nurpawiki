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

open Eliom_content.Html5.F

open Lwt

open Services
open Types

let about_page_html =
  [h1 [pcdata "About Nurpawiki"];
   p 
     [pcdata ("Nurpawiki v"^Version.version^
                " Copyright (c) 2007, 2008 Janne Hellsten <jjhellst@gmail.com>");
      br ();
      br ();
      pcdata "See the ";
      Raw.a ~a:[a_href (uri_of_string (fun () -> "http://code.google.com/p/nurpawiki"))]
        [pcdata "project homepage"];
      pcdata "."]]

let _ =
  Eliom_registration.Html5.register about_page
    (fun () () ->
       Session.with_guest_login
         (fun cur_user ->
            return
              (Html_util.html_stub
                 (Html_util.navbar_html ~cur_user about_page_html))))
