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

open Lwt
open ExtList
open ExtString

open Services
open Types

module Db = Database

let revision_table page_descr =
  let%lwt revisions = Db.query_page_revisions page_descr in

  let page_link descr (rev:int) = 
    a ~service:wiki_view_page [txt ("Revision "^(string_of_int rev))]
      (descr, (None, (Some rev, None))) in

  let rows =
    List.map 
      (fun r ->
         tr [td [page_link page_descr r.pr_revision];
             td [txt r.pr_created];
             td [txt (Option.default "" r.pr_owner_login)]])
      revisions in

  return
    [table @@
       (tr [th [txt "Revision"]; th [txt "When"]; th [txt "Changed by"]]) ::
       rows]


let view_page_revisions page_descr =
  Session.with_guest_login
    (fun cur_user ->
       revision_table page_descr >>= fun revisions ->
       return
         (Html_util.html_stub
            (Html_util.navbar_html ~cur_user
               (h1 [txt (page_descr ^ " Revisions")] :: revisions))))

(* /page_revisions?page_id=<id> *)
let _ =
  Eliom_registration.Html.register page_revisions_page
    (fun page_descr () ->
       view_page_revisions page_descr)
