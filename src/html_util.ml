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
open Eliom_content.Html.F.Form

open Eliom_parameter
open Eliom_service

open Lwt

open Config
open Types
open Services

let make_static_uri name =
  make_uri (static_dir ()) name

let disconnect_box s =
  a ~service:disconnect_page [txt s] ()

(* Use this as the basis for all pages.  Includes CSS etc. *)
let html_stub ?(javascript=[]) ~title body_html =
  let script src =
    js_script ~a:[a_defer ()] ~uri:(make_static_uri src) () in
  let scripts  =
    script ["nurpawiki.js"] :: (List.map script javascript) in
  html ~a:[a_xmlns `W3_org_1999_xhtml]
    (head
       (Eliom_content.Html.F.title (txt (title ^ " — Nurpawiki")))
       ((scripts) @
          [css_link ~a:[] ~uri:(make_uri ~service:(static_dir ())
                                  ["style.css"]) ();
           css_link ~a:[] ~uri:(make_uri ~service:(static_dir ())
                                  ["jscalendar"; "calendar-blue2.css"]) ()]))
    (body body_html)

let is_guest user =
  user.user_login = "guest"

let navbar_html ~cur_user ?(top_info_bar=[]) ?(wiki_revisions_link=[]) ?(wiki_page_links=[]) ?(todo_list_table=[]) content =
  let home_link link_text =
    a ~service:wiki_view_page
      ~a:[a_accesskey 'h'; a_class ["ak"]] link_text
      (Config.site.cfg_homepage, (None, (None, None))) in
  let scheduler_link =
    a ~service:scheduler_page
      ~a:[a_accesskey 'r'; a_class ["ak"]]
      [img ~alt:"Scheduler" ~src:(make_static_uri ["calendar.png"]) ();
       txt "Scheduler"] () in
  let history_link =
    a ~service:history_page
      ~a:[a_accesskey 'r'; a_class ["ak"]]
      [img ~alt:"History" ~src:(make_static_uri ["home.png"]) ();
       txt "History"] None in

  let search_input =
    [get_form search_page
       (fun (chain : ([`One of string] param_name)) ->
          [p [input ~input_type:`Submit ~value:"Search" Form.string;
              input ~input_type:`Text ~name:chain Form.string]])] in

  (* Greet user and offer Login link if guest *)
  let user_greeting =
    txt ("Howdy "^cur_user.user_login^"!  ") ::
      if is_guest cur_user then
        [br(); br ();
         txt "To login as an existing user, click ";
         a ~a:[a_class ["login_link_big"]] ~service:wiki_view_page
           [txt "here"]
           (Config.site.cfg_homepage,(None,(None, Some true)));
         txt ".";
         br (); br ();
         txt "Guests cannot modify the site.  Ask the site admin for an account to be able to edit content."]
      else
        [] in

  let disconnect_link =
    if is_guest cur_user then [] else [disconnect_box "Logout"] in

  let my_preferences_link =
    if is_guest cur_user then
      []
    else
      [a ~service:edit_user_page [txt "My Preferences"]
         (None,cur_user.user_login)] in

  let edit_users_link =
    if Privileges.can_view_users cur_user then
      [a ~service:user_admin_page [txt "Edit Users"] ()]
    else
      [] in

  [div ~a:[a_id "topbar"]
     [table ~a:[a_class ["top_menu_size"]]
        [tr
           [td ~a:[a_class ["top_menu_left_align"]]
              [table
                 [tr [td [home_link
                            [img ~alt:"Home" ~src:(make_static_uri ["home.png"]) ();
                             txt "Home"]];
                      td [scheduler_link];
                      td [history_link];
                      td wiki_page_links]]
                 ];
            td ~a:[a_class ["top_menu_right_align"]]
              ([a ~service:about_page [txt "About"] ()] @
                 [txt " "] @
                 my_preferences_link @
                 [txt " "] @
                 edit_users_link @
                 [txt " "] @
                 disconnect_link)]]]]
  @
    (if top_info_bar = [] then [] else [div ~a:[a_id "top_action_bar"] top_info_bar])
  @
    [div ~a:[a_id "navbar"]
       (user_greeting @ [br ()] @ search_input @ wiki_revisions_link @ todo_list_table);
     div ~a:[a_id "content"]
       content]

let error text =
  span ~a:[a_class ["error"]] [txt text]

let error_page msg =
  html_stub ~title:"Error"
    [p [error msg]]


let string_of_priority = function
    3 -> "lo"
  | 2 -> "med"
  | 1 -> "hi"
  | _ -> "INTERNAL ERROR: PRIORITY OUT OF RANGE"

let priority_css_class p =
  "todo_pri_"^(string_of_priority p)

(* Hash page description to a CSS palette entry.  Used to syntax
   highlight wiki page links based on their names. *)
let css_palette_ndx_of_wikipage page_id =
  "palette"^(string_of_int (page_id mod 12))

let todo_page_links_of_pages ?(colorize=false) ?(link_css_class=None) ?(insert_parens=true) pages =
  let attrs page =
    let color_css =
      if colorize then [css_palette_ndx_of_wikipage page.p_id] else [] in
    match link_css_class with
      Some c -> [a_class ([c] @ color_css)]
    | None -> [a_class color_css] in
  let link page =
    a ~a:(attrs page) ~service:wiki_view_page [txt page.p_descr]
      (page.p_descr,(None,(None,None))) in
  let rec insert_commas acc = function
      (x::_::xs) as lst ->
        insert_commas (txt ", "::x::acc) (List.tl lst)
    | x::[] ->
        insert_commas (x::acc) []
    | [] -> List.rev acc in
  let insert_parens_html lst =
    txt " ("::lst @ [txt ")"] in
  if pages <> [] then
    let lst = insert_commas [] (List.map link pages) in
    if insert_parens then
      insert_parens_html lst
    else
      lst
  else
    []

let todo_page_links todo_in_pages ?(colorize=false) ?(link_css_class=None) ?(insert_parens=true) id =
  let pages = try IMap.find id todo_in_pages with Not_found -> [] in
  todo_page_links_of_pages ~colorize pages

let todo_edit_img_link page_cont task_id =
  [a ~a:[a_title "Edit"] ~service:edit_todo_get_page
     [img ~alt:"Edit"
        ~src:(make_static_uri ["edit_small.png"]) ()]
     (page_cont, Some task_id)]

let complete_task_img_link task_id =
  let img_html =
    [img ~alt:"Mark complete"
       ~src:(make_static_uri ["mark_complete.png"]) ()] in
  a ~service:task_side_effect_complete_action
    ~a:[a_title "Mark as completed!"] img_html task_id

let todo_descr_html descr owner =
  match owner with
    None -> [txt descr]
  | Some o ->
      [txt descr;
       span ~a:[a_class ["todo_owner"]] [txt (" ["^o.owner_login^"] ")]]


(* Use to create a "cancel" button for user submits *)
let cancel_link service params =
  a ~a:[a_class ["cancel_edit"]] ~service:service
    [txt "Cancel"]
    params
