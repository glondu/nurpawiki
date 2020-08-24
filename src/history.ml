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

module P = Printf
open Eliom_content.Html.F

open Eliom_parameter
open Eliom_service

open Lwt
open ExtList
open ExtString

open Services
open Types
open Util

open CalendarLib

module Db = Database

let ( & ) f x = f x

let n_log_items_per_page = 300

let descr_of_activity_type = function
    AT_create_todo -> "Created"
  | AT_complete_todo -> "Completed"
  | AT_work_on_todo -> "Worked on"
  | AT_create_page -> "Created"
  | AT_edit_page -> "Edited"
  | AT_uncomplete_todo -> "Resurrected"

module ReverseOrdString =
  struct
    type t = String.t
    let compare a b = String.compare b a
  end

module RSMap = Map.Make (ReverseOrdString)

type act_group =
    {
      ag_created_todos : (string * string * page list) list;
      ag_completed_todos : (string * string * page list) list;
      ag_resurrected_todos : (string * string * page list) list;
      ag_edited_pages : page list;
      ag_page_editors : string list;
    }

let empty_act_group =
  {
      ag_created_todos = [];
      ag_completed_todos = [];
      ag_resurrected_todos = [];
      ag_page_editors = [];
      ag_edited_pages = [];
  }

let group_activities activities activity_in_pages =
  List.fold_left
    (fun acc a ->
        let date = date_of_date_time_string a.a_date in
        let d = Printer.DatePrinter.sprint "%Y-%m-%d" date in
        let ag = try RSMap.find d acc with Not_found -> empty_act_group in
        let pages = try IMap.find a.a_id activity_in_pages with Not_found -> [] in
        let opt_to_str o = Option.default "" o in
        let ag' =
          let changed_by = opt_to_str a.a_changed_by in
          match a.a_activity with
            AT_create_todo ->
              (match a.a_todo_descr with
                 Some descr ->
                   let e = (descr, changed_by, pages)::ag.ag_created_todos in
                   { ag with ag_created_todos = e }
               | None -> P.eprintf "no descr in activity_log %i\n" a.a_id; ag)
          | AT_complete_todo ->
              (match a.a_todo_descr with
                 Some descr ->
                   let e = (descr, changed_by, pages)::ag.ag_completed_todos in
                   { ag with ag_completed_todos = e }
               | None -> P.eprintf "no descr in activity_log %i\n" a.a_id; ag)
          | AT_uncomplete_todo ->
              (match a.a_todo_descr with
                 Some descr ->
                   let e = (descr, changed_by, pages)::ag.ag_resurrected_todos in
                   { ag with ag_resurrected_todos = e }
               | None -> P.eprintf "no descr in activity_log %i\n" a.a_id; ag)
          | AT_create_page | AT_edit_page ->
              let add_editor e acc =
                if List.mem e acc then acc else e::acc in
              { ag with
                  ag_page_editors = add_editor changed_by ag.ag_page_editors;
                  ag_edited_pages = pages @ ag.ag_edited_pages }
          | AT_work_on_todo -> ag in
        RSMap.add d ag' acc)
    RSMap.empty activities

let remove_duplicates strs =
  let module PSet =
    Set.Make (struct
                type t = page
                let compare a b =  compare a.p_descr b.p_descr
              end) in
  let s =
    List.fold_left (fun acc e -> PSet.add e acc) PSet.empty strs in
  PSet.fold (fun e acc -> e::acc) s []

let page_links cur_page max_pages =
  let links = ref [] in
  for i = 0 to max_pages do
    let p = string_of_int i in
    let link =
      if cur_page = i then
        strong [txt p]
      else
        a ~service:history_page [txt p] (Some i) in
    links := link :: txt " " :: !links
  done;
  txt "More pages: " :: List.rev !links

let view_history_page ~cur_user ~nth_page =
  let%lwt highest_log_id = Database.query_highest_activity_id () in
  (* max_id is inclusive, min_id exclusive, hence 1 and 0 *)
  let max_id = max 1 (highest_log_id - nth_page * n_log_items_per_page) in
  let min_id = max 0 (max_id - n_log_items_per_page) in
  let n_total_pages = highest_log_id / n_log_items_per_page in
  let%lwt activity =
    Database.query_past_activity ~min_id ~max_id in
  let%lwt activity_in_pages =
    Database.query_activity_in_pages ~min_id ~max_id in

  let prettify_date d =
    let d = date_of_date_time_string d in
    Printer.DatePrinter.sprint "%a %b %d, %Y" d in

  let activity_groups = group_activities activity activity_in_pages in

  let act_table =
    table ~a:[a_class ["todo_table"]] @@
      (tr [th [];
           th [txt "Activity"];
           th [txt "By"];
           th [txt "Details"]]) ::
      (List.rev
         (fst
            (RSMap.fold
               (fun date e (lst_acc,prev_date) ->
                  let prettified_date = prettify_date date in
                  let date_text =
                    if prev_date = prettified_date then
                      []
                    else
                      [txt prettified_date] in

                  let todo_html ty lst =
                    List.rev
                      (List.mapi
                         (fun ndx (todo,changed_by,pages) ->
                            (tr [td [];
                                 td (if ndx = 0 then [txt ty] else []);
                                 td ~a:[a_class ["todo_owner"]] [txt changed_by];
                                 td ([txt todo] @
                                        (Html_util.todo_page_links_of_pages
                                           ~colorize:true pages))]))
                         lst) in

                  let created_todos =
                    todo_html "Created" e.ag_created_todos in
                  let completed_todos =
                    todo_html "Completed" e.ag_completed_todos in
                  let resurrected_todos =
                    todo_html "Resurrected" e.ag_resurrected_todos in
                  let pages_html =
                    if e.ag_edited_pages <> [] then
                      [tr [td [];
                           td [txt "Edited"];
                           td
                             ~a:[a_class ["todo_owner"]]
                             [txt (String.concat "," e.ag_page_editors)];
                           td (Html_util.todo_page_links_of_pages
                                 ~colorize:true ~insert_parens:false
                                 (remove_duplicates e.ag_edited_pages))]]
                    else
                      [] in

                  (* NOTE: 'tr' comes last as we're building the page
                     in reverse order *)
                  (pages_html @ created_todos @ completed_todos @ resurrected_todos @
                     [tr [td ~a:[a_class ["no_break"; "h_date_heading"]] date_text]] @ lst_acc,
                   prettified_date))
               activity_groups ([],"")))) in
  return & Html_util.html_stub ~title:"History"
    (Html_util.navbar_html ~cur_user
       ([h1 [txt "Blast from the past"]] @
          (page_links nth_page n_total_pages) @ [br (); br ()] @
          [act_table]))

(* /history *)
let _ =
  Eliom_registration.Html.register history_page
    (fun nth_page () ->
       Session.with_guest_login
         (fun cur_user ->
            let page = Option.default 0 nth_page in
            view_history_page ~cur_user ~nth_page:page))
