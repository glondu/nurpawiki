(* Copyright (c) 2007-2008 Janne Hellsten <jjhellst@gmail.com> *)

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

open CalendarLib

let match_pcre_option ?(charpos=0) rex s =
  (* Pcre's ~pos seems to work quite differently from Str's begin
     character.  The below sub string hack is to make Pcre extract
     behave the same way as Str's match *)
  let s = String.sub s charpos ((String.length s) - charpos) in
  try Some (Pcre.extract (* ~pos:charpos *) ~rex s) with Not_found -> None

let iso_date_re = Pcre.regexp "([0-9]+)-([0-9]+)-([0-9]+)"

let date_of_string s = 
  match match_pcre_option iso_date_re s with
    Some r ->
      let year = int_of_string r.(1) in
      let month = int_of_string r.(2) in
      let day = int_of_string r.(3) in
      Date.make year month day
  | None -> assert false


let iso_date_time_re = Pcre.regexp "([0-9]+)-([0-9]+)-([0-9]+) .*"

let date_of_date_time_string s =
  match (match_pcre_option iso_date_time_re s,
         match_pcre_option iso_date_re s) with
    (Some r,_)|(_,Some r) ->
      let year = int_of_string r.(1) in
      let month = int_of_string r.(2) in
      let day = int_of_string r.(3) in
      Date.make year month day
  | _ -> 
      Ocsigen_messages.errlog ("invalid date '"^s^"'");
      assert false


(** [del_substring s c] return [s] with all occurrences of substring [c]
    removed. *)
let del_substring s c =
  let q = Pcre.regexp (Pcre.quote c) in
  Pcre.replace ~rex:q ~templ:"" s

(* Unit tests *)
let _ =
  let a = "\\*foo\\*baz\\*" in
  assert (del_substring a "\\*" = "foobaz");
  let b = "__foo__" in
  assert (del_substring b "_" = "foo")
