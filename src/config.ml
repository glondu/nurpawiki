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

module P = Printf

open Xml

type db_config = 
    {
      db_name : string;
      db_user : string;
      db_host : string option;
      db_port : string option;
      db_pass : string option;
    }

type site_config =
    {
      cfg_allow_ro_guests : bool;
      cfg_homepage : string;
    }

let get_attr_opt attr attrs = 
  try Some (List.assoc attr attrs)
  with Not_found -> None

let get_attr_with_err e attr attrs =
  try (List.assoc attr attrs)
  with Not_found -> 
    raise (Ocsigen_extensions.Error_in_config_file 
             ("Expecting "^e^"."^attr^" attribute in Nurpawiki config"))

let dbcfg =
  let rec find_dbcfg = function
      (Element ("database", attrs, _)::_) ->
        let dbname = get_attr_with_err "database" "name" attrs in
        let dbuser = get_attr_with_err "database" "user" attrs in
        let dbhost = get_attr_opt "host" attrs in
        let dbport = get_attr_opt "port" attrs in
        let dbpass = get_attr_opt "password" attrs in
        {
          db_name = dbname;
          db_user = dbuser;
          db_host = dbhost;
          db_port = dbport;
          db_pass = dbpass;
        }
    | x::xs -> 
        find_dbcfg xs
    | [] -> 
        raise (Ocsigen_extensions.Error_in_config_file ("Couldn't find database element from config")) in
  find_dbcfg (Eliom_config.get_config ())

let site =
  let rec find_site_cfg = function
      (Element ("nurpawiki", attrs, _))::_ ->
        let allow_ro_guests = 
          (match get_attr_opt "allow_read_only_guests" attrs with
             Some s -> s = "yes"
           | None -> false) in
        let homepage =
          (match get_attr_opt "homepage" attrs with
             Some s -> s
           | None -> "WikiStart") in
        {
          cfg_allow_ro_guests = allow_ro_guests;
          cfg_homepage = homepage;
        }
    | (Element (x,_,_))::xs -> 
        Ocsigen_messages.errlog x;
        find_site_cfg xs 
    | _ ->
        {
          cfg_allow_ro_guests = false;
          cfg_homepage = "WikiStart";
        } in
  let cfg = find_site_cfg (Eliom_config.get_config ()) in
  Ocsigen_messages.warning (P.sprintf "read-only guests allowed %b" cfg.cfg_allow_ro_guests);
  cfg
