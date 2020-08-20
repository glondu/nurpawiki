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

open Types
module Psql = Postgresql
module P = Printf

open Config

type connection = Psql.connection

let ( |> ) f g = g f

module ConnectionPool =
  struct
    open Psql

    (* We have only one connection to pool from for now.  This will
       likely be extended for more connetcions in the future.  There's
       no need for it yet though. *)

    let connection_mutex = Mutex.create ()
    let connection : Postgresql.connection option ref = ref None

    let with_mutex m f =
      Mutex.lock m;
      try
        let r = f () in
        Mutex.unlock m;
        r
      with
        x ->
          Mutex.unlock m;
          raise x

    (* NOTE we may get a deadlock here if someone uses nested
       with_conn calls.  This should not happen unless there's a
       programming error somewhere.  This case should go away if there
       are more than one DB connections available for with_conn.
       Currently there's only one connection though. *)
    let with_conn_priv (f : (Psql.connection -> 'a)) =
      (* TODO the error handling here is not still very robust. *)
      with_mutex connection_mutex
        (fun () ->
           match !connection with
             Some c ->
               (* Re-use the old connection. *)
               (match c#status with
                  Ok ->
                    f c
                | Bad ->
                    Ocsigen_messages.errlog "Database connection bad.  Trying reset";
                    c#reset;
                    match c#status with
                      Ok ->
                        f c
                    | Bad ->
                        Ocsigen_messages.errlog "Database connection still bad.  Bail out";
                        raise (Error (Psql.Connection_failure "bad connection")))
           | None ->
               let host = Option.default "localhost" dbcfg.db_host in
               let port = Option.default "" dbcfg.db_port in
               let password = Option.default "" dbcfg.db_pass in
               let c =
                 new Psql.connection ~host ~port ~password
                   ~dbname:dbcfg.db_name ~user:dbcfg.db_user
                   () in
               connection := Some c;
               (* Search tables from nurpawiki schema first: *)
               f c)

    let with_conn f =
      try
        with_conn_priv f
      with
        (Psql.Error e) as ex ->
          Ocsigen_messages.errlog (P.sprintf "psql failed : %s\n" (Psql.string_of_error e));
          raise ex
      | x ->
          raise x


  end

let with_conn f =
  Lwt_preemptive.detach ConnectionPool.with_conn f

(* Escape a string for SQL query *)
let escape ~(conn:connection) s = conn#escape_string s

let todos_user_login_join = "FROM nw.todos LEFT OUTER JOIN nw.users ON nw.todos.user_id = nw.users.id"

(* Use this tuple format when querying TODOs to be parsed by
   parse_todo_result *)
let todo_tuple_format = "nw.todos.id,descr,completed,priority,activation_date,user_id,nw.users.login"

let todo_of_row row =
  let id = int_of_string (List.nth row 0) in
  let descr = List.nth row 1 in
  let completed = (List.nth row 2) = "t" in
  let owner_id = List.nth row 5 in
  let owner =
    if owner_id = "" then
      None
    else
      Some {
        owner_id = int_of_string owner_id;
        owner_login = List.nth row 6;
      } in

  let pri = List.nth row 3 in
  {
    t_id = id;
    t_descr = descr;
    t_completed = completed;
    t_priority = int_of_string pri;
    t_activation_date =  List.nth row 4;
    t_owner = owner;
  }

let parse_todo_result res =
  List.fold_left
    (fun acc row ->
       let id = int_of_string (List.nth row 0) in
       IMap.add id (todo_of_row row) acc)
    IMap.empty res#get_all_lst

let guarded_exec ~(conn : Psql.connection) query =
  try
    conn#exec query
  with
    (Psql.Error e) as ex ->
      (match e with
         Psql.Connection_failure msg ->
           P.eprintf "psql failed : %s\n" msg;
           raise ex
       | _ ->
           P.eprintf "psql failed : %s\n" (Psql.string_of_error e);
           raise ex)

let insert_todo_activity ~user_id todo_id ?(page_ids=None) activity =
  let user_id_s = string_of_int user_id in
  match page_ids with
    None ->
      "INSERT INTO nw.activity_log(activity_id,user_id,todo_id) VALUES ("^
        (string_of_int (int_of_activity_type activity))^", "^user_id_s^
        ", "^todo_id^")"
  | Some pages ->
      let insert_pages =
        List.map
          (fun page_id ->
             "INSERT INTO nw.activity_in_pages(activity_log_id,page_id) "^
               "VALUES (CURRVAL('nw.activity_log_id_seq'), "^string_of_int page_id^")")
          pages in
      let page_act_insert = String.concat "; " insert_pages in
      "INSERT INTO nw.activity_log(activity_id,user_id,todo_id) VALUES ("^
        (string_of_int (int_of_activity_type activity))^", "^
        user_id_s^", "^todo_id^"); "^
        page_act_insert

let insert_save_page_activity ~user_id (page_id : int) = with_conn (fun conn ->
  let sql = "BEGIN;
INSERT INTO nw.activity_log(activity_id, user_id)
       VALUES ("^(string_of_int (int_of_activity_type AT_edit_page))^
    " ,"^(string_of_int user_id)^");
INSERT INTO nw.activity_in_pages(activity_log_id,page_id)
       VALUES (CURRVAL('nw.activity_log_id_seq'), "^string_of_int page_id^");
COMMIT" in
  ignore (guarded_exec ~conn sql)
)

let query_todos_by_ids_raw todo_ids conn =
  if todo_ids <> [] then
    let ids = String.concat "," (List.map string_of_int todo_ids) in
    let r =
      guarded_exec ~conn
      ("SELECT "^todo_tuple_format^" "^todos_user_login_join^" WHERE nw.todos.id IN ("^ids^")") in
    List.map todo_of_row (r#get_all_lst)
  else
    []

let query_todos_by_ids todo_ids = with_conn (query_todos_by_ids_raw todo_ids)

let query_todo id = with_conn (fun conn ->
  match query_todos_by_ids_raw [id] conn with
    [task] -> Some task
  | [] -> None
  | _ -> None
)

let todo_exists id =
  match%lwt query_todo id with Some _ -> Lwt.return true | None -> Lwt.return false

let update_todo_activation_date todo_id new_date = with_conn (fun conn ->
  let sql =
    "UPDATE nw.todos SET activation_date = '"^new_date^"' WHERE id = "^
      (string_of_int todo_id) in
  ignore (guarded_exec ~conn sql)
)


let update_todo_descr todo_id new_descr = with_conn (fun conn ->
  let sql =
    "UPDATE nw.todos SET descr = '"^escape ~conn new_descr^"' WHERE id = "^
      (string_of_int todo_id) in
  ignore (guarded_exec ~conn sql)
)


let update_todo_owner_id todo_id owner_id = with_conn (fun conn ->
  let owner_id_s =
    match owner_id with
      Some id -> string_of_int id
    | None -> "NULL" in
  let sql =
    "UPDATE nw.todos SET user_id = "^owner_id_s^" WHERE id = "^
      (string_of_int todo_id) in
  ignore (guarded_exec ~conn sql)
)


let select_current_user id =
  (match id with
     None -> ""
   | Some user_id ->
       " AND (user_id = "^string_of_int user_id^" OR user_id IS NULL) ")

(* Query TODOs and sort by priority & completeness *)
let query_all_active_todos ~current_user_id () = with_conn (fun conn ->
  let r = guarded_exec ~conn
    ("SELECT "^todo_tuple_format^" "^todos_user_login_join^" "^
       "WHERE activation_date <= current_date AND completed = 'f' "^
       select_current_user current_user_id^
       "ORDER BY completed,priority,id") in
  List.map todo_of_row r#get_all_lst
)

let query_upcoming_todos ~current_user_id date_criterion = with_conn (fun conn ->
  let date_comparison =
    let dayify d =
      "'"^string_of_int d^" days'" in
    match date_criterion with
      (None,Some days) ->
        "(activation_date > now()) AND (now()+interval "^dayify days^
          " >= activation_date)"
    | (Some d1,Some d2) ->
        let sd1 = dayify d1 in
        let sd2 = dayify d2 in
        "(activation_date > now()+interval "^sd1^") AND (now()+interval "^sd2^
          " >= activation_date)"
    | (Some d1,None) ->
        let sd1 = dayify d1 in
        "(activation_date > now()+interval "^sd1^")"
    | (None,None) ->
        "activation_date <= now()" in
  let r = guarded_exec ~conn
    ("SELECT "^todo_tuple_format^" "^todos_user_login_join^" "^
       "WHERE "^date_comparison^
       select_current_user current_user_id^
       " AND completed='f' ORDER BY activation_date,priority,id") in
  List.map todo_of_row r#get_all_lst
)

let new_todo ~conn page_id user_id descr =
  (* TODO: could wrap this into BEGIN .. COMMIT if I knew how to
     return the data from the query! *)
  let sql =
    "INSERT INTO nw.todos(user_id,descr) values('"^(string_of_int user_id)^"','"^escape ~conn descr^"');
 INSERT INTO nw.todos_in_pages(todo_id,page_id) values(CURRVAL('nw.todos_id_seq'), "
    ^string_of_int page_id^");"^
      (insert_todo_activity ~user_id
         "(SELECT CURRVAL('nw.todos_id_seq'))" ~page_ids:(Some [page_id])
         AT_create_todo)^";
 SELECT CURRVAL('nw.todos_id_seq')" in
  let r = guarded_exec ~conn sql in
  (* Get ID of the inserted item: *)
  (r#get_tuple 0).(0)

(* Mapping from a todo_id to page list *)
let todos_in_pages_raw todo_ids conn =
  (* Don't query if the list is empty: *)
  if todo_ids = [] then
    IMap.empty
  else
    let ids = String.concat "," (List.map string_of_int todo_ids) in
    let sql =
      "SELECT todo_id,page_id,page_descr "^
        "FROM nw.todos_in_pages,nw.pages WHERE todo_id IN ("^ids^") AND page_id = nw.pages.id" in
    let r = guarded_exec ~conn sql in
    let rows = r#get_all_lst in
    List.fold_left
      (fun acc row ->
         let todo_id = int_of_string (List.nth row 0) in
         let page_id = int_of_string (List.nth row 1) in
         let page_descr = List.nth row 2 in
         let lst = try IMap.find todo_id acc with Not_found -> [] in
         IMap.add todo_id ({ p_id = page_id; p_descr = page_descr }::lst) acc)
      IMap.empty rows

let todos_in_pages todo_ids = with_conn (todos_in_pages_raw todo_ids)

(* TODO must not query ALL activities.  Later we only want to
   currently visible activities => pages available. *)
let query_activity_in_pages ~min_id ~max_id = with_conn (fun conn ->
  let sql =
    "SELECT activity_log_id,page_id,page_descr
       FROM nw.activity_in_pages,nw.pages
      WHERE page_id = pages.id
        AND (activity_log_id > "^string_of_int min_id^"
             AND activity_log_id <= "^string_of_int max_id^")" in
  let r = guarded_exec ~conn sql in
  List.fold_left
    (fun acc row ->
       let act_id = int_of_string (List.nth row 0) in
       let page_id = int_of_string (List.nth row 1) in
       let page_descr = List.nth row 2 in
       let lst = try IMap.find act_id acc with Not_found -> [] in
       IMap.add act_id ({ p_id = page_id; p_descr = page_descr }::lst) acc)
    IMap.empty (r#get_all_lst)
)

(* Note: This function should only be used in contexts where there
   will be no concurrency issues.  Automated sessions should be used for
   real ID inserts.  In its current form, this function is used to get
   the highest activity log item ID in order to display history separated
   into multiple web pages. *)
let query_highest_activity_id () = with_conn (fun conn ->
  let sql = "SELECT last_value FROM nw.activity_log_id_seq" in
  let r = guarded_exec ~conn sql in
  int_of_string (r#get_tuple 0).(0)
)


(* Collect todos in the current page *)
let query_page_todos page_id = with_conn (fun conn ->
  let sql = "SELECT "^todo_tuple_format^" "^todos_user_login_join^" WHERE nw.todos.id in "^
    "(SELECT todo_id FROM nw.todos_in_pages WHERE page_id = "^string_of_int page_id^")" in
  let r = guarded_exec ~conn sql in
  parse_todo_result r
)

(* Make sure todos are assigned to correct pages and that pages
   don't contain old todos moved to other pages or removed. *)
let update_page_todos page_id todos = with_conn (fun conn ->
  let page_id' = string_of_int page_id in
  let sql =
    "BEGIN;
 DELETE FROM nw.todos_in_pages WHERE page_id = "^page_id'^";"^
      (String.concat ""
         (List.map
            (fun todo_id ->
               "INSERT INTO nw.todos_in_pages(todo_id,page_id)"^
                 " values("^(string_of_int todo_id)^", "^page_id'^");")
            todos)) ^
      "COMMIT" in
  ignore (guarded_exec ~conn sql)
)

(* Mark task as complete and set completion date for today *)
let complete_task_generic ~user_id id op = with_conn (fun conn ->
  let (activity,task_complete_flag) =
    match op with
      `Complete_task -> (AT_complete_todo, "t")
    | `Resurrect_task -> (AT_uncomplete_todo, "f") in
  let page_ids =
    try
      Some (List.map (fun p -> p.p_id) (IMap.find id (todos_in_pages_raw [id] conn)))
    with Not_found -> None in
  let ids = string_of_int id in
  let sql = "BEGIN;
UPDATE nw.todos SET completed = '"^task_complete_flag^"' where id="^ids^";"^
    (insert_todo_activity ~user_id ids ~page_ids activity)^"; COMMIT" in
  ignore (guarded_exec ~conn sql)
)

(* Mark task as complete and set completion date for today *)
let complete_task ~user_id id =
  complete_task_generic ~user_id id `Complete_task

let uncomplete_task ~user_id id =
  complete_task_generic ~user_id id `Resurrect_task

let query_task_priority ~conn id =
  let sql = "SELECT priority FROM nw.todos WHERE id = "^string_of_int id in
  let r = guarded_exec ~conn sql in
  int_of_string (r#get_tuple 0).(0)

(* TODO offset_task_priority can probably be written in one
   query instead of two (i.e., first one SELECT and then UPDATE
   based on that. *)
let offset_task_priority id incr = with_conn (fun conn ->
  let pri = min (max (query_task_priority ~conn id + incr) 1) 3 in
  let sql =
    "UPDATE nw.todos SET priority = '"^(string_of_int pri)^
      "' where id="^string_of_int id in
  ignore (guarded_exec ~conn sql)
)

let up_task_priority id =
  offset_task_priority id (-1)

let down_task_priority id =
  offset_task_priority id 1

let new_wiki_page ~user_id page = with_conn (fun conn ->
  let sql =
    "INSERT INTO nw.pages (page_descr) VALUES ('"^escape ~conn page^"');
     INSERT INTO nw.wikitext (page_id,page_created_by_user_id,page_text)
             VALUES ((SELECT CURRVAL('nw.pages_id_seq')),
                      "^string_of_int user_id^", ''); "^
      "SELECT CURRVAL('nw.pages_id_seq')" in
  let r = guarded_exec ~conn sql in
  int_of_string ((r#get_tuple 0).(0))
)

(* See WikiPageVersioning on docs wiki for more details on the SQL
   queries. *)
let save_wiki_page page_id ~user_id lines = with_conn (fun conn ->
  let page_id_s = string_of_int page_id in
  let user_id_s = string_of_int user_id in
  let escaped = escape ~conn (String.concat "\n" lines) in
  (* Ensure no one else can update the head revision while we're
     modifying it Selecting for UPDATE means no one else can SELECT FOR
     UPDATE this row.  If value (head_revision+1) is only computed and used
     inside this row lock, we should be protected against two (or more)
     users creating the same revision head. *)
  let sql = "
BEGIN;
SELECT * from nw.pages WHERE id = "^page_id_s^";

-- Set ID of next revision
UPDATE nw.pages SET head_revision = nw.pages.head_revision+1
  WHERE id = "^page_id_s^";

-- Kill search vectors for previous version so that only
-- the latest version of the wikitext can be found using
-- full text search.
--
-- NOTE tsearch2 indexing trigger is set to run index updates
-- only on INSERTs and not on UPDATEs.  I wanted to be
-- more future proof and set it trigger on UPDATE as well,
-- but I don't know how to NOT have tsearch2 trigger
-- overwrite the below UPDATE with its own index.
UPDATE nw.wikitext SET page_searchv = NULL WHERE page_id = "^page_id_s^";

INSERT INTO nw.wikitext (page_id, page_created_by_user_id, page_revision, page_text)
  VALUES ("^page_id_s^", "^user_id_s^",
  (SELECT head_revision FROM nw.pages where id = "^page_id_s^"),
  E'"^escaped^"');

COMMIT" in
  ignore (guarded_exec ~conn sql)
)

let find_page_id_raw descr conn =
  let sql =
    "SELECT id FROM nw.pages WHERE page_descr = '"^escape ~conn descr^"' LIMIT 1" in
  let r = guarded_exec ~conn sql in
  if r#ntuples = 0 then None else Some (int_of_string (r#get_tuple 0).(0))

let find_page_id descr = with_conn (find_page_id_raw descr)

let page_id_of_page_name descr =
  with_conn (fun conn -> Option.get (find_page_id_raw descr conn))

let wiki_page_exists page_descr =
  with_conn (fun conn -> find_page_id_raw page_descr conn <> None)

let is_legal_page_revision ~conn page_id_s rev_id =
  let sql = "
SELECT page_id FROM nw.wikitext
 WHERE page_id="^page_id_s^" AND page_revision="^string_of_int rev_id in
  let r = guarded_exec ~conn sql in
  r#ntuples <> 0

(* Load a certain revision of a wiki page.  If the given revision is
   not known, default to head revision. *)
let load_wiki_page ?(revision_id=None) page_id = with_conn (fun conn ->
  let page_id_s = string_of_int page_id in
  let head_rev_select =
    "(SELECT head_revision FROM nw.pages WHERE id = "^page_id_s^")" in
  let revision_s =
    match revision_id with
      None -> head_rev_select
    | Some r ->
        if is_legal_page_revision ~conn page_id_s r then
          string_of_int r
        else
          head_rev_select in
  let sql = "
SELECT page_text FROM nw.wikitext
 WHERE page_id="^string_of_int page_id^" AND
       page_revision="^revision_s^" LIMIT 1" in
  let r = guarded_exec ~conn sql in
  (r#get_tuple 0).(0)
)

let query_page_revisions page_descr = with_conn (fun conn ->
  match find_page_id_raw page_descr conn with
    None -> []
  | Some page_id ->
      let option_of_empty s f =
        if s = "" then None else Some (f s) in
      let sql = "
SELECT page_revision,nw.users.id,nw.users.login,date_trunc('second', page_created) FROM nw.wikitext
  LEFT OUTER JOIN nw.users on page_created_by_user_id = nw.users.id
  WHERE page_id = "^string_of_int page_id^"
  ORDER BY page_revision DESC" in
      let r = guarded_exec ~conn sql in
      List.map
        (fun r ->
           {
             pr_revision = int_of_string (List.nth r 0);
             pr_owner_id = option_of_empty (List.nth r 1) int_of_string;
             pr_owner_login = option_of_empty (List.nth r 2) Std.identity;
             pr_created = List.nth r 3;
           })
        (r#get_all_lst)
)


let query_past_activity ~min_id ~max_id = with_conn (fun conn ->
  let sql =
    "SELECT nw.activity_log.id,activity_id,activity_timestamp,nw.todos.descr,nw.users.login
      FROM nw.activity_log
       LEFT OUTER JOIN nw.todos ON nw.activity_log.todo_id = nw.todos.id
       LEFT OUTER JOIN nw.users ON nw.activity_log.user_id = nw.users.id
      WHERE
       nw.activity_log.activity_timestamp < now()
       AND (nw.activity_log.id > "^string_of_int min_id^"
            AND nw.activity_log.id <= "^string_of_int max_id^")
       ORDER BY activity_timestamp DESC" in
  let r = guarded_exec ~conn sql in
  r#get_all_lst |>
    List.map
    (fun row ->
       let id = int_of_string (List.nth row 0) in
       let act_id = List.nth row 1 in
       let time = List.nth row 2 in
       let descr = List.nth row 3 in
       let user = List.nth row 4 in
       { a_id = id;
         a_activity = activity_type_of_int (int_of_string act_id);
         a_date = time;
         a_todo_descr = if descr = "" then None else Some descr;
         a_changed_by = if user = "" then None else Some user
       })
)

(* Search features *)
let search_wikipage str = with_conn (fun conn ->
  let escaped_ss = escape ~conn str in
  let sql =
    "SELECT page_id,headline,page_descr FROM nw.findwikipage('"^escaped_ss^"') "^
      "LEFT OUTER JOIN nw.pages on page_id = nw.pages.id ORDER BY rank DESC" in
  let r = guarded_exec ~conn sql in
  r#get_all_lst |>
    List.map
    (fun row ->
       let id = int_of_string (List.nth row 0) in
       let hl = List.nth row 1 in
       { sr_id = id;
         sr_headline = hl;
         sr_page_descr = Some (List.nth row 2);
         sr_result_type = SR_page })
)


let user_query_string =
  "SELECT id,login,passwd,real_name,email FROM nw.users"

let user_of_sql_row row =
  let id = int_of_string (List.nth row 0) in
  {
    user_id = id;
    user_login = (List.nth row 1);
    user_passwd = (List.nth row 2);
    user_real_name = (List.nth row 3);
    user_email = (List.nth row 4);
  }

let query_users () = with_conn (fun conn ->
  let sql = user_query_string ^ " ORDER BY id" in
  let r = guarded_exec ~conn sql in
  r#get_all_lst |> List.map user_of_sql_row
)


let query_user username = with_conn (fun conn ->
  let sql =
    user_query_string ^" WHERE login = '"^escape ~conn username^"' LIMIT 1" in
  let r = guarded_exec ~conn sql in
  if r#ntuples = 0 then
    None
  else
    Some (user_of_sql_row (r#get_tuple_lst 0))
)

let add_user ~conn ~login ~passwd ~real_name ~email =
  let sql =
    "INSERT INTO nw.users (login,passwd,real_name,email) "^
      "VALUES ("^(String.concat ","
                    (List.map (fun s -> "'"^escape ~conn s^"'")
                       [login; passwd; real_name; email]))^")" in
  ignore (guarded_exec ~conn sql)

let update_user ~conn~user_id ~passwd ~real_name ~email =
  let sql =
    "UPDATE nw.users SET "^
      (match passwd with
         None -> ""
       | Some passwd -> "passwd = '"^escape ~conn passwd^"',")^
      "real_name = '"^escape ~conn real_name^"',
          email = '"^escape ~conn email^"'
       WHERE id = "^(string_of_int user_id) in
  ignore (guarded_exec ~conn sql)


(* Highest upgrade schema below must match this version *)
let nurpawiki_schema_version = 3

