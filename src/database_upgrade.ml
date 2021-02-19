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

module Psql = Postgresql
open Database


let insert_initial_wiki_pages =
"-- Insert main page and WikiMarkup pages into the DB
INSERT INTO nw.pages (page_descr) VALUES ('WikiStart');
INSERT INTO nw.wikitext (page_id,page_text)
       VALUES ((SELECT CURRVAL('nw.pages_id_seq')), '
= Nurpawiki =

See WikiMarkup for help on getting started.
');


INSERT INTO nw.pages (page_descr) VALUES ('WikiMarkup');
INSERT INTO nw.wikitext (page_id,page_text)
       VALUES ((SELECT CURRVAL('nw.pages_id_seq')), '
= Wiki Markup Page =

== Section heading ==

=== Sub-section heading ===

== Formatting ==
=== Italic ===
_Italic text_.  Single _italic_ word.  _Two_ _italic_ words.

=== Bold ===
*Bold faced text*. *Bold* word. *Bold* _with_ italic in same sentence.


=== Preformatted ===

Use `<pre></pre>` or `8<`:
8<
Preformatted text
WikiLink
[http://localhost/foo]
8<

== Bullet list ==

Paragraph of text.

Second paragraph of text.

* Bullet list top-level
* Another bullet on top-level
** Sub bullet
* Top-level again

== Links ==

A [wiki:WikiStart link] to the WikiStart main page.

Another [http://www.google.com link], this time to [http://www.google.com].

This !WikiText that does not become a link.  Write these as `!WikiText` to make Nurpawiki not regard it as a !WikiLink.  I.e., prefix it with a bang (!).
');"

let logged_exec ~conn logmsg sql =
  Buffer.add_string logmsg ("  "^sql^"\n");
  ignore (guarded_exec ~conn sql)

(* Migrate all tables to version 1 from schema v0: *)
let upgrade_schema_from_0 ~conn logmsg =
  Buffer.add_string logmsg "Upgrading schema to version 1\n";
  (* Create version table and set version to 1: *)
  let sql =
    "CREATE TABLE version (schema_version integer NOT NULL);
     INSERT INTO version (schema_version) VALUES('1')" in
  logged_exec ~conn logmsg sql;

  let empty_passwd = (Digest.to_hex (Digest.string "")) in
  let sql =
    "CREATE TABLE users (id SERIAL,
                         login text NOT NULL,
                         passwd varchar(64) NOT NULL,
                         real_name text,
                         email varchar(64));
     INSERT INTO users (login,passwd) VALUES('admin', '"^empty_passwd^"')" in
  logged_exec ~conn logmsg sql;

  (* Todos are now owned by user_id=0 *)
  let sql =
    "ALTER TABLE todos ADD COLUMN user_id integer" in
  logged_exec ~conn logmsg sql;

  (* Add user_id field to activity log table *)
  let sql =
    "ALTER TABLE activity_log ADD COLUMN user_id integer" in
  logged_exec ~conn logmsg sql


let table_exists ~conn ~schema ~table =
  let sql =
    "SELECT * from pg_tables WHERE schemaname = '"^schema^"'
     AND tablename = '"^table^"'" in
  let r = guarded_exec ~conn sql in
  r#ntuples <> 0

let function_exists ~conn fn =
  let sql =
    "SELECT proname from pg_proc WHERE proname = '"^fn^"'" in
  let r = guarded_exec ~conn sql in
  r#ntuples <> 0

let redefine_wikitext_search ~conn schema =
  let version =
    match function_exists ~conn "tsvector_update_trigger" with
      true -> `Built_in_tsearch2
    | false ->
        if function_exists ~conn "tsearch2" then
          `No_built_in_tsearch2
        else (* TODO no tsearch2 installed, ISSUE ERROR! *)
          assert false in
  let proc =
    match version with
      `No_built_in_tsearch2 ->
        "tsearch2('page_searchv', 'page_text')"
    | `Built_in_tsearch2 ->
        "tsvector_update_trigger(page_searchv, 'pg_catalog.english', page_text)" in
  "
-- Redefine wikitext tsearch2 update trigger to not trigger
-- on UPDATEs
DROP TRIGGER IF EXISTS wikitext_searchv_update ON "^schema^".wikitext;

CREATE TRIGGER wikitext_searchv_update
    BEFORE INSERT ON "^schema^".wikitext
    FOR EACH ROW
    EXECUTE PROCEDURE "^proc

let upgrade_schema_from_1 ~conn logmsg =
  Buffer.add_string logmsg "Upgrading schema to version 2\n";
  let sql =
    "ALTER TABLE pages ADD COLUMN head_revision bigint not null default 0" in
  logged_exec ~conn logmsg sql;

  let sql =
    "ALTER TABLE wikitext ADD COLUMN page_revision bigint not null default 0" in
  logged_exec ~conn logmsg sql;

  let sql =
    "ALTER TABLE wikitext
     ADD COLUMN page_created timestamp not null default now()" in
  logged_exec ~conn logmsg sql;

  let sql = "ALTER TABLE wikitext ADD COLUMN page_created_by_user_id bigint" in
  logged_exec ~conn logmsg sql;

  (* Change various tsearch2 default behaviour: *)
  if table_exists ~conn ~schema:"public" ~table:"pg_ts_cfg" then
    let sql = "UPDATE pg_ts_cfg SET locale = current_setting('lc_collate')
 WHERE ts_name = 'default'" in
    logged_exec ~conn logmsg sql
  else
    ();
  let sql = redefine_wikitext_search ~conn "public" in
  logged_exec ~conn logmsg sql;

  logged_exec ~conn logmsg "UPDATE version SET schema_version = 2"

let findwikipage_function_sql prfx =
  "CREATE FUNCTION nw.findwikipage(text) RETURNS SETOF nw.findwikipage_t
    AS $_$
SELECT page_id, "^prfx^"headline(page_text, q), "^prfx^"rank(page_searchv, q) FROM nw.wikitext, to_tsquery($1) AS q WHERE page_searchv @@ q ORDER BY "^prfx^"rank(page_searchv, q) DESC$_$
    LANGUAGE sql"

(* Version 2 -> 3: move all tables under 'nw' schema *)
let upgrade_schema_from_2 ~conn logmsg =
  let tables =
    ["users"; "todos"; "activity_in_pages"; "activity_log";
     "pages"; "version"; "wikitext"; "todos_in_pages"] in
  logged_exec ~conn logmsg "CREATE SCHEMA nw";
  List.iter
    (fun tbl ->
       let sql = "ALTER TABLE "^tbl^" SET SCHEMA nw" in
       logged_exec ~conn logmsg sql) tables;

  logged_exec ~conn logmsg (redefine_wikitext_search ~conn "nw");

  logged_exec ~conn logmsg "ALTER TYPE findwikipage_t SET SCHEMA nw";
  logged_exec ~conn logmsg "DROP FUNCTION findwikipage(text)";

  let create_find_fn_sql =
    if function_exists ~conn "ts_rank" then
      findwikipage_function_sql "ts_"
    else
      if function_exists ~conn "rank" then
        findwikipage_function_sql ""
      else
        assert false in
  logged_exec ~conn logmsg create_find_fn_sql;

  logged_exec ~conn logmsg insert_initial_wiki_pages;

  (* TODO seqs, findwikipage *)
  logged_exec ~conn logmsg "UPDATE nw.version SET schema_version = 3"

(* Version 3 -> 4: change user password handling *)
let upgrade_schema_from_3 ~conn logmsg =


  (* increase this size of the passwd column to be able to store longer hash *)
  logged_exec ~conn logmsg "ALTER TABLE nw.users ALTER COLUMN passwd TYPE character varying(256)";

  (* add prefix for the new password storage schema *)
  logged_exec ~conn logmsg "UPDATE nw.users SET passwd = CONCAT('$1$',passwd)";

  (* Update the schema_version number to 4 *)
  logged_exec ~conn logmsg "UPDATE nw.version SET schema_version = 4"

(* TODO clean up *)
let db_schema_version_raw conn =
  if table_exists ~conn ~schema:"nw" ~table:"version" then
    let r = guarded_exec ~conn "SELECT (nw.version.schema_version) FROM nw.version" in
    int_of_string (r#get_tuple 0).(0)
  else
    if not (table_exists ~conn ~schema:"public" ~table:"version") then
      0
    else
      let r = guarded_exec ~conn "SELECT (version.schema_version) FROM version" in
      int_of_string (r#get_tuple 0).(0)

let db_schema_version () = with_conn db_schema_version_raw

let upgrade_schema_raw conn =
  (* First find out schema version.. *)
  let logmsg = Buffer.create 0 in
  if db_schema_version_raw conn = 0 then
    begin
      Buffer.add_string logmsg "Schema is at version 0\n";
      upgrade_schema_from_0 ~conn logmsg
    end;
  if db_schema_version_raw conn = 1 then
    begin
      Buffer.add_string logmsg "Schema is at version 1\n";
      upgrade_schema_from_1 ~conn logmsg
    end;
  if db_schema_version_raw conn = 2 then
    begin
      Buffer.add_string logmsg "Schema is at version 2\n";
      upgrade_schema_from_2 ~conn logmsg
    end;
  if db_schema_version_raw conn = 3 then
    begin
      Buffer.add_string logmsg "Schema is at version 3\n";
      upgrade_schema_from_3 ~conn logmsg
    end;
  assert (db_schema_version_raw conn == nurpawiki_schema_version);
  Buffer.contents logmsg

let upgrade_schema () = with_conn upgrade_schema_raw

(** Check whether the nurpawiki schema is properly installed on Psql *)
let is_schema_installed () = with_conn (fun conn ->
  let sql =
    "SELECT * from pg_tables WHERE (schemaname = 'public' OR schemaname = 'nw') AND "^
      "tablename = 'todos'" in
  let r = guarded_exec ~conn sql in
  r#ntuples <> 0
)
