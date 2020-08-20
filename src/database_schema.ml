(* Copyright (c) 2008 Janne Hellsten <jjhellst@gmail.com> *)

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

let install_schema () = with_conn (fun conn ->
  let sql = "
--
-- PostgreSQL database dump
--

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path TO public, pg_catalog;

--
-- Name: findwikipage_t; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE findwikipage_t AS (
    page_id bigint,
    headline text,
    rank real
);


CREATE FUNCTION findwikipage(text) RETURNS SETOF findwikipage_t
    AS $_$
SELECT page_id, ts_headline(page_text, q), ts_rank(page_searchv, q) FROM wikitext, to_tsquery($1) AS q WHERE page_searchv @@ q ORDER BY ts_rank(page_searchv, q) DESC$_$
    LANGUAGE sql;



SET default_tablespace = '';

CREATE TABLE activity_in_pages (
    activity_log_id bigint NOT NULL,
    page_id bigint NOT NULL
);


CREATE TABLE activity_log (
    id integer NOT NULL,
    activity_timestamp timestamp without time zone DEFAULT now(),
    activity_id bigint NOT NULL,
    todo_id bigint
);


CREATE SEQUENCE activity_log_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER SEQUENCE activity_log_id_seq OWNED BY activity_log.id;

CREATE TABLE pages (
    id integer NOT NULL,
    page_descr character varying(256)
);

CREATE SEQUENCE pages_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER SEQUENCE pages_id_seq OWNED BY pages.id;

CREATE TABLE todos (
    id integer NOT NULL,
    completed boolean DEFAULT false,
    created timestamp without time zone DEFAULT now(),
    priority integer DEFAULT 3,
    descr text,
    activation_date date DEFAULT now(),
    CONSTRAINT todos_priority CHECK ((((priority = 1) OR (priority = 2)) OR (priority = 3)))
);

CREATE SEQUENCE todos_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER SEQUENCE todos_id_seq OWNED BY todos.id;


CREATE TABLE todos_in_pages (
    todo_id bigint NOT NULL,
    page_id bigint NOT NULL
);

CREATE TABLE wikitext (
    page_id bigint,
    page_text text,
    page_searchv tsvector
);

ALTER TABLE activity_log ALTER COLUMN id SET DEFAULT nextval('activity_log_id_seq'::regclass);

ALTER TABLE pages ALTER COLUMN id SET DEFAULT nextval('pages_id_seq'::regclass);

ALTER TABLE todos ALTER COLUMN id SET DEFAULT nextval('todos_id_seq'::regclass);

ALTER TABLE ONLY activity_log
    ADD CONSTRAINT activity_log_pkey PRIMARY KEY (id);

ALTER TABLE ONLY pages
    ADD CONSTRAINT pages_pkey PRIMARY KEY (id);

ALTER TABLE ONLY todos
    ADD CONSTRAINT todos_pkey PRIMARY KEY (id);

CREATE INDEX wikitext_index ON wikitext USING gist (page_searchv);

ALTER TABLE ONLY todos_in_pages
    ADD CONSTRAINT \"$1\" FOREIGN KEY (todo_id) REFERENCES todos(id);

ALTER TABLE ONLY activity_in_pages
    ADD CONSTRAINT \"$1\" FOREIGN KEY (activity_log_id) REFERENCES activity_log(id);

ALTER TABLE ONLY activity_log
    ADD CONSTRAINT \"$2\" FOREIGN KEY (todo_id) REFERENCES todos(id);

ALTER TABLE ONLY todos_in_pages
    ADD CONSTRAINT \"$2\" FOREIGN KEY (page_id) REFERENCES pages(id);

ALTER TABLE ONLY activity_in_pages
    ADD CONSTRAINT \"$2\" FOREIGN KEY (page_id) REFERENCES pages(id);

ALTER TABLE ONLY wikitext
    ADD CONSTRAINT wikitext_page_id_fkey FOREIGN KEY (page_id) REFERENCES pages(id);

" in
  ignore (guarded_exec ~conn sql);
  ignore (Database_upgrade.upgrade_schema_raw conn)
)
