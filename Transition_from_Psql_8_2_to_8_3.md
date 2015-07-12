# Migrating Nurpawiki installation from Postgresql 8.2 to 8.3 or later #

Postgresql 8.3 has tsearch2 as a built-in feature which causes some gray hair when moving a Nurpawiki database over from 8.2 to a 8.3 installation.

This wiki page describes how to migrate from an older Postgresql over to 8.3 or later.

## BACKUP! ##
Take a backup of your DB before attempting to do anything.

## Upgrade Nurpawiki to latest version (1.2 at minimum) ##
See [upgrade instructions](Upgrading.md)

Upgrading Nurpawiki to its latest version will modify the existing Nurpawiki database schema so that the data is easier carried over to a newer Postgresql version.

You need to browse your Nurpawiki site with your browser to have it automatically upgrade the database schema to the latest version.

Before continuing, ensure that your Nurpawiki DB has upgraded properly and that is is working as expected.

## Export the database ##
Export the data out of your DB:

  * `pg_dump -U postgres -W -h localhost -n nw nurpawiki > db_dump.txt`

Note that only the contents of the DB schema `nw` are carried over to the new installation.

## Upgrade Postgresql ##
Upgrade your Postgresql installation.

## Massage the DB dump to make it work on Postgresql 8.3 ##
The export from older Postgresql 8.2 version does not work as-is on 8.3.  You need to manually edit it to make it run on 8.3.  Luckily this is pretty easy, so here's how to do it:

  * Find `CREATE TABLE wikitext` and change
    * `page_searchv public.tsvector` to
    * `page_searchv tsvector` (i.e., remove `public.`)
  * Find:
    * `CREATE INDEX wikitext_index ON wikitext USING gist (page_searchv public.gist_tsvector_ops);` and replace it by
    * `CREATE INDEX wikitext_index ON wikitext USING gist (page_searchv);`

## Restore Nurpawiki ##
  * `dropdb nurpawiki`
  * `createdb -E UTF-8 nurpawiki`
  * `psql nurpawiki < db_dump.txt`

## Launch Nurpawiki ##
Point your browser to the Nurpawiki main page.