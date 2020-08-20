Install Nurpawiki from source
=============================

Note: this approach is harder than other methods, but is provided as a
reference for developers or in case something goes wrong with packaged
Nurpawiki.

Install Ocsigen & other required packages
-----------------------------------------

Install Ocsigen server and Eliom through your favourite distribution
or [OPAM](https://opam.ocaml.org).

For Windows, I find it convenient to run Linux in vmware and use Linux
for serving pages and Windows for browsing.

Get the source
--------------

Download the latest release from the [downloads
page](https://github.com/glondu/nurpawiki/releases).

You can also go for the latest and greatest by getting the source from
[GitHub](https://github.com/glondu/nurpawiki).

Configuration
-------------

Run `gen_ocsigen_config` like so:

```
# NOTE: This depends on the way you configured your Postgresql.
# NOTE 3: See DB installation page for info on postgres user password & authentication
env DBNAME=nurpawiki DBUSER=postgres DBPASSWD=<pass> ./gen_ocsigen_config > nurpawiki.conf.local
```

The script will complain if some of the dependent libraries are not
installed via ocamlfind.

Output of `gen_ocsigen_config` is piped to a file and will later on be
passed to Ocsigen server when you start it.

See [SiteConfiguration](SiteConfiguration.md) for more info on configuring Nurpawiki.

Build Nurpawiki
---------------

Run `make`.

Start server
------------

Run `ocsigenserver -c nurpawiki.conf.local`.
