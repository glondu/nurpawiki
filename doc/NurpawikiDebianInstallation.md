Nurpawiki installation on Debian
================================

This page describes how to install Nurpawiki using the [Nurpawiki
Debian package](http://packages.debian.org/nurpawiki).

## 1. Package installation

Nurpawiki is currently only in Debian unstable, so it's likely that
you need to include unstable packages in your `/etc/apt/sources.list`.

Install required Debian packages:

```
apt-get install postgresql
apt-get install nurpawiki
```

## 2. Database setup

With Bullseye's PostgreSQL (version 12.4), the steps on the SQL server are:

 1. Create a user in the database:
```
sudo -u postgres createuser ${DBUSER}
```
 2. Create the database for Nurpawiki:
```
sudo -u postgres createdb -O ${DBUSER} -E UTF-8 ${DBNAME}
```
 3. Set a password for the user accessing the database:
```
sudo -u postgres psql ${DBNAME} ${DBUSER}
ALTER ${DBUSER} PASSWORD '${DBPASSWORD}';
\q
```

You can take, for example, DBUSER=ocsigen and DBNAME=nurpawiki (beware of commands executed in psql shell).

## 3. Ocsigen configuration

A sample template for a configuration file is available in
`/usr/share/doc/nurpawiki/examples`. Filling it with proper database
user, database name and password will give you a `<file>` that can be
run with `ocsigen -c <file>` (as root). Ocsigen will then be listening
on port 8080, as user ocsigen, and be serving Nurpawiki only (at
/). If satisfied, and if you don't use any other Ocsigen-based
service, you can directly use that configuration file as
/etc/ocsigen/ocsigenserver.conf and use an initscript or systemd unit
to launch ocsigen. Of course, Nurpawiki can be used with other
Ocsigen-based services, but you'll have to edit
/etc/ocsigen/ocsigenserver.conf yourself.

At installation, a wiki user "admin" with an empty password is
created.

If everything went right and Ocsigen was properly started, you should
be able to get into Nurpawiki by browsing to http://localhost:8080

## More details

More details are available in
`/usr/share/doc/nurpawiki/README.Debian`.
