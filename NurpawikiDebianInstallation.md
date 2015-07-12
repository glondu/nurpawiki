# Nurpawiki installation on Debian #

This page describes how to install Nurpawiki using the [Nurpawiki Debian package](http://packages.debian.org/experimental/nurpawiki).


## 1. Package installation ##
Nurpawiki is currently only in Debian experimental, so it's likely that you need to include experimental packages in your `/etc/apt/sources.list`.

Install required Debian packages:

```
apt-get install postgresql
apt-get install nurpawiki
```

## 2. Database setup ##
With Lenny's PostgreSQL (version 8.3), the steps on the SQL server are (as user postgres):

  1. Switch to user postgres
> > `su postgres`
  1. Create a user in the database:
> > `createuser ${DBUSER`}
  1. Create the database for Nurpawiki:
> > `createdb -O ${DBUSER} -E UTF-8 ${DBNAME`}
  1. Set a password for the user accessing the database:
```
      psql ${DBNAME} ${DBUSER}
      ALTER ${DBUSER} PASSWORD '${DBPASSWORD}';
      \q
```

You can take, for example, DBUSER=ocsigen and DBNAME=nurpawiki (beware of commands executed in psql shell).

## 3. Ocsigen configuration ##
A sample template for a configuration file is available in `/usr/share/doc/nurpawiki/examples`. Filling it with proper database user, database name and password will give you a `<file>` that can be run with `ocsigen -c <file>` (as root). Ocsigen will then be listening on port 8080, as user ocsigen, and be serving Nurpawiki only (at /). If satisfied, and if you don't use any other Ocsigen-based service, you can directly use that configuration file as /etc/ocsigen/ocsigen.conf and use the initscript to launch ocsigen. Of course, Nurpawiki can be used with other Ocsigen-based services, but you'll have to edit /etc/ocsigen.conf yourself.

At installation, a wiki user "admin" with an empty password is created.

If everything went right and Ocsigen was properly started, you should be able to get into Nurpawiki by browsing to http://localhost:8080

## More details ##
More details available in `/usr/share/doc/nurpawiki/README.Debian`.