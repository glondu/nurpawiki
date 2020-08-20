How to setup the Nurpawiki database with Postgresql
===================================================

The installation procedure here explains a setup in which you install
Postgresql from source into your home directory.

Installing Postgresql
---------------------

### Configure, build, install Postgresql

```
./configure --prefix=$HOME/opt/postgresql-12.4
make
make install
```

I've also made a symlink from the install dir to "psql" to simplify my command lines:

```
cd $HOME/opt
ln -s postgresql-12.4 psql
```

Create DB storage and our DB
----------------------------

Note the use of UTF-8!

```
# Create data storage
nurpamac:~/opt/postgresql-12.4 janne$ ./bin/initdb -E UTF-8 --locale=en_US.UTF-8 data
```

You can now start Postgresql (Postgresql prints this after successful
installation):

```
Success. You can now start the database server using:

    bin/postgres -D $HOME/opt/psql/data
or
    bin/pg_ctl -D $HOME/opt/psql/data -l logfile start
```

```
# Create DB:
nurpamac:~/opt/postgresql-12.4 janne$ ./bin/createdb -E UTF-8 nurpawiki
```

Install OCaml postgresql bindings
---------------------------------

OCaml's postgresql bindings will by default pick up the wrong
Postgresql installation (the one in /usr) which might cause
compilation to fail. To have them pick up the right PostgreSQL
installation, add `$HOME/opt/psql/bin` to your `PATH` environment
variable.
