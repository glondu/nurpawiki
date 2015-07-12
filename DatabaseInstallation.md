# How to setup the Nurpawiki database with Postgresql #

The installation procedure here explains a setup in which you install Postgresql from source into your home directory.  I'm successfully using this setup.

A system-wide installation of Postgresql should be doable, however I'm not sure how make tsearch2 work well with pre-built packages.  If you know, please let me know how.

## Installing Postgresql ##

### Configure, build, install Postgresql ###
```
./configure --prefix=$HOME/opt/postgresql-8.2.3
make
make install
```

I've also made a symlink from the install dir to "psql" to simplify my command lines:

```
cd $HOME/opt
ln -s postgresql-8.2.3 psql
```

### Enable tsearch2 ###

```
cd contrib/tsearch2
make
make install
```

## Create DB storage and our DB ##

Note the use of UTF-8!

```
# Create data storage
nurpamac:~/opt/postgresql-8.2.3 janne$ ./bin/initdb -E UTF-8 --locale=en_US.UTF-8 data
```

You can now start Postgresql (Postgresql prints this after successful installation):

```
Success. You can now start the database server using:

    bin/postgres -D $HOME/opt/psql/data
or
    bin/pg_ctl -D $HOME/opt/psql/data -l logfile start
```

```
# Create DB:
nurpamac:~/opt/postgresql-8.2.3 janne$ ./bin/createdb -E UTF-8 nurpawiki
```


## Setup Nurpawiki schema ##
Run Postgresql schema setup script:
```
$HOME/opt/psql/bin/psql nurpawiki < schema.psql
```

## Install OCaml postgresql bindings ##
OCaml's postgresql bindings will by default pick up the wrong Postgresql installation (the one in /usr/local) which might cause compilation to fail.

To have GODI pick up the right Psql installation, run it like this:

```
env PATH=$HOME/opt/psql/bin:$PATH godi_console 
```

In addition, you also need to configure conf-postgresql in GODI to point to the right Postgresql installation path (include & lib directories).