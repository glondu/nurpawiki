# Configuring Postgresql for Nurpawiki #
These installation instructions apply to both Debian and Ubuntu.  Package versions may change depending on your distribution.

## Install Postgresql and postgresql OCaml bindings ##
  * `apt-get install postgresql postgresql-client postgresql-contrib libpq-dev`
    * Make sure your distro has Postgresql 8.3 or newer, otherwise things might not go so smoothly with installation.

## Create DB & configure ##

### Install DB and configure ###

  * Become postgres user by `su root; su postgres`
    * `createdb -E UTF-8 nurpawiki`
      * Note: if Postgresql doesn't let you do this and prompts for a password, you need to know `postgres` user's Unix password.  If you can, you can just change it with `passwd postgres`.  Doing this should enable Postgresql to give you local administrative access using the `postgres` user.
  * If you're on **Postgresql 8.2 or earlier** do:
    * `psql nurpawiki < /usr/share/postgresql/8.2/contrib/tsearch2.sql` (file location may change from one distro to another)
    * This step is not required on Postgresql 8.3 or later as tsearch2 is now a built-in feature in Postgresql.

Note: we will load the `nurpawiki` schema later when we have its source code.

Do either (1) or (2), not both.

#### Method #1: Setup postgres user authentication to the database ####
This is the preferred method on making the DB connection authenticate property.  If you're having problems, you can try the (2) option as well, but that's not recommended.

  * Become postgres user by `su root; su postgres`
  * Set postgres DB password for the nurpawiki database
    * Connect to DB: `psql nurpawiki`
    * `ALTER USER postgres PASSWORD 'barfoo';`

You can now test that your newly set password actually lets you access you database:

  * _As a normal user_ do: `psql -h localhost -W -U postgres nurpawiki` and type in the password you used above.  Note that the password for `postgres` Unix user and the DB password are not the same.

When creating Nurpawiki's configuration with `gen_ocsigen_config`script, be sure to specify the same password you used in the above ALTER statement.

#### Method #2: Hack postgres configuration to allow DB access without a password ####

Edit `/etc/postgresql/8.2/main/pg_hba.conf` like so:

```
-host    all         all         127.0.0.1/32          md5
+host    all         all         127.0.0.1/32          trust
```