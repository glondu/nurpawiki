Configuring Postgresql for Nurpawiki
====================================

These installation instructions apply to both Debian and Ubuntu.
Package versions may change depending on your distribution.

Install Postgresql and postgresql OCaml bindings
------------------------------------------------

 * `apt-get install postgresql postgresql-client postgresql-contrib libpq-dev`
   * Make sure your distro has Postgresql 8.3 or newer, otherwise
     things might not go so smoothly with installation.

Create DB & configure
---------------------

### Install DB and configure

 * `sudo -u postgres createdb -E UTF-8 nurpawiki`

#### Method #1: Setup postgres user authentication to the database

This is the preferred method on making the DB connection authenticate properly.

 * Set postgres DB password for the nurpawiki database
   * Connect to DB: `sudo -u postgres psql nurpawiki`
   * `ALTER USER postgres PASSWORD 'barfoo';`

You can now test that your newly set password actually lets you access you database:

 * _As a normal user_ do: `psql -h localhost -W -U postgres nurpawiki`
   and type in the password you used above.  Note that the password
   for `postgres` Unix user and the DB password are not the same.

When creating Nurpawiki's configuration with `gen_ocsigen_config`
script, be sure to specify the same password you used in the above
ALTER statement.
