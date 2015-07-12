# Nurpawiki installation through GODI package manager #

Nurpawiki is packaged as a GODI package and can be easily installed via GODI.

## Install Nurpawiki using GODI ##

First install the following Debian packages:
  * `apt-get install libpcre3-dev`
  * `apt-get install libgdbm-dev`
  * `apt-get install libsqlite3-dev`

Then download [GODI bootstrap package](http://godi.ocaml-programming.de/) and follow GODI's installation instructions.

Run `godi_console` and select `apps-nurpawiki` package and install it.  GODI will automatically install all required dependencies.

## Configuration ##
The package does not automatically configure everything to work and thus you need to do a bit of twiddling before you can run Nurpawiki.

After installation you should have the following two files in `godi/share/nurpawiki`:

  * `example.conf` is a configuration file for Nurpawiki/Ocsigen that needs to match your platform.

### example.conf ###
  * Take a copy of `example.conf` (say `nurpawiki.conf`) and modify your local copy and not the original.
  * Edit the `database` element's `password` attribute value to match your postgres user's database password.  Search for `insert_password_here`.

## Start Nurpawiki! ##

Provided everything installed the right way and you configured everything the right way, you can now start Nurpawiki:

  * `mkdir -p var/run var/lib var/log`
  * `ocsigen -v -c path/to/your/local/nurpawiki.conf`

### Trouble shooting ###
  * No database found?  Need to install database.  See DatabaseInstallationDebian.
  * Cannot authenticate?  See above.
  * Can't find `var/log` or `var/run` directories?  You need to have them in whichever directory you choose to start nurpawiki/ocsigen from.  Creating:
    * `mkdir -p var/lib`
    * `mkdir -p var/run`
    * `mkdir -p var/log`
    * Or you can edit your `nurpawiki.conf` to point these to an existing directory.
  * Get this: `Exn during page generation: Sqlite3.Error("error opening database: unable to open database file") (sending 500)`?  See previous bullet point on creating log and lib directories.