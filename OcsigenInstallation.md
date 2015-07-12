# How to install everything required by Ocsigen on Debian #

## Database installation ##

Read [DatabaseInstallationDebian](DatabaseInstallationDebian.md) and follow instructions.  It's better to get Postgresql installed prior to attempting GODI installation.

## Installing GODI ##

  * `apt-get install libpcre3-dev`
  * `apt-get install libgdbm-dev`
  * `apt-get install libsqlite3-dev`
  * `apt-get install libpq-dev`
  * Download and install GODI as per its installation manual (http://godi.ocaml-programming.de/)

## Install Ocsigen through GODI ##

  * Install `apps-ocsigen` from `godi_console`.  If you're lucky, all the dependent packages will be automatically selected and installed.  If you're not lucky, you'll need to dance package installation rumba for a little while.

## Install packages required by Nurpawiki ##

  * Install `godi-extlib`
  * Install `godi-postgresql`
  * Install `godi-calendar`