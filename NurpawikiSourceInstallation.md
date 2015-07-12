# Install Nurpawiki from source #
Note: this approach is harder than [other methods](NurpawikiGodiInstallation.md), but is provided as a reference for developers or in case something goes wrong with packaged Nurpawiki.

## Install Ocsigen & other required packages ##

Install Ocsigen through [the GODI package manager](http://godi.ocaml-programming.de/).

Follow [these instructions](OcsigenInstallation.md) to install Nurpawiki on Debian / Ubuntu.  If you encounter any problems, please file them as bugs into the issue tracker.

The installation procedure has been tested to work on Debian testing and Ubuntu 7.x or above.  For Windows, I find it convenient to run Linux in vmware and use Linux for serving pages and Windows for browsing.

## Get the source ##
Download the latest release from the [downloads page](http://code.google.com/p/nurpawiki/downloads/list).

You can also go for the latest and greatest by getting the source from SVN
(see http://code.google.com/p/nurpawiki/source).

## Configuration ##
Run `gen_ocsigen_config` like so:

```
# NOTE: This depends on the way you configured your Postgresql.
# NOTE 3: See DB installation page for info on postgres user password & authentication
env DBNAME=nurpawiki DBUSER=postgres DBPASSWD=<pass> ./gen_ocsigen_config > nurpawiki.conf.local
```

The script will complain if some of the dependent libraries are not installed via ocamlfind.

Output of `gen_ocsigen_config` is piped to a file and will later on be passed to Ocsigen when you start the Ocsigen web server.

See SiteConfiguration for more info on configuring Nurpawiki.
## Build Nurpawiki ##

Run `make`.


## Start server ##

Run

```
ocsigen -c nurpawiki.conf.local
```