# Installation #

Nurpawiki can be installed in various ways.  By far the simplest method is to use the Nurpawiki Debian package.

## 1. Installation via the Nurpawiki Debian package (recommended) ##

Debian installation: [installing Nurpawiki on Debian](NurpawikiDebianInstallation.md).

If you successfully installed using the above link, you can stop reading now.

## 2. Installation via source / GODI (not recommended) ##

### Install Postgresql & create a database ###
See [Debian DB instructions](DatabaseInstallationDebian.md) for instructions on installing using Debian's packages.  This should work for both Debian and Ubuntu.  The instructions should be easily adaptable for other distributions.

See [DB from source](DatabaseInstallation.md) for installing everything from source (not recommended).

### Install Nurpawiki ###

  * NurpawikiGodiInstallation (recommended) or
  * NurpawikiSourceInstallation

See [here](SiteConfiguration.md) for more configuration options.

### Test it! ###
Point your browser to http://localhost:8080/view?p=WikiStart and login as "admin" (leave the password field empty).