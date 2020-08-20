Configuration
=============

This page details Nurpawiki's configuration options.  The
configuration file is passed to the Ocsigen when you start its web
server (`ocsigenserver -c <path_to_your_conf>`).

If you want to go beyond configuring the basic capabilities of
Nurpawiki, for example, if you want to configure the web server ports
or users, you need to refer to Ocsigen web server's configuration
manual.  Read [here](http://www.ocsigen.org/config) for more info.

The Nurpawiki-specific configuration options can be found from the
Ocsigen server configuration file under the XML element `<eliom
module=".../nurpawiki.cma">`.  Here's an example of a Nurpawiki site
configuration:

```
    <host>
      <site dir="" charset="UTF-8">
        <eliom module="nurpawiki.cma">
          <nurpawiki homepage="WikiStart" allow_read_only_guests="yes" />
          <database name="nurpawiki" user="postgres" password="default_passwd" />
        </eliom>

        <static dir="./files" />
      </site>
    </host>
```

Using an example configuration
------------------------------

If you installed Nurpawiki from an installation package (e.g., Debian
package), you may wish to review the contents of
`<path_to_config>/example.conf`.

At the bare minimum, you need to configure the default settings of
your database.

Generating a new configuration file
-----------------------------------

If you've installed Nurpawiki from source, you can generate your own
base configuration file by running `nurpawiki/gen_ocsigen_config`
script.  The created configuration should work well as a default
configuration.  You may want to edit it to better suit your needs.

Configuration options
---------------------

The configuration elements inside the Nurpawiki `<eliom>` element are:

### Element: nurpawiki

_Attribute list_:

 * homepage (optional): name of the wiki page to go to by default.
   If the user navigates to http://localhost:8080, he will be
   automatically redirected to http://localhost:8080/view?p=homepage .
   If this option is left unspecified, the default is to use
   WikiStart.
 * allow_read_only_guests (optional): if "yes", allow read-only access
   to the full wiki.  Guests can read all wiki pages, inspect todos,
   but are not allowed to modify any of the data. Default is to
   disallow read-only guests.

### Element: database

_Attribute list_:

 * name (required): name of the Nurpawiki database.  Example:
   "nurpawiki".
 * user (required): database user for accessing the Nurpawiki
   database.  Example: "postgres".
 * password (required): password used to let database _user_ access
   the database.
 * port (optional): database is running on this port.
 * host (optional): host where the database is running.  Defaults to
   localhost.
