# Release notes #

## Nurpawiki 1.2.3 ([r471](https://code.google.com/p/nurpawiki/source/detail?r=471)) ##
  * Many good patches applied from various people.  Summary below.
  * Bunch of Ltwizations by Stéphane Glondu applied.
  * Bunch of build flow improvements from Stéphane Glondu
  * Allow specifying the database host in the nurpawiki database config.  Thanks Stéphane!
  * URL validation issues: [issue 63](https://code.google.com/p/nurpawiki/issues/detail?id=63), [issue 64](https://code.google.com/p/nurpawiki/issues/detail?id=64), [issue 65](https://code.google.com/p/nurpawiki/issues/detail?id=65) all fixed. Thanks Bjoern.Appel, vzapolskiy and animist for patches.

## Nurpawiki 1.2.2 ([r444](https://code.google.com/p/nurpawiki/source/detail?r=444)) ##
  * Automatic installation of the database schema on first use ([issue 62](https://code.google.com/p/nurpawiki/issues/detail?id=62)).  This simplifies installations as there are less manual steps to get Nurpawiki going.
  * More logging in for diagnosing potential database installation problems.
  * Don't show Logout and My Preferences links when a user is not logged in.
  * Modify to work on Ocsigen 1.1.0 and latest OCaml Calendar.  Thanks again Stéphane for help.
  * Debian package for Nurpawiki.  Thanks Stéphane!  See http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=500726 for more information.

## Nurpawiki 1.2.1 ([r416](https://code.google.com/p/nurpawiki/source/detail?r=416)) ##
**Warning** Requires Ocsigen 1.0.0 in order to work.  Just let GODI update the package.

**Warning** Re-create all your configuration files with `gen_ocsigen_config` script.

  * Modify to work on Ocsigen 1.0.0.  Thanks to Stephane Glondu for his changes!
  * Fixed [issue 54](https://code.google.com/p/nurpawiki/issues/detail?id=54) - if an unknown todo ID was specified in a wiki page, the server crashed and become unresponsive.

## Nurpawiki 1.2.0 ([r407](https://code.google.com/p/nurpawiki/source/detail?r=407)) ##
**Warning** Take a database backup before upgrading -- this release alters your DB schema. Once upgraded, you can't go back to the previous version.
**Caveats** Be sure to create "var/lib" directory in the server working directory when starting the server (see NurpawikiGodiInstallation), otherwise login might start failing.

  * Support for installing on Postgresql 8.3 ([issue 51](https://code.google.com/p/nurpawiki/issues/detail?id=51)).  Postgresql 8.3 has tsearch2 built-in and that didn't play well with our schema installation script.
  * Set session timeouts to infinity so that users don't get logged out after a while of inactivity ([issue 47](https://code.google.com/p/nurpawiki/issues/detail?id=47)).
  * Extend the schema to make migrating Nurpawiki installation from Postgresql 8.2 to Postgresql 8.3 easy (see [Transition\_from\_Psql\_8\_2\_to\_8\_3](Transition_from_Psql_8_2_to_8_3.md))

## Nurpawiki 1.1.2 ([r358](https://code.google.com/p/nurpawiki/source/detail?r=358)) ##
  * Split /history view into multiple pages to make viewing recent changes snappier.
  * Give the user ability to configure his preferred homepage (thanks Stephane Glondu!).
  * Performance optimizations in /history view.
  * Added a Selenium IDE based functional test suite.

## Nurpawiki 1.1.1 ([r330](https://code.google.com/p/nurpawiki/source/detail?r=330)) ##
  * Read-only access that lets in non-logged in users who are allowed to view the site content but not to modify it ([issue 33](https://code.google.com/p/nurpawiki/issues/detail?id=33))
  * Highlight a moved task in the to-do list if it's been moved up or down in priorities ([issue 8](https://code.google.com/p/nurpawiki/issues/detail?id=8))
  * Connection pooling (although restricted to only one concurreent connection)
  * Attempt to re-try database connections if the database server has been restarted while Nurpawiki has been running.
  * Less database queries during page loads with many to-dos
  * Login screen problems in [issue 40](https://code.google.com/p/nurpawiki/issues/detail?id=40) will be automatically fixed by Ocsigen 0.9.5 (release to be expected in Jan 2008).
  * A few smaller bugfixes & code improvements.

## Nurpawiki 1.1.0 ([r282](https://code.google.com/p/nurpawiki/source/detail?r=282)) ##
  * **Warning** Take a database backup before upgrading -- this release alters your DB schema.  Once upgraded, you can't go back to the previous version.
  * Reason for minor version upgrade: database schema changes.
  * Implement wiki page revision history:
    * Users can now view old versions of wiki pages
  * Several database schema improvements:
    * Set current DB locale on upgrade (enables users to migrate an existing Nurpawiki DB to a Postgresql installation on a different locale)
    * Start seq's from 1 (old schema.psql shouldn't have set their start values at all)
  * Link to nurpawiki main page after a database upgrade

## Nurpawiki 1.0.2 ([r239](https://code.google.com/p/nurpawiki/source/detail?r=239)) ##
  * Implement "undo complete task" that allows users to undo if they accidentally closed a task.
  * Check/uncheck button for (de)selecting all tasks for editing in scheduler.
  * Created a GODI package.  A few changes went into configuration scripts to enable this.
  * Rename `configure` to `gen_ocsigen_config`.
  * Rename `site.cma` to `nurpawiki.cma` (Requires you to update your Ocsigen config).
  * Strip down JSCalendar to make Nurpawiki release package smaller (113KB from 380KB).
  * Work arounds for IE compatibility

## Nurpawiki 1.0.1 ([r177](https://code.google.com/p/nurpawiki/source/detail?r=177)) ##
  * Automatically create WikiStart and WikiMarkup wiki pages on DB init so that the user is taken to a simple help screen after first install.
  * Implement bold, italic and code styles in wiki markup.
  * Fix a bug in bullet list whitespace handling.

## Nurpawiki 1.0.0 ##
  * First ever public release.