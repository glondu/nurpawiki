# How to test a new release #

Each version, be it major, minor or patch needs to pass these tests:

  * Check that VERSION matches About box's version (should be automatic)
  * Go through the sample wiki page to check that Wiki markup is parsing hasn't regressed
  * SeleniumFunctionalTests

## Patch revision releases ##
Patch revision releases should never add or modify the DB schema, so they're slightly easier to QA.

## Minor revision releases ##
  * Try installation on an empty database (dropdb, createdb)
    * This is already tested by Selenium tests
  * Try installation on an existing database (take personal db and install on it)
  * Benchmarks

## Major revision releases ##
  * TODO DB upgrades