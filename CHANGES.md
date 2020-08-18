dev
===

 * Use proper connection-dependent PostgreSQL escaping
 * Port to modern Ocsigen stack (mainly Eliom 6.12.1 and lwt_ppx)
 * Fix schema installation failure with PostgreSQL 12

1.2.3 (2009-09-07)
==================

 * Bugfix: inexistent todo id caused a server crash on save
 * Port to Ocsigen 1.1 and Calendar 2.0.4
 * Add message to error log if with_conn raises an exception
 * Make calendar string input field readonly. Now it can only be
   edited with the JS calendar picker.
 * Install DB schema automatically without requiring a manual
   installation step using psql commands
 * Many improvements to the build system
   - switch to ocamlbuild
   - make a pack of all Nurpawiki modules, to prevent global module
     namespace pollution
   - build native archive/plugin if possible
 * Properly use Lwt in page generation code
 * Add host parameter to db configuration
 * Minor bug fixes in external link handling
