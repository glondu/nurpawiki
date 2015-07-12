# How to upgrade to a newer release #
**NOTE: always take a database backup before upgrading!**

If you have compiled Nurpawiki from source, you need to get the new source and compile it.

If you're using a packaged Nurpawiki, just install the latest package.  The GODI package may fail to upgrade if you have edited any of the files installed by GODI.  This applies particularly to `godi/share/nurpawiki/example.conf`.  If you've modified it, GODI may fail the package installation.  In such a case, take a backup of your modified file and delete the original and try re-installing the package.

Configuration files may require changes from time to time.  See e.g., NurpawikiGodiInstallation and SiteConfiguration for notes.

The nurpawiki database will be automatically upgraded when you try to access any of your Nurpawiki pages with your browser.  You're strongly recommended to backup your database before attempting an upgrade.

Note that it is **impossible to go back to an earlier version** of your database once you have upgraded to a later version.