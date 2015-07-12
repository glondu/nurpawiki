# Updating the GODI package #

## How to upgrade the GODI package ##
  * Change VERSION to the latest version in apps-nurpawiki/Makefile

## Testing the release candidate locally ##

Take the release candidate and distinfo from the previous steps of the ReleaseProcess.

Copy the packages over to your **test** GODI installation (have a separate installation for testing!):

```
cp nurpawiki-$VERSION.tar.gz $GODIBASE/build/distfiles
cp distinfo $GODIBASE/build/apps/apps-nurpawiki
cp Makefile $GODIBASE/build/apps/apps-nurpawiki
```

Test it:

```
cd $GODIBASE/build/apps/apps-nurpawiki
godi_delete apps-nurpawiki-$VERSION
godi_make clean
godi_make install
```

If you have a final release package uploaded to releases, you can test that it downloads successfully by deleting the `nurpawiki-$VERSION.tar.gz` package from distfiles and running `godi_make fetch`.

The package should be tested on both 3.09 and 3.10 release lines.

## Releasing the GODI package ##

  * Create a new `distinfo` file by running `utils/gen_godi_distinfo.sh` on your official release package (download from server to ensure you have the right one)
  * Copy your local package files (distinfo, Makefile, DESCR, etc.) to your godi-build apps-nurpawiki directory and submit to SVN.
  * Follow the standard GODI package release process