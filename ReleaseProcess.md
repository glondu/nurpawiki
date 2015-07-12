# Release Process #

  1. Review wiki documentation to ensure that the docs are up-to-date for the version to be released.  An easy way to do this is to read all wiki pages that are labeled with Docs-ReviewedFor-x.y where x.y is the prevous released major.minor version.  After approving a wiki page for the new version, label it with Docs-ReviewedFor-z.w where z.w is the upcoming release major.minor version.
  1. Update VERSION to the version number being released
  1. Make a release candidate out of the current head (record the revision number you use in the export!)
```
svn export -r ABC https://nurpawiki.googlecode.com/svn/trunk nurpawiki-x.y.z
tar cf nurpawiki-x.y.z.tar nurpawiki-x.y.z
gzip nurpawiki-x.y.z.tar
./utils/gen_godi_distinfo.sh
cd nurpawiki-x.y.z
```
    * Untar the release candidate, configure and build it
  1. QA the package according to [DevTestPlan](DevTestPlan.md)
  1. Test the release candidate tarball with GODI (see GodiPackageRelease)
  1. Snapshot current svn version
    * NOTE: Add main revision into submit message so that it's easier to diff against it
    * `svn cp -r ABC https://nurpawiki.googlecode.com/svn/trunk https://nurpawiki.googlecode.com/svn/tags/x.y.z`
  1. Update ReleaseNotes
  1. Upload release tarball to download page
  1. Finally release the GODI package (see GodiPackageRelease)
  1. **Eat your own dog food**: Upgrade personal installation to use the release version from GODI.  This is good for various reasons:
    * New GODI releases of Nurpawiki don't start to lag behind, as the latest features are installed to my own personal installation via releases.
    * GODI package gets tested on a real installation
    * Setting up Nurpawiki from GODI gets tested and hopefully will become streamlined over time as I get reminded about the installation procedure on each release.
  1. Announce