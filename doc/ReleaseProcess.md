Release Process
===============

 1. Review documentation to ensure that the docs are up-to-date
    for the version to be released.
 2. Update CHANGES.md
 3. Update VERSION to the version number being released
 4. Make a release candidate out of the current head
    * Untar the release candidate, configure and build it
 5. QA the package according to [DevTestPlan](DevTestPlan.md)
 6. Test the release candidate tarball with Debian
 7. Make a tag
 8. Upload the Debian Package
 9. **Eat your own dog food**: Upgrade personal installation to use
    the release version from Debian.  This is good for various
    reasons:
    * New Debian releases of Nurpawiki don't start to lag behind, as
      the latest features are installed to my own personal
      installation via releases.
    * Debian package gets tested on a real installation
    * Setting up Nurpawiki from Debian gets tested and hopefully will
      become streamlined over time as I get reminded about the
      installation procedure on each release.
 10. Announce
