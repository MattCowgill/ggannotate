# ggannotate 0.2.0

## Breaking changes
* Now requires R >= 4.1

## Improvements
* Full compatibility with ggplot2 4.0.0 (S7 migration)
* Added defensive wrappers for ggplot2 internal structure access
* Modernized CI/CD with r-lib/actions v2
* Added pkgdown documentation website
* Upgraded to testthat 3rd edition
* Updated all dependencies to modern versions

## Bug fixes
* Fixed potential issues with coord_flip detection on newer ggplot2 versions

# ggannotate 0.1.0.900
* Bug fixes

# ggannotate 0.1.0
* Multiple annotations now allowed

# ggannotate 0.0.0.9100

* Added a `NEWS.md` file to track changes to the package.
* ggannotate() code output now more compact; only non-default parameter values included
* Transformed facet variables (as in 'facet_wrap(~factor(cyl)))') recognised
