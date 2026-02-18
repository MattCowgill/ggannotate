# Changelog

## ggannotate 1.0.0

- First CRAN release.

## ggannotate 0.3.0

- Annotations across different facet panels with the same geom and
  parameters are now combined into a single geom call, producing more
  concise code.
- Annotations are no longer lost when switching between layers in the
  Shiny gadget
  ([\#11](https://github.com/MattCowgill/ggannotate/issues/11)).
- [`ggannotate()`](https://mattcowgill.github.io/ggannotate/reference/ggannotate.md)
  now includes a “Delete” button to remove the current annotation layer
  ([\#11](https://github.com/MattCowgill/ggannotate/issues/11)).
- [`ggannotate()`](https://mattcowgill.github.io/ggannotate/reference/ggannotate.md)
  now intelligently rounds x and y coordinate values in generated
  annotation code based on the axis range, producing cleaner output
  ([\#44](https://github.com/MattCowgill/ggannotate/issues/44)).
- [`ggannotate()`](https://mattcowgill.github.io/ggannotate/reference/ggannotate.md)
  no longer briefly errors when switching to a new layer with the rect
  geom before completing a brush selection.
- The annotation layer dropdown now shows the geom type for each layer,
  e.g. “1 ”
  ([\#11](https://github.com/MattCowgill/ggannotate/issues/11)).

## ggannotate 0.2.0

### Breaking changes

- Now requires R \>= 4.1

### Improvements

- Full compatibility with ggplot2 4.0.0 (S7 migration)
- Added defensive wrappers for ggplot2 internal structure access
- Modernized CI/CD with r-lib/actions v2
- Added pkgdown documentation website
- Upgraded to testthat 3rd edition
- Updated all dependencies to modern versions

### Bug fixes

- Fixed potential issues with coord_flip detection on newer ggplot2
  versions

## ggannotate 0.1.0.900

- Bug fixes

## ggannotate 0.1.0

- Multiple annotations now allowed

## ggannotate 0.0.0.9100

- Added a `NEWS.md` file to track changes to the package.
- ggannotate() code output now more compact; only non-default parameter
  values included
- Transformed facet variables (as in ‘facet_wrap(~factor(cyl)))’)
  recognised
