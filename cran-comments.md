## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local macOS (aarch64-apple-darwin), R 4.x
* GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macos-latest): R-release

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes on package functionality

This package provides an interactive Shiny gadget for annotating ggplot2 plots.
The main function `ggannotate()` only works in interactive sessions, hence
examples are wrapped in `\dontrun{}`.

The package accesses some internal ggplot2 structures (e.g., `layout$panel_scales_x`)
to determine axis types and coordinate systems. These structures have been stable
since ggplot2 3.0.0, and defensive coding with tryCatch wrappers is in place to
handle any future changes gracefully. The package is tested against both ggplot2
3.x and 4.0.0 (which uses S7 objects).
