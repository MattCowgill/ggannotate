
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggannotate <img src="man/figures/ggannotate_hex.png" align="right" height="138.5"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/mattcowgill/ggannotate/workflows/R-CMD-check/badge.svg)](https://github.com/mattcowgill/ggannotate/actions)
[![Codecov test
coverage](https://codecov.io/gh/mattcowgill/ggannotate/branch/master/graph/badge.svg)](https://codecov.io/gh/mattcowgill/ggannotate?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggannotate)](https://CRAN.R-project.org/package=ggannotate)

<!-- badges: end -->

{ggannotate} is a point-and-click tool to help you put your annotations
exactly where you want them to go on your {ggplot2} plots.

The last step in data visualisation - adding annotations - is one of the
most important, but it can be hard to get annotations in exactly the
right place. With {ggannotate}, you interactively annotate ggplots using
a Shiny-based RStudio add-in.

## Installation

{ggannotate} is not yet on CRAN. It will be submitted to CRAN when it is
more stable and feature-complete.

Install from GitHub:

``` r
remotes::install_github("mattcowgill/ggannotate")
```

## Usage

Create a ggplot2 object, then call `ggannotate()`:

``` r
library(ggplot2)
library(ggannotate)

p <- ggplot(mtcars, 
            aes(x = wt, y = mpg)) + 
  geom_point() 

ggannotate(p)
```

If you invoke `ggannotate()` without specifying a plot, it will use the
last plot you modified or created, using `ggplot2::last_plot()`:

``` r
ggplot(mtcars, 
            aes(x = wt, y = mpg)) + 
  geom_point() 

ggannotate()
```

You can also use the RStudio add-in to annotate your `last_plot()` with
`ggannotate()`.

Each of these options will launch a Shiny app. Add your annotation(s)
and click ‘copy code’ to copy the annotation code to the clipboard. Then
paste the copied code in your script and you’re done!

You can adjust the size of the plot in the Shiny app so you can set it
to be the same as your intended output.

## Lifecycle

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

{ggannotate} is still in active development, in an experimental phase.
Aspects of the functionality are quite likely to change as the package
evolves.

Suggestions are welcome (please file a GitHub issue). The UI will change
substantially.
