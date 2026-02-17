
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggannotate <img src="man/figures/ggannotate_hex.png" align="right" height="138.5"/>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build
status](https://github.com/mattcowgill/ggannotate/workflows/R-CMD-check/badge.svg)](https://github.com/mattcowgill/ggannotate/actions)
[![Codecov test
coverage](https://codecov.io/gh/mattcowgill/ggannotate/branch/master/graph/badge.svg)](https://codecov.io/gh/mattcowgill/ggannotate?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggannotate)](https://CRAN.R-project.org/package=ggannotate)

<!-- badges: end -->

{ggannotate} makes it easy to add annotations to your {ggplot2} plots by
pointing and clicking.

Annotations are often the most important part of a data visualisation,
but getting them in exactly the right place can be fiddly. {ggannotate}
launches an interactive Shiny gadget that lets you click on your plot to
place annotations, then generates the code to reproduce them.

<figure>
<img src="man/figures/demo.gif" alt="Demo of ggannotate" />
<figcaption aria-hidden="true">Demo of ggannotate</figcaption>
</figure>

## Features

- Add **text**, **labels**, **curves**, and **rectangles** to any
  ggplot2 plot
- Place multiple annotations in a single session
- Full support for faceted plots
- Annotations using the same geom are combined into a single layer for
  cleaner code
- Copy generated code to the clipboard with one click
- Use as an RStudio addin or call directly from the console

## Installation

Install from CRAN:

``` r
install.packages("ggannotate")
```

Or install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("MattCowgill/ggannotate")
```

## Usage

Create a ggplot2 object and pass it to `ggannotate()`:

``` r
library(ggplot2)
library(ggannotate)

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggannotate(p)
```

Or call `ggannotate()` with no arguments to annotate your most recent
plot:

``` r
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggannotate()
```

This launches a Shiny app where you can add your annotations
interactively. When you’re done, click “Copy code” to copy the
annotation code to your clipboard, then paste it into your script.
