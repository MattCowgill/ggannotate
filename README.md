
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggannotate <img src='man/figures/ggannotate_hex.png' align='right' height='138.5'/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/mattcowgill/ggannotate/workflows/R-CMD-check/badge.svg)](https://github.com/mattcowgill/ggannotate/actions)
[![Travis build
status](https://travis-ci.com/mattcowgill/ggannotate.svg?branch=master)](https://travis-ci.com/mattcowgill/ggannotate)
[![Codecov test
coverage](https://codecov.io/gh/mattcowgill/ggannotate/branch/master/graph/badge.svg)](https://codecov.io/gh/mattcowgill/ggannotate?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggannotate)](https://CRAN.R-project.org/package=ggannotate)
<!-- badges: end -->

{ggannotate}is a point-and-click tool to help you put your annotations
exactly where you want them to go on your {ggplot2} plots.

The last step in data visualisation - adding annotations - is one of the
most important, but it can be hard to get annotations in exactly the
right place. With {ggannotate}, you interactively annotate ggplots using
a Shiny-based RStudio add-in.

## Installation

{ggannotate} is not yet on CRAN.

Install from GitHub:

``` r
remotes::install_github("mattcowgill/ggannotate")
```

## Usage

There are three ways to use `ggannotate`:

1.  Select some ggplot2 code in RStudio, then invoke the `ggannotate`
    addin via RStudio’s Addins menu.
    ![](https://github.com/MattCowgill/ggannotate/blob/dev/man/figures/ggannotate_example.gif?raw=true)

2.  Create a ggplot2 object, then call `ggannotate()`, like this:

<!-- end list -->

``` r
library(ggplot2)
p <- ggplot(mtcars, 
            aes(x = wt, y = mpg)) + 
  geom_point() 

ggannotate::ggannotate(p)
```

3.  Wrap some code that creates a ggplot2 object in `ggannotate()`, like
    this:

<!-- end list -->

``` r
ggannotate::ggannotate(ggplot(mtcars, 
                              aes(x = wt, y = mpg)) + 
                         geom_point())
```

Each of these options will launch a Shiny app. Choose what type of
annotation you want, click to place it in the right place, and click
‘copy code’ to copy the annotation code to the clipboard. Then paste
the copied code in your script and you’re done\!

You can adjust the size of the plot in the Shiny app so you can set it
to be the same as your intended output. You can keep repeat the process
to add multiple annotations (this will get easier in later versions.)

## Lifecycle

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

{ggannotate} is still in active development, in an experimental phase.
Aspects of the functionality are quite likely to change as the package
evolves.

Suggestions are welcome (please file a GitHub issue). The UI will change
substantially. The main feature to be added is the ability to add more
than one annotation within the app – at the moment you have to do
annotations one-at-a-time.
