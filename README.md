
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggannotate

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/MattCowgill/ggannotate.svg?branch=master)](https://travis-ci.org/MattCowgill/ggannotate)
[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

ggannotate is a tool to help you put your annotations exactly where you
want them to go.

With ggannotate, you can interactively annotate ggplots using a
Shiny-based RStudio add-in.

## Installation

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
