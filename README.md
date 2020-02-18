
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

The most convenient way to use `ggannotate` is as an RStudio add-in.
Write some code to generate a ggplot2 plot, highlight the code, then
select ‘Annotate ggplot object’ from the RStudio add-ins menu.

This will launch a Shiny app. Choose what type of annotation you want,
click to place it in the right place, and click ‘copy code’ to copy the
annotation code to the clipboard. Then paste the clipboard in your
script\!

You can adjust the size of the plot in the Shiny app so you can set it
to be the same as your intended output.
