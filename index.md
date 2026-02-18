# ggannotate ![](reference/figures/ggannotate_hex.png)

{ggannotate} makes it easy to add annotations to your {ggplot2} plots by
pointing and clicking.

Annotations are often the most important part of a data visualisation,
but getting them in exactly the right place can be fiddly.

{ggannotate} launches an interactive Shiny gadget that lets you **click
on your plot to place annotations exactly where you want them.** It then
generates the code to reproduce your annotations

![](reference/figures/demo.gif)

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

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("MattCowgill/ggannotate")
```

## Usage

Firs, create a ggplot2 object:

``` r
library(ggplot2)
library(ggannotate)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
```

Then just call
[`ggannotate()`](https://mattcowgill.github.io/ggannotate/reference/ggannotate.md)
or use the handy RStudio addin to invoke ggannotate.

It also works with named plots:

``` r
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggannotate(p)
```

This launches a Shiny app where you can add your annotations
interactively. When you’re done, click “Copy code” to copy the
annotation code to your clipboard, then paste it into your script.
