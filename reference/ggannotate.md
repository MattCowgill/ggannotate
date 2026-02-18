# Interactively annotate a ggplot2 plot

Launches an interactive Shiny gadget that lets you point and click on a
ggplot2 plot to add annotations, then generates the code to reproduce
them.

## Usage

``` r
ggannotate(plot = last_plot())
```

## Arguments

- plot:

  A ggplot2 object. Default is \`ggplot2::last_plot()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggannotate(p)
} # }
```
