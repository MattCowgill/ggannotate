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

## Value

Called for its side effects. When the gadget is closed via the "Copy
code" or "Done" button, the generated annotation code is copied to the
clipboard. Returns \`NULL\` invisibly.

## Examples

``` r
if (interactive()) {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  ggannotate(p)
}
```
