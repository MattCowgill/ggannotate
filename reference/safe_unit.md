# Functions to safely work with grid::unit() and grid::arrow() in Shiny If an input is NULL, NULL is returned; otherwise return a call to unit/arrow.

This is needed because \`unit(NULL, "inches")\` gives an error, whereas
we want \`NULL\`.

## Usage

``` r
safe_unit(x, units)

safe_arrow(angle, length, ends = "last", type = "closed")
```

## Arguments

- x:

  Size of \`grid::unit()\` object

- units:

  Units of \`grid::unit()\` object

- angle:

  Angle of arrow created by \`grid::arrow()\`

- length:

  Length of arrow created by \`grid::arrow()\`

- ends:

  See \`?grid::arrow()\`

- type:

  See \`?grid::arrow()\`
