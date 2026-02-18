# Take a list of calls, return a string Each element of the list will be converted to a string, and then combined with the other elements

Take a list of calls, return a string Each element of the list will be
converted to a string, and then combined with the other elements

## Usage

``` r
calls_to_string(list_of_calls, sep = " +\n")
```

## Arguments

- list_of_calls:

  a list of calls

- sep:

  a character string to separate the items of \`list_of_calls\` when
  they are converted to a single string

## Value

A length-one character vector; each element of \`list_of_calls\` will be
converted to a string; each string will be combined, separated by
\`sep\`
