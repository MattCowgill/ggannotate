# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ggannotate is an R package that provides a point-and-click Shiny gadget for adding annotations to ggplot2 plots. Users can interactively place text, labels, curves, or rectangles on a plot, and the package generates reproducible ggplot2 code.

### Design Principles

The goal of ggannotate is to enable users to more easily create code to generate annotations for their ggplot2 visualisations. This helps with the "final stage" of visualisations and removes the need to "touch up" charts in other software.

ggannotate should always "collect like geoms" where possible, combining (for example) multiple text annotations into a single geom_text() layer rather than multiple annotate() layers where possible.

ggannotate should try to produce concise code for the users to add to their scripts, but not at the expense of accuracy. The code should not be verbose - it should contain enough detail and arguments to reproduce the annotation created in the Shiny app, but no more.

## Common Commands

```
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests
Rscript -e "devtools::test()"

# To run all tests for files starting with {name}
Rscript -e "devtools::test(filter = '^{name}')"

# To run all tests for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R')"

# To run a single test "blah" for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R', desc = 'blah')"

# To document the package
Rscript -e "devtools::document()"

# To check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# To format code
air format .
```

## Architecture

### Core Flow

1. **Entry Point** (`R/ggannotate.R`): The `ggannotate()` function launches a Shiny gadget. It builds the input plot, extracts metadata (flipped coords, date axes, facets), and sets up the server.

2. **UI** (`R/ggann_ui.R`, `R/geom_specific_ui.R`): Defines the Shiny interface with geom selection (text/label/curve/rect) and geom-specific parameter controls.

3. **Layer Generation** (`R/make_layer.R`): `make_layer()` creates unevaluated ggplot2 calls from aesthetics, parameters, and facet information. Returns a `call` object that can be `eval()`ed and added to plots.

4. **Layer Combination** (`R/combine_layers.R`): `combine_layers()` merges multiple annotations that share the same geom, parameters, and facets into single layers for cleaner output code.

5. **Code Output** (`R/call_to_string.R`): Converts call objects to formatted strings for clipboard copying.

### Key Helpers

- **`R/ggplot2_compat.R`**: Version-aware access to ggplot2 internals (handles both 3.x and 4.0.0+ S7 migration). All internal struct access uses `tryCatch` for robustness.
- **`R/facet_functions.R`**: Extracts and corrects facet information from plot clicks.
- **`R/date_functions.R`**: Handles date scale detection and conversion from Shiny's numeric click values.
- **`R/has_req_aes.R`**: Validates that annotations have all required aesthetics.
- **`R/remove_default_params.R`**: Strips default parameter values from generated code for cleaner output.

## Coding

* Always run `air format .` after generating code
* Use the base pipe operator (`|>`) not the magrittr pipe (`|>`)
* Don't use `_$x` or `_$[["x"]]` since dbplyr must work on R 4.1.
* Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}` 

### Testing

- Uses testthat with vdiffr for visual regression tests. Test files in `tests/testthat/` cover core functions. Visual snapshots are stored in `tests/testthat/_snaps/`.
- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`. 
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describes the change to the end user, and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. don't wrap the bullet).
- If the change is related to a function, put the name of the function early in the bullet.
- Order bullets alphabetically by function name, but only within an individual version number. Put all bullets that don't mention function names at the beginning.

### Writing

- Use sentence case for headings.
- Use Australian English where possible (UK English if unsure).

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing. 

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading. 

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.
