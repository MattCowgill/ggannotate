# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ggannotate is an R package that provides a point-and-click Shiny gadget for adding annotations to ggplot2 plots. Users can interactively place text, labels, curves, or rectangles on a plot, and the package generates reproducible ggplot2 code.

### Design Principles

The goal of ggannotate is to enable users to more easily create code to generate annotations for their ggplot2 visualisations. This helps with the "final stage" of visualisations and removes the need to "touch up" charts in other software.

ggannotate should always "collect like geoms" where possible, combining (for example) multiple text annotations into a single geom_text() layer rather than multiple annotate() layers where possible.

ggannotate should try to produce concise code for the users to add to their scripts, but not at the expense of accuracy. The code should not be verbose - it should contain enough detail and arguments to reproduce the annotation created in the Shiny app, but no more.

## Common Commands

```bash
# Run all tests
Rscript -e "devtools::test()"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-make_layer.R')"

# Check package (CRAN-style)
Rscript -e "devtools::check()"

# Load package for interactive testing
Rscript -e "devtools::load_all()"

# Generate documentation
Rscript -e "devtools::document()"

# Update test snapshots (vdiffr visual tests)
Rscript -e "testthat::snapshot_accept()"
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

### Testing

Uses testthat with vdiffr for visual regression tests. Test files in `tests/testthat/` cover core functions. Visual snapshots are stored in `tests/testthat/_snaps/`.
