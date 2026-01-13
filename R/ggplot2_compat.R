# ggplot2 Compatibility Helpers
#
# These functions provide version-aware access to ggplot2 internals,
# ensuring compatibility with both ggplot2 3.x and 4.0.0+ (S7 migration).
# All access to internal structures is wrapped in tryCatch for robustness.

#' Get ggplot2 version
#' @return A package_version object
#' @noRd
ggplot2_version <- function() {

  utils::packageVersion("ggplot2")
}

#' Check if using ggplot2 4.0.0 or later
#' @return Logical
#' @noRd
is_ggplot2_v4 <- function() {
  ggplot2_version() >= "4.0.0"
}

#' Safely access layout$coord from a built plot
#'
#' Works with both S3-style ($) and S7-style (@) access.
#'
#' @param built_plot A ggplot_build object
#' @return The coord object, or NULL if not accessible
#' @noRd
get_layout_coord <- function(built_plot) {
  tryCatch(
    {
      # S3-style access (works in both, backwards compat in 4.0)
      built_plot$layout$coord
    },
    error = function(e) {
      # Fallback for potential future S7-only access
      tryCatch(
        built_plot@layout@coord,
        error = function(e2) NULL
      )
    }
  )
}

#' Check if plot uses polar coordinates
#'
#' @param built_plot A ggplot_build object
#' @return Logical indicating if CoordPolar is used
#' @noRd
is_polar_coord <- function(built_plot) {
  coord <- get_layout_coord(built_plot)
  inherits(coord, "CoordPolar")
}

#' Get whether coordinates are flipped
#'
#' Uses summarise_coord() with fallback to class checking.
#'
#' @param built_plot A ggplot_build object
#' @return Logical indicating if coordinates are flipped
#' @noRd
get_flipped_coords <- function(built_plot) {
  tryCatch(
    {
      ggplot2::summarise_coord(built_plot)$flip
    },
    error = function(e) {
      # Fallback: check coord class directly
      coord <- get_layout_coord(built_plot)
      inherits(coord, "CoordFlip")
    }
  )
}

#' Safely access panel scales from a built plot
#'
#' @param built_plot A ggplot_build object
#' @param axis Which axis: "x" or "y"
#' @return The first panel scale object, or NULL if not accessible
#' @noRd
get_panel_scale <- function(built_plot, axis = c("x", "y")) {
  axis <- match.arg(axis)
  tryCatch(
    {
      scales_list <- if (axis == "x") {
        built_plot$layout$panel_scales_x
      } else {
        built_plot$layout$panel_scales_y
      }
      scales_list[[1]]
    },
    error = function(e) NULL
  )
}

#' Safely access facet_params from a built plot
#'
#' @param built_plot A ggplot_build object
#' @return The facet_params list, or an empty list if not accessible
#' @noRd
get_facet_params <- function(built_plot) {
  tryCatch(
    {
      built_plot$layout$facet_params
    },
    error = function(e) list()
  )
}

#' Check if a scale is a date scale
#'
#' @param scale A ggplot2 scale object
#' @return Logical indicating if scale is a date scale
#' @noRd
is_date_scale <- function(scale) {
  if (is.null(scale)) {
    return(FALSE)
  }
  inherits(scale, "ScaleContinuousDate")
}

#' Safely get required aesthetics from a geom
#'
#' @param geom A ggplot2 geom object (evaluated layer)
#' @return Character vector of required aesthetics, or empty character vector
#' @noRd
get_required_aes <- function(geom) {
  tryCatch(
    {
      geom$geom$required_aes
    },
    error = function(e) character(0)
  )
}

#' Safely get geom parameters and default aesthetics
#'
#' @param geom_as_string Character string like "geom_text"
#' @return Named list of default parameter values
#' @noRd
get_geom_defaults <- function(geom_as_string) {
  tryCatch(
    {
      geom <- call(geom_as_string)
      geom <- eval(geom)
      c(geom$geom_params, geom$geom$default_aes)
    },
    error = function(e) {
      list()
    }
  )
}
