
#' @importFrom ggtext geom_richtext GeomRichtext GeomRichText

richlegend_text <- function(plot, aes_type = "fill") {

  p <- ggplot2::ggplot_build(plot)

  scales <- p$plot$scales$scales

  matches_aes_type <- function(scale) {
    aes <- scale[["aesthetics"]]
    identical(aes, aes_type)
  }

  scales_match_type <- purrr::map_lgl(.x = scales, matches_aes_type)

  matching_scale <- scales[which(scales_match_type)]

  if(length(matching_scale) > 1) {
    stop("More than one scale matches ", aes_type)
  }

  if(length(matching_scale) < 1) {
    stop("No scale matches ", aes_type)
  }

  pal <- matching_scale[[1]][["palette.cache"]]

  vars <- matching_scale[[1]][["range"]][["range"]]

  text_legend <- paste0("<span style='color:",
                        pal,
                        "'>",
                        vars,
                        "</span>",
                        collapse = "<br>")

  text_legend

}


richlegend_call <- function(plot,
                            x, y,
                            aes_type = "fill",
                            label.padding = unit(0, "lines"),
                            label.size = 0,
                            ...) {

  text_legend <- richlegend_text(plot, aes_type = aes_type)

  make_layer(geom = "richtext",
             x = x,
             y = y,
             label = text_legend,
             params = c(list(label.padding = label.padding,
                           label.size = label.size),
                        ...)
             )

}


richlegend <- function(plot,
                           aes_type = "fill",
                           x, y,
                           label.padding = unit(0, "lines"),
                           label.size = 0,
                           ...) {


  legend_call <- richlegend_call(plot = plot,
                                 aes_type = aes_type,
                                 x = x, y = y,
                                 label.padding = unit(0, "lines"),
                                 label.size = 0,
                                 ...)

  eval(legend_call)

}




