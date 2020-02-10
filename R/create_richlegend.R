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


add_richlegend <- function(plot,
                           aes_type = "fill",
                           x, y,
                           label.padding = unit(0, "lines"),
                           label.size = 0,
                           ...) {

  text_legend <- richlegend_text(plot, aes_type = aes_type)

  legend_df <- tibble::tibble(x = x,
                              y = y,
                              label = text_legend)

  plot +
    ggtext::geom_richtext(data = legend_df,
                          aes(x = x, y = y, label = label),
                          label.padding = label.padding,
                          label.size = label.size,
                          inherit.aes = FALSE,
                          ...)


}






