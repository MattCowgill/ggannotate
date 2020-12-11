

replace_elements <- function(list_a, list_b) {
  list_out <- list_a

  for (name in names(list_a)) {
    if (name %in% names(list_b)) {
      list_out[[name]] <- list_b[[name]]
    }
  }
  list_out
}
