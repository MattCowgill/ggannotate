
select_geom <- function(id) {
  selectInput(NS(id, "geom"), "Geom",
    choices = c("text", "label", "curve"),
    selected = "text"
  )
}
