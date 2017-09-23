#' @export
id_unite <- function(d, remove = TRUE) {
  d %>%
    unite(id, year, quarter, ship, gear, haulno, remove = remove)
}
