#' id_unite
#'
#' Unite varible that constitute a haul identification (year, quarter, ship, gear,
#' haulno) into variable id
#'
#' @param d A dataframe containing variable id
#' @param remove If TRUE, remove input columns from output data frame.
#'
#' @export
id_unite <- function(d, remove = TRUE) {
  d %>%
    tidyr::unite(id, year, quarter, ship, gear, haulno, remove = remove)
}
