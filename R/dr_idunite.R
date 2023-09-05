#' dr_idunite
#'
#' Unite varible that constitute a haul identification (year, quarter, ship, gear,
#' haulno) into variable id
#'
#' @param d A dataframe containing variable id
#' @param remove If TRUE, remove input columns from output data frame.
#'
#' @export
dr_idunite <- function(d, remove = TRUE) {
  d %>%
    tidyr::unite(col="id",
                 survey, year, quarter, country, ship, gear, haulno,
                 remove = remove)
}



