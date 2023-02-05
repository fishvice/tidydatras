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
    tidyr::unite(id, year, quarter, ship, gear, haulno, remove = remove)
}




#' dr_idseparate
#'
#' Separate the haul id code into its components
#'
#' @param d A dataframe containing variable id
#' @param remove If TRUE, remove input column from output data frame.
#'
#' @export
dr_idseparate <- function(d, remove = TRUE) {
  d %>%
    tidyr::separate(id, c("year", "quarter", "ship", "gear", "haulno"),
                    remove = remove,
                    convert = TRUE)
}
