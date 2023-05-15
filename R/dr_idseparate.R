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
    tidyr::separate(id,
                    c("survey", "year", "quarter", "country", "ship", "gear", "haulno"),
                    remove = remove,
                    convert = TRUE)
}
