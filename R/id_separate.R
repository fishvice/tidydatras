#' id_separatate
#'
#' Separate the haul id code into its components
#'
#' @param d A dataframe containing variable id
#' @param remove If TRUE, remove input column from output data frame.
#'
#' @export
id_separate <- function(d, remove = TRUE) {
  d %>%
    tidyr::separate(id, c("year", "quarter", "ship", "gear", "haulno"),
                    remove = remove,
                    convert = TRUE)
}
