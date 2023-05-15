#' Get an overview of available years and quarter for flexfiles
#'
#' @param surveys A character vector specifying the surveys of interest. E.g.
#' c("ROCKALL", "NS-IBTS").
#'
#' @return A tibble containing variable survey, year and quarter and flexfile (0 or 1)
#' @export
#'
dr_getflexoverview <- function(surveys, years, quarters) {

  d <- tibble::tibble()
  for (survey in surveys) {
    for (y in years) {
      for (q in quarters) {
        print(paste(survey, y, q))
        x <- suppressWarnings(icesDatras::getFlexFile(survey = survey, year = y, quarter = q))
        if (!is.null(x)&!is.logical(x)) {
          t <- tibble::tibble(survey=survey, year=y, quarter=q, flexfile=0)
        } else {
          t <- tibble::tibble(survey=survey, year=y, quarter=q, flexfile=1)
        }
        d <- dplyr::bind_rows(d,t)
      }
    }
  }
  return(d)
}
