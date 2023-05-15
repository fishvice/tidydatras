#' Get an overview of avialable years and quarter
#'
#' @param surveys A character vector specifying the surveys of interest. E.g.
#' c("ROCKALL", "NS-IBTS").
#'
#' @return A tibble containing variable survey, year and quarter
#' @export
#'
dr_getoverview <- function(surveys) {

  if(missing(surveys)) {
    d <-
      tibble::tibble(survey =  icesDatras::getSurveyList())
  } else {
    d <- tibble::tibble(survey = surveys)
  }

  suppressWarnings(
    d <-
      d |>
      # TODO: Get rid of error message
      dplyr::mutate(year = purrr::map(survey, icesDatras::getSurveyYearList)) %>%
      tidyr::unnest(year) %>%
      # TODO: Get rid of error message
      dplyr::mutate(quarter = purrr::map2(survey, year, icesDatras::getSurveyYearQuarterList)) %>%
      tidyr::unnest(quarter)
  )

  return(d)

}
