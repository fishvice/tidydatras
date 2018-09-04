#' Get an overview of avialable years and quarter
#'
#' @param survey A character vector specifying the surveys of interest. E.g.
#' c("ROCKALL", "NS-IBTS").
#'
#' @return A tibble containing variable survey, year and quarter
#' @export
#'
dtrs_overview <- function(survey) {

  if(missing(survey)) {
    d <-
      dplyr::data_frame(survey =  icesDatras::getSurveyList())
  } else {
    d <- dplyr::data_frame(survey = survey)
  }

  d <-
    d %>%
    dplyr::mutate(year = purrr::map(survey, icesDatras::getSurveyYearList)) %>%
    tidyr::unnest() %>%
    dplyr::mutate(quarter = purrr::map2(survey, year, icesDatras::getSurveyYearQuarterList)) %>%
    tidyr::unnest()

  return(d)

}
