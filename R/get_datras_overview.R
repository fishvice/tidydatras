get_datras_overview <- function(survey) {

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
    tidyr::unnest() %>%
    dplyr::mutate(surveyname = paste0(survey, "_", year, "_", quarter))

  return(d)

}
