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

  d <-
    d |>
    # TODO: Get rid of error message
    dplyr::mutate(year = purrr::map(survey, icesDatras::getSurveyYearList)) %>%
    tidyr::unnest(year) %>%
    # TODO: Get rid of error message
    dplyr::mutate(quarter = purrr::map2(survey, year, icesDatras::getSurveyYearQuarterList)) %>%
    tidyr::unnest(quarter)

  return(d)

}

dr_settypes <- function(d) {

  key_int <- dr_coltypes |> filter(type == "int") |> pull(field) |> unique()
  key_dbl <- dr_coltypes |> filter(type == "dbl") |> pull(field) |> unique()

  d <-
    d %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),    as.character))  |>
    dplyr::mutate(dplyr::across(dplyr::any_of(key_int), as.integer))  |>
    dplyr::mutate(dplyr::across(dplyr::any_of(key_dbl), as.numeric))

  return(d)

}

#' Get any DATRAS data
#'
#' This a wrapper around icesDatras::getDATRAS that ensure that proper variable
#' type is returned.
#'
#' @param record the data type required: "HH" haul data, "HL" length-based data, "CA" age-based data.
#' @param surveys the survey acronym e.g. NS-IBTS
#' @param years a vector of years of the survey, e.g. c(2010, 2012) or 2005:2010.
#' @param quarters a vector of quarters of the year the survey took place, i.e. c(1, 4) or 1:4.
#'
#' @return A tibble
#' @export
#'
dr_getdata <- function(record = "HH", surveys, years, quarters) {

  res <- list()
  counter <- 0

  # purrr this
  for (survey in c(surveys)) {
    counter <- counter + 1
    d <-
      suppressMessages(icesDatras::getDATRAS(record   = record,
                                         survey   = survey,
                                         years    = years,
                                         quarters = quarters))

    if(!is.null(d)) res[[counter]] <- d |> dr_settypes()

  }

  return(dplyr::bind_rows(res))

}
