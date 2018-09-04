#' Download and save DATRAS data
#'
#' Downloads and stores the DATRAS exchange data as R binary files in a directory
#' specified by the argument path. For each year, survey and quarter the HH
#' (haul dataframe), HL (length frequency dataframe) and CA (detail measurment
#' dataframe) are stored as a separate binary file. The synax for the file
#' names are survey_year_quarter_xx.rds where xx stands for any of the three
#' dataframe.
#'
#' @param d A dataframe containing variables survey, year and quarter.
#' @param record A vector containin one, tow or all of "HH", "HL" and/or "CA".
#' The default is c("HH", "HL", "CA") meaning that all three tables will be
#' downloaded and stored.
#' @param path Path to the directory where the data are stored.
#'
#' @export
#'
dtrs_download <- function(d, record = c("HH", "HL", "CA"), path = "data-raw/datras") {

  for(i in 1:nrow(d)) {

    if("HH" %in% record) {
      d[i,] %>%
        get_hh() %>%
        readr::write_rds(path = paste0(path, "/",
                                       d$survey[i], "_",
                                       d$year[i], "_",
                                       d$quarter[i], "_",
                                       "HH.rds"))
    }

    if("HL" %in% record) {
      d[i,] %>%
        get_hl() %>%
        readr::write_rds(paste0(path, "/",
                                d$survey[i], "_",
                                d$year[i], "_",
                                d$quarter[i], "_",
                                "HL.rds"))
    }

    if("CA" %in% record) {
      d[i,] %>%
        get_ca() %>%
        readr::write_rds(paste0(path, "/",
                                d$survey[i], "_",
                                d$year[i], "_",
                                d$quarter[i], "_",
                                "CA.rds"))
    }
  }
}