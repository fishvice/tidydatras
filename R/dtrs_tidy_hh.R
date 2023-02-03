#' dtrs_tidy_hh
#'
#' @param df A datras haul dataframe
#' @param all_variables A TRUE/FALSE flag. Should all variables
#' (default FALSE) or all variable (TRUE) be returned
#'
#' @return data.frame
#'
dtrs_tidy_hh <- function(df, all_variables = FALSE) {

  df <-
    df %>%
    dplyr::rename_all(tolower) %>%
    # create a unique station id
    id_unite(remove = FALSE) %>%
    # get proper date
    dplyr::mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
                  timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                                    ":",
                                    stringr::str_sub(timeshot, 3, 4)),
                  datetime = lubridate::ymd_hm(paste(year, month, day, timeshot))) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(doortype = as.character(doortype),
                  gearexp = as.character(gearexp),
                  hydrostno = as.character(hydrostno),
                  rigging = as.character(rigging),
                  stno = as.character(stno),
                  stratum = as.character(stratum),
                  thermocline = as.character(thermocline))

  if(all_variables) {

    df %>%
      dplyr::select(-recordtype) %>%
      return()

  } else {
    df  %>%
      dplyr::select(survey, id, year, quarter, survey, ship, gear, haulno,
                    date = datetime, country, depth, haulval, hauldur,
                    shootlat, shootlong,
                    haullat, haullong,
                    statrec = statrec,
                    daynight,
                    datatype,
                    stdspecreccode,
                    bycspecreccode) %>%
      return()
  }

}
