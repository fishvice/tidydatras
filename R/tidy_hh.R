#' tidy_hh
#'
#' @param df A datras haul dataframe
#' @param all_variables A TRUE/FALSE flag. Should all variables
#' (default FALSE) or all variable (TRUE) be returned
#'
#' @return data.frame
#' @export
#'
tidy_hh <- function(df, all_variables = FALSE) {

  df <-
    df %>%
    select_all(tolower) %>%
    # create a unique station id
    id_unite(remove = FALSE) %>%
    # get proper date
    dplyr::mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
                  timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                                    ":",
                                    stringr::str_sub(timeshot, 3, 4)),
                  datetime = lubridate::ymd_hm(paste(year, month, day, timeshot))) %>%
    dplyr::tbl_df()

  if(all_variables) {

    df %>%
      select(-recordtype) %>%
      return()

  } else {
    df  %>%
      dplyr::select(id, year, quarter, survey, ship, gear, haulno,
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