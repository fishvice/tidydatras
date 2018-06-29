#' tidy_hauls
#'
#' @param df A datras haul dataframe
#' @param selected_variables A TRUE/FALSE flag. Should only selected variables
#' (default TRUE) or all variable (FALSE) returned
#'
#' @return data.frame
#' @export
#'
tidy_hauls <- function(df, selected_column = TRUE) {

  colnames(df) <- tolower(colnames(df))

  df <-
    df %>%
    # create a unique station id
    id_unite(remove = FALSE) %>%
    # get proper date
    dplyr::mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
                  timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                                    ":",
                                    stringr::str_sub(timeshot, 3, 4)),
                  datetime = lubridate::ymd_hm(paste(year, month, day, timeshot))) %>%
    dplyr::tbl_df()

  if(!selected_column) {

    df %>%
      select(-recordtype) %>%
      return()

  } else {
    df  %>%
      dplyr::select(id, survey, year, quarter, ship, gear, haulno, haulval, country, hauldur,
                    shootlat, shootlong, date = datetime, depth,
                    subarea = statrec, daynight, datatype) %>%
      return()
  }

}