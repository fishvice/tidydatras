#' tidy_hh
#'
#' @param df A datras haul dataframe
#' @param remove logical; remove all but "essential" columns
#'
#' @return data.frame
#' @export
#'
tidy_hh <- function(df, remove = TRUE) {


  key_int <- datras_field_types |> filter(type == "int") |> pull(field) |> unique()
  key_dbl <- datras_field_types |> filter(type == "dbl") |> pull(field) |> unique()

  df <-
    df %>%
    mutate(across(everything(),   as.character)) %>%
    mutate(across(any_of(key_int), as.integer)) %>%
    mutate(across(any_of(key_dbl), as.numeric)) %>%
    dplyr::rename_all(tolower) %>%
    tibble::as_tibble() |>
    # create a unique station id
    id_unite(remove = FALSE) %>%
    # get proper date
    dplyr::mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
                  timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                                    ":",
                                    stringr::str_sub(timeshot, 3, 4)),
                  datetime = lubridate::ymd_hm(paste(year, month, day, timeshot)))



  if(!remove) {

    df %>%
      dplyr::select(-recordtype) %>%
      return()

  } else {
    df  %>%
      dplyr::select(survey, .id, year, quarter, survey, ship, gear, haulno,
                    date = datetime, country, depth, haulval, hauldur,
                    shootlat, shootlong,
                    haullat, haullong,
                    statrec = statrec,
                    daynight,
                    datatype) %>%
      return()
  }

}
