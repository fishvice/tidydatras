#' tidy_hauls
#'
#' @param df A datras haul dataframe
#'
#' @return data.frame
#' @export
#'
tidy_hauls <- function(df) {

  colnames(df) <- tolower(colnames(df))

  df <-
    df %>%
    # create a unique station id
    id_unite(remove = FALSE) %>%
    # get proper date
    mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
           timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                             ":",
                             stringr::str_sub(timeshot, 3, 4)),
           datetime = lubridate::ymd_hm(paste(year, month, day, timeshot))) %>%
    # Get roundfish area - not included in the function
    # mutate(area = gisland::geo_inside(shootlong, shootlat, ns_area, "AreaName")) %>%
    # select only "needed" column - this should may be be user option
    select(id, survey, year, quarter, ship, gear, haulno, haulval, country, hauldur,
           shootlat, shootlong, date = datetime, depth,
           subarea = statrec, daynight, datatype) %>%
    tbl_df()

  return(df)

}