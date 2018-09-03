get_hh <- function(df) {

  d <-
    icesDatras::getHHdata(df$survey, df$year, df$quarter)

  if(is.null(d)) {
    d <- dplyr::data_frame()
  } else {
    d <-
      d %>%
      dplyr::mutate(DoorType = as.character(DoorType),
                    GearExp = as.character(GearExp),
                    HydroStNo = as.character(HydroStNo),
                    Rigging = as.character(Rigging),
                    StNo = as.character(StNo),
                    Stratum = as.character(Stratum),
                    ThermoCline = as.character(ThermoCline))
  }

  return(d %>% dplyr::as_tibble())

}
