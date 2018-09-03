get_ca <- function(df) {
  d <-
    icesDatras::getCAdata(df$survey, df$year, df$quarter)

  if(is.null(d)) {
    d <- dplyr::data_frame()
  } else {
    d <-
      d %>%
      dplyr::mutate(Maturity = as.character(Maturity),
                    AreaCode = as.character(AreaCode),
                    DoorType = as.character(DoorType),
                    GearExp = as.character(GearExp),
                    LngtCode = as.character(LngtCode),
                    PlusGr = as.character(PlusGr),
                    Sex = as.character(Sex),
                    StNo = as.character(StNo))
  }

  return(d %>% dplyr::as_tibble())

}
