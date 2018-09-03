get_hl <- function(df) {

  d <-
    icesDatras::getHLdata(df$survey, df$year, df$quarter)

  if(is.null(d)) {
    d <- dplyr::data_frame()
  } else {
    d <-
      d %>%
      dplyr::mutate(DoorType = as.character(DoorType),
                    GearExp = as.character(GearExp),
                    StNo = as.character(StNo),
                    LngtCode = as.character(LngtCode),
                    Sex = as.character(Sex))
  }

  return(d %>% dplyr::as_tibble())

}
