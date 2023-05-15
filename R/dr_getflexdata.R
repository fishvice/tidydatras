#' Get DATRAS flex data
#'
#' This a wrapper around icesDatras::getFlexFile that ensure that proper variable
#' type is returned.
#'
#' @param surveys the survey acronym e.g. NS-IBTS
#' @param years a vector of years of the survey, e.g. c(2010, 2012) or 2005:2010.
#' @param quarters a vector of quarters of the year the survey took place, i.e. c(1, 4) or 1:4.
#' @param quiet whether to prevent messages from being shown on screen (default is TRUE).
#'
#' @return A tibble
#' @export
#'
dr_getflexdata <- function(surveys, years, quarters, quiet=TRUE) {

  res <- list()
  counter <- 1

  # record = "FL"; survey="NS-IBTS"; years=2020:2022; quarters=c(1,3); quiet=TRUE;
  # fromweb = FALSE; folder="C:/DATA/DATRAS/raw"

  # purrr this
  for (survey in c(surveys)) {
    for (y in years) {
      # y = 2015
      for (q in quarters) {
        # q = 4
        if(quiet) {
          d <-
            suppressWarnings(icesDatras::getFlexFile(survey   = survey,
                                                     year     = y,
                                                     quarter  = q))
        } else {
          d <-
            icesDatras::getFlexFile(survey   = survey,
                                    year    = y,
                                    quarter = q)
        } # end of ifelse quiet

        if(!is.null(d)&!is.logical(d)) {

          if(!quiet) print(paste(record, survey, y, q))
          res[[counter]] <- d |> dplyr::mutate(RecordType = "FL") |> dr_settypes()
          counter=counter+1

        } # end of non-empty data.frame (and not a logical)
      } # end of loop over quarters
    } # end of loop over years
  } # end of loop over surveys

  return(dplyr::bind_rows(res))

}
