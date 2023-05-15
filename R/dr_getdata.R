#' Get any DATRAS data
#'
#' This a wrapper around icesDatras::getDATRAS and icesDatras::getFlexFile that ensure that proper variable
#' type is returned.
#'
#' @param record the data type required: "HH" haul data, "HL" length-based data, "CA" age-based data, "FL" flexfile.
#' @param surveys the survey acronym e.g. NS-IBTS
#' @param years a vector of years of the survey, e.g. c(2010, 2012) or 2005:2010.
#' @param quarters a vector of quarters of the year the survey took place, i.e. c(1, 4) or 1:4.
#' @param quiet whether to prevent messages from being shown on screen (default is TRUE).
#' @param fromweb whether to get the data from Datras website (default is TRUE).
#' @param folder folder name if data is to be retrieved from existing file.
#'
#' @return A tibble
#' @export
#'
dr_getdata <- function(record = "HH", surveys, years, quarters, quiet=TRUE,
                       fromweb = TRUE, folder=NA) {

  res <- list()
  counter <- 1

  if(fromweb == FALSE) {
    if(!dir.exists(folder)) stop(paste0("Error: folder ",folder," does not exist"))
  }

  if(!record %in% c("HH","HL","CA","FL")) stop("Error: record should be HH, HL, CA or FL")

  # purrr this
  for (survey in c(surveys)) {

    # record = "HH"; survey="NS-IBTS"; years=2020:2022; quarters=c(1,3); quiet=TRUE;
    # fromweb = FALSE; folder="C:/DATA/DATRAS/raw"

    # record="FL";surveys=c("FR-CGFS"); years=2020:2021;quarters=c(4);quiet=TRUE
    # fromweb = TRUE; folder=NA
    # survey = "FR-CGFS"

    #getDATRAS method
    if(record %in% c("HH","HL","CA")) {
      if (fromweb) {
        if(quiet) {
          d <-
            suppressWarnings(icesDatras::getDATRAS(record   = record,
                                                   survey   = survey,
                                                   years    = years,
                                                   quarters = quarters))

        } else {
          d <-
            icesDatras::getDATRAS(record   = record,
                                  survey   = survey,
                                  years    = years,
                                  quarters = quarters)

        } # end of if else  quiet

      } else {

        # from file
        d <-
          readr::read_rds(file.path(folder,paste0(tolower(survey),"_",tolower(record),".rds"))) |>
          dplyr::filter(Year %in% years, Quarter %in% quarters)

      } # end of if else fromweb

      if(!is.null(d)&!is.logical(d)) {

        if(!quiet) print(paste(record, survey, years, quarters))
        res[[counter]] <- d |> dr_settypes()
        counter=counter+1
      }

    # else: flexfile
    } else {

      # record = "FL"; survey="NS-IBTS"; years=2020:2022; quarters=c(1,3); quiet=TRUE;
      # fromweb = TRUE; folder=""

      # record = "FL"; survey="NS-IBTS"; years=2020:2022; quarters=c(1,3); quiet=TRUE;
      # fromweb = FALSE; folder="C:/DATA/DATRAS/raw"

      #getFLEXFILE method; loop over years and quarters
      if (fromweb) {
        res <-
          dr_getflexdata(surveys   = survey,
                         years     = years,
                         quarters  = quarters,
                         quiet = quiet)
      } else {

        # from file

        # record="FL"; survey=c("FR-CGFS"); years=2020:2021; quarters=c(4);
        # fromweb=FALSE; folder="C:/DATA/DATRAS/raw"; quiet=TRUE

        res <-
          readr::read_rds(file.path(folder,paste0(tolower(survey),"_",tolower(record),".rds"))) |>
          dplyr::filter(Year %in% years, Quarter %in% quarters)

      } # end of if else fromweb
    } # end of ifelse record type

  } # end of loop over surveys

  return(dplyr::bind_rows(res))

  # t <- dplyr::bind_rows(res)
  # readr::write_rds(t,  file = paste0(folder, "/", tolower(survey), "_fl.rds"))
}
