# run via terminal:
#   nohup R < R/xxx.R --vanilla > logs/2023-02-05_download.log &

#' Download all DATRAS data
#'
#' Saves rds files of all surveys HH, HL and CA data into a specified directory.
#' This takes a while so suggest to run this function in an R-script that is run via
#' nohub or "RStudio Background Jobs".
#'
#' @param save_dir The path to the directory where data are to be stored
#' @param surveys survey name or names (optional)
#' @param years year selection (optional)
#' @param quarters quarter selection (optional)
#' @param quiet boolean (default FALSE) that regulates whether info is being shown
#'
dr_download_all <- function(save_dir, surveys="all", years="all", quarters="all", quiet = FALSE) {

  # save_dir = "C:/DATA/DATRAS/raw"; surveys="NS-IBTS"; years=2020:2021; quarters=c(1); quiet=FALSE
  # save_dir ="C:/DATA/DATRAS/raw"; surveys=c("NS-IBTS", "FR-CGFS"); years=2021:2022; quarters=4; quiet=FALSE

  print("Getting survey overviews")
  suppressWarnings(
    if (any(surveys == "all")) {
      dtrs <- dr_getoverview()
      if(all(years) != "all")    {dtrs <- dplyr::filter(dtrs, year %in% years)}
      if(all(quarters) != "all") {dtrs <- dplyr::filter(dtrs, quarter %in% quarters)}
    } else {
      dtrs <- dr_getoverview(surveys)
      if(all(years) != "all")    {dtrs <- dplyr::filter(dtrs, year %in% years)}
      if(all(quarters) != "all") {dtrs <- dplyr::filter(dtrs, quarter %in% quarters)}
    }
  )

  dtrs <- dtrs |> nest(year=year, quarter=quarter)

  # print(head(dtrs))

  # Loop through each survey, download and save ----------------------------------
  for(i in 1:nrow(dtrs)) {

    # sur <- names(dtrs[i])
    sur <- dtrs[i,]$survey
    if(!quiet) print(paste(sur))

    yrs <- dplyr::filter(dtrs, survey==sur) |> dplyr::select(year)    |> tidyr::unnest(year) |> unlist() |>   as.integer()
    qts <- dplyr::filter(dtrs, survey==sur) |> dplyr::select(quarter) |> tidyr::unnest(quarter) |> distinct(quarter) |>
      unlist() |>   as.integer()

    if(!quiet) print("HH")
    dr_getdata(record = "HH", survey = sur, years = yrs, quarters = qts) |>
      readr::write_rds(file = paste0(save_dir, "/", tolower(sur), "_hh.rds"))
    if(!quiet) print("HL")
    dr_getdata(record = "HL", survey = sur, years = yrs, quarters = qts) |>
      readr::write_rds(file = paste0(save_dir, "/", tolower(sur), "_hl.rds"))
    if(!quiet) print("CA")
    dr_getdata(record = "CA", survey = sur, years = yrs, quarters = qts) |>
      readr::write_rds(file = paste0(save_dir, "/", tolower(sur), "_ca.rds"))
    if(!quiet) print("FL")
    dr_getdata(record = "FL", survey = sur, years = yrs, quarters = qts) |>
      readr::write_rds(file = paste0(save_dir, "/", tolower(sur), "_fl.rds"))
  }

}
