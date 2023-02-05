# run via terminal:
#   nohup R < R/xxx.R --vanilla > logs/2023-02-05_download.log &

#' Download all DATRAS data
#'
#' Saves rds files of all surveys HH, HL and CA data into a specified directory.
#'
#' @param save_dir The path to the directory where data are to be stored
#' @param verbose boolean (default TRUE) that prints out survey being worked on
#'
dr_download_all <- function(save_dir, verbose = TRUE) {

  dtrs <- icesDatras::getDatrasDataOverview()

  # Loop through each survey, download and save ----------------------------------
  for(i in 1:length(dtrs)) {

    sur <- names(dtrs[i])
    if(verbose) print(paste(i, sur))

    yrs <- rownames(dtrs[[i]]) %>% as.integer()
    qts <- c(1:4)

    dr_getdata(record = "HH", survey = sur, years = yrs, quarters = qts) |>
      readr::write_rds(file = paste0(save_dir, "/", tolower(sur), "_hh.rds"))
    dr_getdata(record = "HL", survey = sur, years = yrs, quarters = qts) |>
      readr::write_rds(file = paste0(save_dir, "/", tolower(sur), "_hl.rds"))
    dr_getdata(record = "CA", survey = sur, years = yrs, quarters = qts) |>
      readr::write_rds(file = paste0(save_dir, "/", tolower(sur), "_ca.rds"))
  }

}
