#' Reads a local DATRAS table into R session
#'
#' It is assumed that the synax for the file
#' names are survey_year_quarter_xx.rds where xx stands for any of the three
#' dataframes, "HH", "HL" or "CA". This format is what is returned when using
#' the dtrs_download function.
#'
#' @param surveys Character vector containing survey names, e.g. c("NS-IBTS",
#' ROCKALL").
#' @param type The table type, one of "HH", "HL" or "CA"
#' @param path The directory path of were the data is stored.
#'
#' @return A tibble
#'
#' @export
#'
dtrs_read <- function(surveys, type = "HH", path = "data-raw/datras") {

  Type <- type

  files <-
    dplyr::data_frame(path = fs::dir_ls(path)) %>%
    dplyr::mutate(survey = basename(path),
                  survey = str_remove(survey, ".rds")) %>%
    tidyr::separate(survey, c("survey", "year", "quarter", "type"), sep = "_", convert = TRUE) %>%
    tidyr::drop_na()

  if(!missing(surveys)) files <- files %>% dplyr::filter(survey %in% surveys)

  files <-
    files %>%
    dplyr::filter(type == Type)

  purrr::map(files$path, read_rds) %>%
    dplyr::bind_rows()

}