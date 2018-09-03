dtrs_import <- function(surveys, Type = "HH", path = "data-raw/datras") {

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