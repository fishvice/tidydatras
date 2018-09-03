# should have an option of which table to download
get_datras <- function(d, path = "data-raw/datras", save = TRUE) {

  d <-
    d %>%
    mutate(id = 1:n()) %>%
    group_by(id) %>%
    nest() %>%
    # operating on datras webservice
    mutate(hh = map(data, tidyices:::get_hh),
           hl = map(data, tidyices:::get_hl),
           ca = map(data, tidyices:::get_ca)) %>%
    unnest(data) %>%
    select(survey, year, quarter, hh, hl, ca)

  if(!save) {
    return(d)
  } else {

    d <-
      d %>%
      mutate(file_out = paste0(survey, "_", year, "_", quarter))
    d %>%
      select(file_out, hh) %>%
      mutate(file_out = paste0(file_out, "_hh.rds"),
             file_out_path = file.path(path, file_out)) %>%
      transpose() %>%
      walk(~write_rds(.$hh, .$file_out_path))
    d %>%
      select(file_out, hl) %>%
      mutate(file_out = paste0(file_out, "_hl.rds"),
             file_out_path = file.path(path, file_out)) %>%
      transpose() %>%
      walk(~write_rds(.$hl, .$file_out_path))
    d %>%
      select(file_out, ca) %>%
      mutate(file_out = paste0(file_out, "_ca.rds"),
             file_out_path = file.path(path, file_out)) %>%
      transpose() %>%
      walk(~write_rds(.$ca, .$file_out_path))
  }

}
