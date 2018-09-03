download_datras <- function(d, what = c("hh", "hl", "ca"), path = "data-raw/datras") {

  for(i in 1:nrow(d)) {
    d[i,] %>%
      get_hh() %>%
      readr::write_rds(path = paste0(path, "/",
                              d$survey[i], "_",
                              d$year[i], "_",
                              d$quarter[i], "_",
                              "hh.rds"))
    d[i,] %>%
      get_hl() %>%
      readr::write_rds(paste0(path, "/",
                       d$survey[i], "_",
                       d$year[i], "_",
                       d$quarter[i], "_",
                       "hl.rds"))
    d[i,] %>%
      get_ca() %>%
      readr::write_rds(paste0(path, "/",
                       d$survey[i], "_",
                       d$year[i], "_",
                       d$quarter[i], "_",
                       "ca.rds"))
  }
}