dtrs_tidy <- function(d, ...) {

  type = unique(d$RecordType)[1]

  if(type == "HH") {
    d <- tidy_hh(d)
    return(d)
  }

  if(type == "CA") {
    d <- tidy_ca(d)
    return(d)
  }

  # one more for type == "HL"
  if(type == "HL") {
    return(dplyr::data_frame())
  }



}
