#' Tidy DATRAS data
#'
#' NEED GOOD EXPLANATION HERE
#'
#' @param d Any nontidied table of the form HH, HL or CA
#' @param all_variables A boolean (default FALSE) indicating
#' if all or only "necessary" variables are return.
#' @param ...
#'
#'
dtrs_tidy <- function(d, all_variables = FALSE, ...) {

  type = unique(d$RecordType)[1]

  if(type == "HH") {
    d <- dtrs_tidy_hh(d, all_variables = all_variables)
    return(d)
  }

  if(type == "CA") {
    d <- dtrs_tidy_ca(d, all_variables = all_variables)
    return(d)
  }

  # one more for type == "HL"
  if(type == "HL") {
    d <- dtrs_tidy_hl(d, hh = hh, all_variables = all_variables)
    return(d)
  }



}
