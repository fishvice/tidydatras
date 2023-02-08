#' Calculate with DATRAS data
#'
#' @param d A tidy hl or ca object obtained from tidyices::dr_getdata
#' @param hh A tidy haul object obtained from tidyices::dr_getdata
#'
#' @return A tibble of the same dimension as the input
#' @export
#'
dr_calccpue <- function(d, hh) {

  colnames(d) <- tolower(colnames(d))

  d <- d |> tibble::as_tibble()

  if(unique(d$recordtype) == "HL") d <- d |> dr_calccpue_hl(hh)
  if(unique(d$recordtype) == "CA") d <- d |> dr_calccpue_ca(hh)

  return(d)

}



#' dr_calccpue_hl
#'
#' @param d A tidy hl object obtained from tidyices::dr_getdata
#' @param hh A tidy haul object obtained from tidyices::dr_getdata
#'
#' @return A tibble
#'
#'
dr_calccpue_hl <- function(d, hh) {

  d <-
    d |>

    #left join hh
    dplyr::left_join(hh %>% dplyr::select(id,
                                          datatype, hauldur),
                     by = "id") |>

    # catch per hour
    dplyr::mutate(nperhour = dplyr::case_when(
         datatype == "R"  ~ hlnoatlngt * 60 / hauldur,
         datatype == "C"  ~ hlnoatlngt,
         TRUE             ~ as.numeric(NA)))

  return(d)

}

#' dr_calccpue_ca
#'
#' @param d A tidy ca object obtained from tidyices::dr_getdata
#' @param hh A tidy haul object obtained from tidyices::dr_getdata
#'
#' @return A tibble
#'
#'
dr_calccpue_ca <- function(d, hh) {

  d <-
    d |>

    #left join hh
    dplyr::left_join(hh %>% dplyr::select(id,
                                          datatype, hauldur),
                     by = "id") |>

    # catch per hour
    dplyr::mutate(nperhour = dplyr::case_when(
      datatype == "R"  ~ as.numeric(canoatlngt) * 60 / hauldur,
      datatype == "C"  ~ as.numeric(canoatlngt),
      TRUE             ~ as.numeric(NA)))

    return(d)

}
