#' Calculate with DATRAS data
#'
#' @param d A tidy hl or ca object obtained from tidydatras::dr_getdata
#' @param hh A tidy haul object obtained from tidydatras::dr_getdata
#'
#' @return A tibble of the same dimension as the input
#' @export
#'
dr_calccpue <- function(d, hh, fl) {

  colnames(d) <- tolower(colnames(d))

  d <- d |> tibble::as_tibble()

  if(unique(d$recordtype) == "HL") d <- d |> dr_calccpue_hl(hh, fl)
  if(unique(d$recordtype) == "CA") d <- d |> dr_calccpue_ca(hh)

  return(d)

}



#' dr_calccpue_hl
#'
#' @param d A tidy hl object obtained from tidydatras::dr_getdata
#' @param hh A tidy haul object obtained from tidydatras::dr_getdata
#'
#' @return A tibble
#'
#'
dr_calccpue_hl <- function(d, hh, fl) {

  d <-
    d |>

    #left join hh
    dplyr::left_join(hh |> dplyr::select(id,
                                         datatype, hauldur, statrec, shootlong, shootlat, haulval,
                                         distance, doorspread, wingspread, depth),
                     by = "id") |>

    #left join fl
    dplyr::left_join(fl |> dplyr::select(id,
                                         cal_distance, cal_doorspread, cal_wingspread,
                                         sweptareadskm2, sweptareawskm2),
                     by = "id") |>

    # catch per hour
    dplyr::mutate(number_per_hour   = dplyr::case_when(
         datatype %in% c("S", "R")  ~ hlnoatlngt * 60 / hauldur,
         datatype == "C"            ~ hlnoatlngt,
         TRUE                       ~ as.numeric(NA))) |>

    # catch per km2
    dplyr::mutate(number_per_km2_ws = hlnoatlngt / sweptareawskm2) |>
    dplyr::mutate(number_per_km2_ds = hlnoatlngt / sweptareadskm2)

  return(d)

}

#' dr_calccpue_ca
#'
#' @param d A tidy ca object obtained from tidydatras::dr_getdata
#' @param hh A tidy haul object obtained from tidydatras::dr_getdata
#'
#' @return A tibble
#'
#'
dr_calccpue_ca <- function(d, hh) {

  d <-
    d |>

    #left join hh
    dplyr::left_join(hh %>% dplyr::select(id,
                                          datatype, hauldur, statrec, shootlong, shootlat),
                     by = "id") |>

    # catch per hour
    dplyr::mutate(cpue_number_per_hour = dplyr::case_when(
      datatype == "R"  ~ as.numeric(canoatlngt) * 60 / hauldur,
      datatype == "C"  ~ as.numeric(canoatlngt),
      TRUE             ~ as.numeric(NA)))

    return(d)

}
