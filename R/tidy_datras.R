#' Tidy DATRAS data
#'
#' @param d An object obtained from tidyices::dr_getdata or icesDatras::getDATRAS
#'
#' @return A tibble of the same dimention as the input
#' @export
#'
dr_tidy <- function(d) {

  colnames(d) <- tolower(colnames(d))

  d[d == -9] <- NA                      # convert all -9 to NA

  d <- d |> tibble::as_tibble()

  type <- d$recordtype[1]

  if(type == "HH") d <- d |> dr_tidyhh()
  if(type == "HL") d <- d |> dr_tidyhl()
  if(type == "CA") d <- d |> dr_tidyca()

  return(d)

}


#' dr_tidyhh
#'
#' @param d DATRAS raw haul dataframe, as obtained via the function tidyices::
#'
#' @return a tibble
#'
dr_tidyhh <- function(d) {

  d <-
    d |>
    dplyr::mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
                  timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                                    ":",
                                    stringr::str_sub(timeshot, 3, 4)),
                  timeshot = lubridate::ymd_hm(paste(year, month, day, timeshot)),
                  year = as.integer(year)) |>
    dplyr::filter(haulval == "V")

  return(d)

}


#' dr_tidyhl
#'
#' @param d DATRAS raw length dataframe, as obtained via the function
#' icesDatras::getDATRAS
#'
#' @return A tibble
#'
#'
dr_tidyhl <- function(d) {

  d <-
    d |>

    # remove records without lengthclass or without numbers at length
    dplyr::filter(!is.na(lngtclass), !is.na(hlnoatlngt) ) |>

    # length class to cm
    dplyr::mutate(length     = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass)) |>

    # apply subfactor
    dplyr::mutate(subfactor = ifelse(is.na(subfactor),1, subfactor)) |>
    dplyr::mutate(hlnoatlngt = hlnoatlngt * subfactor) |>

    # finalize aphia
    dplyr::mutate(aphia      = dplyr::case_when(!is.na(valid_aphia) & valid_aphia != "0" ~ valid_aphia,
                                                speccodetype                      == "W" ~ speccode,
                                                TRUE                                     ~ NA_character_),
                  aphia = as.integer(aphia),
                  year = as.integer(year))

  return(d)

}

#' dr_tidyca
#'
#' @param d DATRAS raw age dataframe
#'
#' @return A tibble
#'
dr_tidyca <- function(d) {

  d <-
    d %>%
    # turn everything to cm
    dplyr::mutate(length = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass),
                  indwgt = ifelse(indwgt <= 0, NA, indwgt),
                  aphia = dplyr::case_when(!is.na(valid_aphia) & valid_aphia != "0" ~ valid_aphia,
                                           speccodetype == "W" ~ speccode,
                                           TRUE ~ NA_character_),
                  aphia = as.integer(aphia),
                  year = as.integer(year))


  return(d)

}
