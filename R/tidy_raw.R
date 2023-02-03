tidy_raw <- function(d, remove = TRUE) {

  key_int <- datras_field_types |> filter(type == "int") |> pull(field) |> unique()
  key_dbl <- datras_field_types |> filter(type == "dbl") |> pull(field) |> unique()

  d <-
    d %>%
    mutate(across(everything(),   as.character)) %>%
    mutate(across(any_of(key_int), as.integer)) %>%
    mutate(across(any_of(key_dbl), as.numeric)) %>%
    dplyr::rename_all(tolower) %>%
    tibble::as_tibble() |>
    # create a unique station id
    id_unite(remove = remove)

  return(d)

}


#' tidy_hh
#'
#' @param d DATRAS raw haul dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param remove logical; remove all but "essential" columns. Currently not active.
#'
#' @return a tibble
#' @export
#'
tidy_hh <- function(d, remove = TRUE) {

  d <-
    d |>
    # not the same remove as in the function call
    tidy_raw(remove = TRUE) |>
    # get proper date
    dplyr::mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
                  timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                                    ":",
                                    stringr::str_sub(timeshot, 3, 4)),
                  datetime = lubridate::ymd_hm(paste(stringr::str_sub(id, 1, 4), month, day, timeshot)))

  return(d)

}


#' tidy_hl
#'
#' @param d DATRAS raw length dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param remove logical; remove all but "essential" columns.  Currently not active.
#'
#' @return A tibble
#'
#' @export
#'
tidy_hl <- function(d, remove = TRUE) {

  d <-
    d |>
    # not the same remove as in the function call
    tidy_raw(remove = TRUE) |>
    # length class to cm
    dplyr::mutate(length = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass),
                  hlnoatlngt = hlnoatlngt * subfactor)

  return(d)

}

#' tidy_ca
#'
#' @param d DATRAS raw age dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param remove logical; remove all but "essential" columns. Currently not active.
#'
#' @return A tibble with
#'
#' \describe{
#'   \item{id}{haul id}
#'   \item{latin}{Latin name of species}
#'   \item{species}{English name of species}
#'   \item{length}{Length in centimeters}
#'   \item{sex}{Sex of fish}
#'   \item{maturity}{The maturity scale}
#'   \item{age}{Age in years}
#'   \item{wgt}{Weight in grammes}
#'   \item{n}{Number of fish}
#' }
#'
#' @export
#'
tidy_ca <- function(d, remove = TRUE) {

  d <-
    d %>%
    # not the same remove as in the function call
    tidy_raw(remove = TRUE) |>
    # turn everything to cm
    dplyr::mutate(length = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass),
                  indwgt = ifelse(indwgt <= 0, NA, indwgt))


  return(d)

}
