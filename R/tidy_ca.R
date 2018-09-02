#' tidy_ca
#'
#' @param ca datras untidy age dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param species dataframe with species code, of the form obtained via function
#' get_species. Required column names are valid_aphia and latin. If dataframe
#' not supplied in the function argument, it will be automatically obtained.
#' @param all_variables A TRUE/FALSE flag. Should all variables
#' (default FALSE) or all variable (TRUE) be returned
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
tidy_ca <- function(ca, species, all_variables = FALSE) {


  colnames(ca) <- tolower(colnames(ca))

  ca <-
    ca %>%
    id_unite() %>%
    # turn everything to cm
    dplyr::mutate(length = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass),
                  indwgt = ifelse(indwgt <= 0, NA, indwgt))

  # If species dataframe passed to function, supply latin name
  if(!missing(species)) {
    ca <-
      ca %>%
      mutate(aphia = valid_aphia,
             aphia = ifelse(is.na(aphia) & speccodetype == "W",
                            speccode,
                            aphia)) %>%
      left_join(species, by = "aphia") %>%
      select(-aphia)
  }

  ca <-
    ca %>%
    dplyr::mutate(maturity = as.character(maturity),
                  areacode = as.character(areacode),
                  doortype = as.character(doortype),
                  gearexp = as.character(gearexp),
                  lngtcode = as.character(lngtcode),
                  plusgr = as.character(plusgr),
                  sex = as.character(sex),
                  stno = as.character(stno)) %>%
    as_tibble()

  if(!all_variables) {
    ca <-
      ca %>%
      dplyr::select(survey, id, latin, length, sex, maturity, age, wgt = indwgt, n = noatalk) %>%
      as_tibble()
  }

  return(ca)

}