#' dtrs_tidy_hl
#'
#' @param hl datras untidy length dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param hh datras tidy haul data
#' @param species dataframe with species code, of the form obtained via function
#' get_species. Required column names are valid_aphia and latin. If dataframe
#' not supplied in the function argument, it will be automatically obtained.
#' @param all_variables A TRUE/FALSE flag. Should all variables
#' (default FALSE) or all variable (TRUE) be returned
#'
#' @return A tibble with 6 variables:
#'
#' \describe{
#'   \item{id}{haul id}
#'   \item{latin}{Latin name of species}
#'   \item{species}{English name of species}
#'   \item{sex}{Sex of fish}
#'   \item{length}{Length in centimeters}
#'   \item{n}{Number of fish per standardized to 60 minute haul}
#' }
#'
#' @export
#'
dtrs_tidy_hl <- function(hl, hh, species, all_variables = FALSE) {

  colnames(hl) <- tolower(colnames(hl))

  if(missing(species)) species <- readr::read_csv("ftp://ftp.hafro.is/pub/reiknid/einar/datras_worms.csv")


  hl <-
    hl %>%
    #dplyr::distinct() %>%
    #dplyr::filter(!is.na(speccode), !is.na(lngtclass), !is.na(lngtcode)) %>%
    # EINAR - not sure if this is right, done here to test if any difference
    #         (did though not help with respect to discrepancy here below)
    #filter(specval == 1) %>%
    id_unite() %>%
    # only stations that are in the station table (north sea ibts)
    #  may be reduntant
    dplyr::filter(id %in% hh$id) %>%
    # length class to cm
    # Note: In some cases I have seen floor being used in lngtclass / 10
    dplyr::mutate(length = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass),
                  hlnoatlngt = hlnoatlngt * subfactor) %>%
    # get the data type and hauldur
    dplyr::left_join(hh %>%
                       dplyr::select(id, datatype, hauldur), by = "id") %>%
    # catch per hour
    dplyr::mutate(n = ifelse(datatype == "R",
                             hlnoatlngt * 60 / hauldur,
                             hlnoatlngt))
  # If species dataframe passed to function, supply latin name
  if(!missing(species)) {
    hl <-
      hl %>%
      dplyr::mutate(aphia = valid_aphia,
             aphia = ifelse(is.na(aphia) & speccodetype == "W",
                            speccode,
                            aphia)) %>%
      dplyr::left_join(species, by = "aphia") %>%
      dplyr::select(-aphia)
  }

  hl <-
    hl %>%
    dplyr::mutate(doortype = as.character(doortype),
                  gearexp = as.character(gearexp),
                  stno = as.character(stno),
                  lngtcode = as.character(lngtcode),
                  sex = as.character(sex)) %>%
    dplyr::as_tibble()

  if(!all_variables) {
    hl <-
      hl %>%
      # select only needed columns
      dplyr::select(survey, id, latin, sex, length, n)
  }


  return(hl %>% dplyr::as_tibble())

}