#' tidy_lengths
#'
#' @param hl datras untidy length dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param hh datras tidy haul data
#' @param species dataframe with species code, of the form obtained via function
#' get_species. Required column names are valid_aphia and latin. If dataframe
#' not supplied in the function argument, it will be automatically obtained.
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
tidy_lengths <- function(hl, hh, species) {

  colnames(hl) <- tolower(colnames(hl))

  if(!missing(species)) {

    colnames(species) <- tolower(colnames(species))

  } else {

    species <-
      hl %>%
      dplyr::select(valid_aphia) %>%
      dplyr::distinct() %>%
      get_latin()

  }

  hl <-
    hl %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(speccode), !is.na(lngtclass), !is.na(lngtcode)) %>%
    # EINAR - not sure if this is right, done here to test if any difference
    #         (did though not help with respect to discrepancy here below)
    #filter(specval == 1) %>%
    id_unite() %>%
    # only stations that are in the station table (north sea ibts)
    #  may be reduntant
    dplyr::filter(id %in% hh$id) %>%
    # length class to cm
    dplyr::mutate(length = ifelse(lngtcode %in% c("1"), lngtclass, lngtclass / 10),
                  hlnoatlngt = hlnoatlngt * subfactor) %>%
    # get the data type and hauldur
    dplyr::left_join(hh %>% select(id, datatype, hauldur), by = "id") %>%
    # catch per hour
    dplyr::mutate(n = ifelse(datatype == "R",
                             hlnoatlngt * 60 / hauldur,
                             hlnoatlngt)) %>%
    # join with latin name
    dplyr::left_join(species, by = "valid_aphia") %>%
    # select only needed columns
    dplyr::select(id, latin, species, sex, length, n) %>%
    dplyr::tbl_df()

  return(hl)

}