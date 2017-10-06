#' tidy_ages
#'
#' @param ca datras untidy age dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param species dataframe with species code, of the form obtained via function
#' get_species. Required column names are valid_aphia and latin. If dataframe
#' not supplied in the function argument, it will be automatically obtained.
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
tidy_ages <- function(ca, species) {


  colnames(ca) <- tolower(colnames(ca))


  if(!missing(species)) {

    colnames(species) <- tolower(colnames(species))

  } else {

    species <-
      ca %>%
      select(valid_aphia) %>%
      distinct() %>%
      get_latin()

  }

  ca <-
    ca %>%
    id_unite() %>%
    # turn everything to cm
    mutate(length = ifelse(!lngtcode %in% c("1"), lngtclass / 10, lngtclass),
           indwgt = ifelse(indwgt <= 0, NA, indwgt)) %>%
    left_join(species) %>%
    select(id, latin, species, length, sex, maturity, age, wgt = indwgt, n = noatalk)

  return(ca)

}