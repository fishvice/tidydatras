#' tidy_ages
#'
#' @param ca datras CA data
#' @param species species code list
#'
#' @return data_frame
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

    # send to global environment
    .species <<- species

  }

  ca <-
    ca %>%
    unite(id, year, quarter, ship, gear, haulno) %>%
    # turn everything to cm
    mutate(length = ifelse(!lngtcode %in% c("1"), lngtclass / 10, lngtclass),
           indwgt = ifelse(indwgt <= 0, NA, indwgt)) %>%
    left_join(species) %>%
    select(id, latin, species, length, sex, maturity, age, wgt = indwgt, n = noatalk)

  return(ca)

}