#' tidy_lengths
#'
#' @param hl datras length untidy length data
#' @param hh tidy datras haul data
#' @param species species code
#'
#' @export
#'
tidy_lengths <- function(hl, hh, species) {

  colnames(species) <- tolower(colnames(species))
  colnames(hl) <- tolower(colnames(hl))

  hl <-
    hl %>%
    distinct() %>%
    filter(!is.na(speccode), !is.na(lngtclass), !is.na(lngtcode)) %>%
    # EINAR - not sure if this is right, done here to test if any difference
    #         (did though not help with respect to discrepancy here below)
    #filter(specval == 1) %>%
    unite(id, year, quarter, ship, gear, haulno) %>%
    # only stations that are in the station table (north sea ibts)
    #  may be reduntant
    filter(id %in% hh$id) %>%
    # length class to cm
    mutate(length = ifelse(lngtcode %in% c("1"), lngtclass, lngtclass / 10),
           hlnoatlngt = hlnoatlngt * subfactor) %>%
    # get the data type and hauldur
    left_join(hh %>% select(id, datatype, hauldur)) %>%
    # catch per hour
    mutate(n = ifelse(datatype == "R",
                      hlnoatlngt * 60 / hauldur,
                      hlnoatlngt)) %>%
    # join with latin name
    left_join(species) %>%
    # select only needed columns
    select(id, latin, sex, length, n) %>%
    tbl_df()

  return(hl)

}