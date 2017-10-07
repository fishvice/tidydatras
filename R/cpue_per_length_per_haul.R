#' cpue_per_length_per_haul
#'
#' Here put text on what is done
#'
#' @param hh datras haul dataframe
#' @param hl datras length frequency dataframe
#' @param tidy If FALSE, first tidy the dataframes using functions tidy_hh and
#' tidy_hl (not implemented)
#'
#' @export
#'
cpue_per_length_per_haul <- function(hh, hl, tidy = TRUE) {

  if(!tidy) {
    stop("Not implemented yet")
  }

  # since ignoring sex, tally up by sex
  hl <-
    hl %>%
    dplyr::group_by(id, latin, length) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::ungroup()
  st.grid <-
    # Fill "a station table" for each species:
    expand.grid(id = unique(hh$id),
                latin = unique(hl$latin),
                stringsAsFactors = FALSE) %>%
    tbl_df()
  # stations with "missing species"
  # "return all rows from st where there are not matching values in length,
  #    keeping just columns from st."
  st.zero <-
    dplyr::anti_join(st.grid, le %>%
                       dplyr::select(id, latin) %>%
                       dplyr::distinct(),
                     by = c("id", "latin")) %>%
    # create a zero record
    dplyr::mutate(length = 0,
                  n = 0)
  cpue <-
    st.grid %>%
    dplyr::left_join(hl, by = c("id", "latin")) %>%
    # add the zero stations
    dplyr::bind_rows(st.zero) %>%
    dplyr::filter(!is.na(latin))

  return(cpue)
}
