#' @export
cpue_per_length_per_haul <- function(st, le, source = "tidy") {

  if(source != "tidy") {
    stop("Not implemented yet")
  }

  # since ignoring sex, tally up by sex
  le <-
    le %>%
    group_by(id, latin, length) %>%
    summarise(n = sum(n)) %>%
    ungroup()
  st.grid <-
    # Fill "a station table" for each species:
    expand.grid(id = unique(st$id),
                latin = unique(le$latin),
                stringsAsFactors = FALSE) %>%
    tbl_df()
  # stations with "missing species"
  # "return all rows from st where there are not matching values in length,
  #    keeping just columns from st."
  st.zero <-
    anti_join(st.grid, le %>% select(id, latin) %>% distinct(), by = c("id", "latin")) %>%
    # create a zero record
    mutate(length = 0,
           n = 0)
  cpue <-
    st.grid %>%
    left_join(le, by = c("id", "latin")) %>%
    # add the zero stations
    bind_rows(st.zero) %>%
    filter(!is.na(latin))

  return(cpue)
}
