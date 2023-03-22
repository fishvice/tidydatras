#' Download and tidy length data
#'
#' @param survey_quarter like "NS-IBTS_3"
#' @param years like "2000:2022"
#'
#' @return a tibble
#' @export
#'
dr_doodle <- function(survey_quarter = "FR-CGFS_4", years = 2005:2022) {

  s <- stringr::str_sub(survey_quarter, 1, nchar(survey_quarter) - 2)
  q <- stringr::str_sub(survey_quarter, nchar(survey_quarter)) |> as.integer()

  HH <-
    dr_getdata("HH", s, years, q)
  hh <-
    HH |>
    dr_tidy() %>%                                # Make tidy with column specifications
    dr_idunite(., remove = FALSE) %>%            # Add identifier
    dplyr::left_join(reco, by=c("ship" = "code"))  # Add vesselname

  HL <-
    tidyices::dr_getdata("HL", s, years, q)
  hl <-
    HL |>
    dr_tidy() %>%                               # Make tidy with column specifications
    dr_idunite(., remove = FALSE) |>            # Add identifier
    dplyr::left_join(aphia_latin) %>%                  # Add scientific name
    dplyr::left_join(asfis) %>%                        # Add FAO species names and codes
    dplyr::left_join(reco, by = c("ship" = "code")) %>%    # Add vesselname
    dplyr::left_join(cgfs_corr) %>%
    dplyr::mutate(factor = ifelse(is.na(factor),1,factor), hlnoatlngt = hlnoatlngt * factor)

  hl <-
    hl |>
    dr_calccpue(hh) |>                          # Calculated CPUE per hour
    #                                           # fill in the zero's
    dplyr::mutate(length = floor(length)) |>
    dplyr::group_by(id, english_name, length) |>
    dplyr::summarise(cpue_number_per_hour = mean(cpue_number_per_hour),
              .groups = "drop") |>
    tidyr::spread(length, cpue_number_per_hour, fill = 0) |>
    tidyr::gather(key = length,
           value = n,
           -c(id, english_name),
           convert = TRUE) |>
    dplyr::group_by(english_name) |>
    dplyr::filter(length <= max(length[n != 0])) |>
    dplyr::ungroup()

  # get some stations details that may be of interst to use downstream
  res <-
    hh |>
    dplyr::left_join(hl)

  return(res)

}
