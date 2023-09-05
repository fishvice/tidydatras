#' Set the datras data types
#'
#' @param d a datras tibble
#'
#' @return A tibble containing tibble with appropriate data types
#' @export
#'
dr_settypes <- function(d) {

  key_int <- dr_coltypes |> dplyr::filter(type == "int") |> dplyr::pull(field) |> unique()
  key_dbl <- dr_coltypes |> dplyr::filter(type == "dbl") |> dplyr::pull(field) |> unique()

  d <-
    d %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),    as.character))  |>
    dplyr::mutate(dplyr::across(dplyr::any_of(key_int), as.integer))  |>
    dplyr::mutate(dplyr::across(dplyr::any_of(key_dbl), as.numeric))

  return(d)

}
