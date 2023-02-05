#' Title
#'
#' @param lon xxx
#' @param lat xxx
#' @param s xxx
#' @param variable xxx
#'
#' @return A vector
#' @export
#'
dr_geoinside <- function(lon, lat, s, variable) {
  s <-
    s |>
    sf::st_make_valid() |>
    dplyr::select( {{variable}} ) |>
    dplyr::distinct() |>
    tidyr::drop_na()
  var <-
    tibble::tibble(lon = lon, lat = lat) |>
    sf::st_as_sf(coords = c("lon", "lat"),
                 crs = 4326) |>
    sf::st_transform(crs = sf::st_crs(s)) |>
    sf::st_join(s) |>
    sf::st_drop_geometry() |>
    dplyr::pull( {{variable}} )
  return(var)

}