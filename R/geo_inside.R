#' @title Spatial location of datapoints
#'
#' @description Returns spatial attributes of coordinates (longitude and latitude)
#' given spatial polygons.
#'
#' @param lon A numerical vector
#' @param lat A numerical vector
#' @param map Normally a spatialPolygonDataFrame (sf or sp)
#' @param variable The variable name, stored in the map attribute table to be
#' returned. If missing (default) only boolean vector is returned indicating
#' if coordinates are inside or outside any region.
#'
#' @export
#'
geo_inside <- function(lon, lat, map, variable) {

  if("sf" %in% class(map)) {
    variable <- rlang::enquo(variable)
    # slow if lots of lons and lats or big sf - needs improvement
    pt <-
      tibble::data_frame(x = lon,
                         y = lat) %>%
      sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(map))

    # slower
    # return(pt %>% sf::st_join(map) %>% dplyr::pull(!!variable))

    # faster
    i <- pt %>% st_within(map) %>% as.numeric()
    y <- map %>% pull(!!variable)
    return(y[i])

  }

  # Here we could try to pass the stuff to geo_inside2
  # The problem is that it does not work if we have holes
  if(class(map) == "data.frame") {

    message("The map is a data.frame, tryp geo_inside2")
    return(NULL)

  }

  # deal with missing lons and lats
  df <- data.frame(long = lon, lat = lat)
  df$id <- 1:nrow(df)
  df.redux <- df[!is.na(df$lon),]

  x <- sp::SpatialPoints(data.frame(long = df.redux$long, lat = df.redux$lat))
  sp::proj4string(x) <- sp::proj4string(map)

  x <- sp::over(x, map)

  x$id <- df.redux$id

  x <-
    data.frame(id = df$id) %>%
    dplyr::left_join(x)

  if(!missing(variable)) {
    if(is.factor(x[, variable])) {
      return(as.character(x[,variable]))
    } else {
      return(x[, variable])
    }
  } else {

    # cumbersome stuff - clarify
    if(is.null(dim(x))) {
      x <- as.logical(x)
      x <- ifelse(is.na(x), FALSE, x)
      return(x)
    } else {
      return(!is.na(x[,1]))
    }
  }
}

