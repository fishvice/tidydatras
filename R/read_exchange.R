#' read_exchange
#'
#' @param fname The name of the exchange file (usually a csv-file)
#'
#' @return a list
#' @export
#'

read_exchange <- function(fname) {

  x <- readLines(fname)

  type <- stringr::str_sub(x, 1, 2)
  h <- stringr::str_detect(type, "Re")
  headers <- stringr::str_split(tolower(x[h]), ",")
  names(headers) <- unique(type)[-1]

  res <- vector("list", length(headers))
  names(res) <- names(headers)

  for(i in 1:length(headers)) {

    j <- stringr::str_detect(type, names(headers)[i])
    #res[[i]] <- as_tibble(matrix(unlist(stringr::str_split(x[j], ",")), byrow = TRUE, ncol = length(headers[[i]])))
    x2 <- matrix(unlist(stringr::str_split(x[j], ",")), byrow = TRUE, ncol = length(headers[[i]]))
    z <- x2 %in% c("-9", "-9.0", "-9.00", "-9.000", "-9.0000")
    x2[z] <- "NA"
    res[[i]] <- tibble::as_tibble(x2)
    colnames(res[[i]]) <- headers[[i]]

  }

  # correct fields
  # HH
  int <-
    fields %>%
    dplyr::filter(datatype == "int",
                  fields %in% colnames(res$HH))
  dbl <-
    fields %>%
    dplyr::filter(!datatype %in% c("int", "char"),
                  fields %in% colnames(res$HH))
  res$HH <-
    res$HH %>%
    purrr::map_at(int$fields, as.integer) %>%
    purrr::map_at(dbl$fields, as.numeric) %>%
    dplyr::bind_cols()
  # HL
  int <-
    fields %>%
    dplyr::filter(datatype == "int",
                  fields %in% colnames(res$HL))
  dbl <-
    fields %>%
    dplyr::filter(!datatype %in% c("int", "char"),
                  fields %in% colnames(res$HL))
  res$HL <-
    res$HL %>%
    purrr::map_at(int$fields, as.integer) %>%
    purrr::map_at(dbl$fields, as.numeric) %>%
    dplyr::bind_cols()
  # CA
  int <-
    fields %>%
    dplyr::filter(datatype == "int",
                  fields %in% colnames(res$CA))
  dbl <-
    fields %>%
    dplyr::filter(!datatype %in% c("int", "char"),
                  fields %in% colnames(res$CA))
  res$CA <-
    res$CA %>%
    purrr::map_at(int$fields, as.integer) %>%
    purrr::map_at(dbl$fields, as.numeric) %>%
    dplyr::bind_cols()

  # time stuff
  res$HH <-
    res$HH %>%
    dplyr::mutate(datim.shot = paste(paste(year, month, day, sep = "-"),
                                     paste(stringr::str_sub(timeshot, 1, 2),
                                           stringr::str_sub(timeshot, 3, 4),
                                           sep = ":")),
                  datim.shot = lubridate::ymd_hm(datim.shot),
                  datim.haul = datim.shot + hauldur * 60)   #Default is seconds

  return(res)

}
