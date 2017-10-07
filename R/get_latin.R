#' get_latin2
#'
#' Accesss ICES vocab for species
#'
#' @return A dataframe with columns valid_aphia, latin and species (english) name.
#'
get_latin2 <- function() {

  icesVocab::getCodeList(code_type = "SpecWoRMS") %>%
    dplyr::select(valid_aphia = Key,
           latin = Description,
           species = LongDescription)

}

#' get_latin
#'
#' Accesss ICES getSpecies webserver. Equivalent to what one would obtain using
#' https://datras.ices.dk/Data_products/qryspec.aspx
#'
#' @param df A dataframe that contains a columname "valid_aphia"
#'
#' @return A dataframe with columns validaphia, latin and english species name.
#' If no object is passed, then read species code from icesVocab
#'
#' @export
#'
get_latin <- function(df) {

  if(missing(df)) return(get_latin2())


  colnames(df) <- tolower(colnames(df))

  df <-
    df %>%
    dplyr::select(valid_aphia) %>%
    dplyr::distinct()

  out.valid <- list()

  for(i in 1:nrow(df)) {
    out.valid[[i]] <-
      paste0("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSpecies?codename=aphia&code=",
             df$valid_aphia[i]) %>%
      icesDatras:::readDatras() %>%
      icesDatras:::parseDatras()
  }


  ret <-
    dplyr::bind_rows(out.valid) %>%
    tibble::as_tibble() %>%
    dplyr::select(valid_aphia = aphia, latin = latinname, species = commonname) %>%
    # count the number of letters in the english name
    dplyr::mutate(n = nchar(species)) %>%
    dplyr::arrange(valid_aphia, n) %>%
    dplyr::group_by(valid_aphia) %>%
    # select only the record where the english name is the shortest
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n)

  return(ret)

}