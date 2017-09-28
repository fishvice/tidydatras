#' get_latin2
#'
#' Accesss ICES vocab for speces
#'
#' @return A dataframe with columns validaphia, latin and english species name.
#'
#' @export
#'
get_latin2 <- function() {

  icesVocab::getCodeList(code_type = "SpecWoRMS") %>%
    select(valid_aphia = Key,
           latin = Description,
           species = LongDescription)

}

#' get_latin
#'
#' Accesss ICES getSpecies webserver. Equivalent to what one would obtain using
#' https://datras.ices.dk/Data_products/qryspec.aspx
#'
#' @param df A dataframe that contains a columname "validaphia"
#'
#' @return A dataframe with columns validaphia, latin and english species name.
#'
#' @export
#'
get_latin <- function(df) {


  colnames(df) <- tolower(colnames(df))

  df <-
    df %>%
    select(valid_aphia) %>%
    distinct()

  out.valid <- list()

  for(i in 1:nrow(df)) {
    out.valid[[i]] <-
      paste0("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSpecies?codename=aphia&code=",
             df$valid_aphia[i]) %>%
      icesDatras:::readDatras() %>%
      icesDatras:::parseDatras()
  }


  ret <-
    bind_rows(out.valid) %>%
    as_tibble() %>%
    select(valid_aphia = aphia, latin = latinname, species = commonname) %>%
    # count the number of letters in the english name
    mutate(n = nchar(species)) %>%
    arrange(valid_aphia, n) %>%
    group_by(valid_aphia) %>%
    # select only the record where the english name is the shortest
    slice(1) %>%
    ungroup() %>%
    select(-n)

  return(ret)

}