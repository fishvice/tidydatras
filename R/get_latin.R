#' get_latin
#'
#' @param df A dataframe that contains a columname "validaphia"
#'
#' @return A datafram with columns validaphia, latin and species name
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
    # count the number of words in the english name
    mutate(n = nchar(species)) %>%
    arrange(valid_aphia, n) %>%
    group_by(valid_aphia) %>%
    # select only the record where the english name is the shortes
    slice(1) %>%
    ungroup() %>%
    select(-n)

  return(ret)

}