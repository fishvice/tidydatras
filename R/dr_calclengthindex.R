#' Download, tidy and process length data into index (including zero hauls)
#'
#' @param surveys like "NS-IBTS"
#' @param years like "2000:2022"
#' @param quarters like c(1,3)
#' @param quiet whether to prevent messages from being shown on screen (default is TRUE).
#' @param fromweb whether to get the data from Datras website (default is TRUE).
#' @param folder folder name if data is to be retrieved from existing file.
#'
#' @return a tibble
#' @export
#'

dr_calclengthindex <- function(surveys = "FR-CGFS", years = 2005:2022, quarters=4,
                               fromweb=FALSE, folder="C:/DATA/DATRAS/raw", quiet=TRUE) {

  # surveys = "FR-CGFS"; years = 2005:2022; quarters=4; fromweb=FALSE; folder="C:/DATA/DATRAS/raw";quiet=TRUE

  # flexfiles
  fl <-
    dr_getdata("FL", surveys, years, quarters, quiet, fromweb, folder) |>
    dr_tidy() |>                                   # Make tidy with column specifications
    dr_idunite(remove=FALSE) |>                    # Add identifier
    dplyr::left_join(reco, by=c("ship" = "code"))  # Add vesselname

  # haulfiles with flex added
  hh <-
    dr_getdata("HH", surveys, years, quarters, quiet, fromweb, folder) |>
    dr_tidy() |>                                       # Make tidy with column specifications
    dr_idunite(remove = FALSE) |>                      # Add identifier
    dplyr::left_join(reco, by=c("ship" = "code"))      # Add vesselname

  # length files
  hl <-
    dr_getdata("HL", surveys, years, quarters, quiet, fromweb, folder) |>
    dr_tidy() |>                                          # Make tidy with column specifications
    dr_idunite(remove = FALSE) |>                         # Add identifier
    dplyr::left_join(aphia_latin) |>                      # Add scientific name
    dplyr::left_join(asfis) |>                            # Add FAO species names and codes
    dplyr::left_join(reco, by = c("ship" = "code")) |>    # Add vesselname
    # dplyr::left_join(cgfs_corr) |>                        # Add CGFS correction factor
    dplyr::mutate(                                        # Use subsampling factor
      subfactor = ifelse(is.na(subfactor),1,subfactor),
      hlnoatlngt = hlnoatlngt * subfactor) |>
    dplyr::mutate(length = floor(length)) |>              # round to cm down
    dplyr::group_by(dplyr::across(c(-hlnoatlngt, -lngtclass, -nomeas))) |>  # group by all except lngthclass and numeric vars
    dplyr::summarise(hlnoatlngt = sum(hlnoatlngt), nomeas=sum(nomeas), .groups = "drop") |>  # sum over cms
    dr_calccpue(hh, fl)                                   # Calculated CPUE per hour and per sweptarea

    # dplyr::group_by(id, english_name, length) |>
  t <-
    hl |>
    dplyr::group_by(dplyr::across(c(-hlnoatlngt, -length, -nomeas))) |>  # group by all except length and numeric vars
    dplyr::summarise(
      number_per_hour   = sum(number_per_hour),
      number_per_km2_ds = sum(number_per_km2_ds),
      number_per_km2_ws = sum(number_per_km2_ws),
      .groups = "drop") |>

    dplyr::select(survey, quarter, id,
                  aphia, latin, species, english_name, family, order,
                  number_per_hour, number_per_km2_ws, number_per_km2_ds) |>
    dplyr::group_by(survey, quarter) |>
    tidyr::complete(id, tidyr::nesting(aphia, latin, species, english_name, family, order),
                    fill=list(number_per_hour=0, number_per_km2_ws=0, number_per_km2_ds=0)) |>
    dplyr::mutate(
      number_per_km2_ds = ifelse(id %in% fl$id, number_per_km2_ds, NA),
      number_per_km2_ws = ifelse(id %in% fl$id, number_per_km2_ws, NA)
    ) |>
    dplyr::arrange(id, english_name) |>
    dplyr::right_join(hh) |>
    dplyr::ungroup()

  return(t)

}


