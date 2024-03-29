#' Datras variable types
#'
#' A table containing the variable (field) types of the datras data
#'
#' A data frame with 125 rows and 3 columns:
#' \describe{
#'   \item{field}{DATRAS variable name as returned by icesDatras::getDATRAS}
#'   \item{type}{The value type}
#'   \item{record}{The DATRAS data type - "HH": haul data, "HL": length-based data, "CA": age-based data}
#' }
#' @source <https://www.ices.dk/data/Documents/DATRAS/DATRAS_Field_descriptions_and_example_file_May2022.xlsx>
"dr_coltypes"

#' Latin names for aphia code
#'
#' A data frame with 2011 rows and 2 columns:
#' \describe{
#'   \item{aphia}{aphia code}
#'   \item{latin}{Latin name}
#' }
#'
"aphia_latin"

#' FAO species codes
#'
#' A data frame with 13417 rows and 5 columns:
#' \describe{
#'   \item{species}{FAO 3-letter code}
#'   \item{latin}{Latin name}
#'   \item{english_name}{English name}
#'   \item{family}{Family}
#'   \item{order}{Order}
#' }
#' @source <https://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip>
"asfis"

#' RECO vessel codes
#'
#' A data frame with 12962 rows and 2 columns:
#' \describe{
#'   \item{code}{Vessel code}
#'   \item{vessel}{Vessel name}
#' }
#' @source <https://vocab.ices.dk/?ref=315>
"reco"

#' IBTS rect data
#'
#' A data frame with 211 rows and 2 columns:
#' \describe{
#'   \item{statrec}{ICES Statistical Rectangle}
#'   \item{weight}{Weight of rectangle in index calculation}
#' }
#' @source <https://www.ices.dk/data/Documents/DATRAS/Indices_Calculation_Steps_IBTS.pdf>
"ibts_rect"
