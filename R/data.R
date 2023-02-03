#' Datras field types
#'
#' A table containing the field types of the datras variables
#'
#' @format ## `datras_field_types`
#' A data frame with 125 rows and 3 columns:
#' \describe{
#'   \item{field}{DATRAS variable name as returned by icesDatras::getDATRAS}
#'   \item{type}{The value type}
#'   \item{record}{The DATRAS data type ("HH": haul data, "HL": length-based data, "CA": age-based data}
#' }
#' @source <https://www.ices.dk/data/Documents/DATRAS/DATRAS_Field_descriptions_and_example_file_May2022.xlsx>
"datras_field_types"