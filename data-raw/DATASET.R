library(tidyverse)

url <- "https://www.ices.dk/data/Documents/DATRAS/DATRAS_Field_descriptions_and_example_file_May2022.xlsx"
tmp <- tempfile()
download.file(url, destfile = tmp)

hh <-
  readxl::read_excel(path  = tmp, sheet = "HH")
hl <-
  readxl::read_excel(path  = tmp, sheet = "HL")
ca <-
  readxl::read_excel(path  = tmp, sheet = "CA")

dr_coltypes <-
  bind_rows(hh |> select(Field, DataType) |>   mutate(source = "HH"),
            hl |> select(Field, DataType) |>   mutate(source = "HL"),
            ca |> select(Field, DataType) |>   mutate(source = "CA")) |>
  distinct() |>
  mutate(type = case_when(DataType == "char" ~ "chr",
                          str_starts(DataType, "dec") ~ "dbl",
                          DataType == "int" ~ "int")) |>
  select(field = Field, type, record = source)


usethis::use_data(dr_coltypes, overwrite = TRUE)

library(sf)
fao_area <- gisland::read_sf_ftp("FAO_AREAS_CWP_NOCOASTLINE")
usethis::use_data(fao_area)

ns_ibts_rf <- gisland::read_sf_ftp("NS_IBTS_RF")
usethis::use_data(ns_ibts_rf)
