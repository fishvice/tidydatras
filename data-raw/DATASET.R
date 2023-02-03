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

datras_field_types <-
  bind_rows(hh |> select(Field, DataType) |>   mutate(source = "HH"),
            hl |> select(Field, DataType) |>   mutate(source = "HL"),
            ca |> select(Field, DataType) |>   mutate(source = "CA")) |>
  distinct() |>
  mutate(type = case_when(DataType == "char" ~ "chr",
                          str_starts(DataType, "dec") ~ "dbl",
                          DataType == "int" ~ "int")) |>
  select(field = Field, type, record = source)

usethis::use_data(datras_field_types, overwrite = TRUE)
