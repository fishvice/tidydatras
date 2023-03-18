library(tidyverse)

# column class -----------------------------------------------------------------
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


# Species joins ----------------------------------------------------------------

## get all combination of speccodetype, speccode and valid_aphia --------------
fil <- fs::dir_ls("~/stasi/datras/data-raw/datras", glob = "*_hl.rds")
hl <-
  map_df(fil, read_rds) |> tidydatras::dr_tidy() |>
  mutate(aphia = case_when(!is.na(valid_aphia) & valid_aphia != "0" ~ valid_aphia,
                           speccodetype == "W" ~ speccode,
                           TRUE ~ NA_character_),
         aphia = as.integer(aphia))
fil <- fs::dir_ls("~/stasi/datras/data-raw/datras", glob = "*_ca.rds")
ca <-
  map_df(fil, read_rds) |>
  tidydatras::dr_tidy() |>
  mutate(aphia = case_when(!is.na(valid_aphia) & valid_aphia != "0" ~ valid_aphia,
                           speccodetype == "W" ~ speccode,
                           TRUE ~ NA_character_),
         aphia = as.integer(aphia))
sp <-
  bind_rows(hl |> select(speccodetype, speccode, valid_aphia, aphia) |> distinct(),
            ca |> select(speccodetype, speccode, valid_aphia, aphia) |> distinct()) |>
  distinct() |>
  rename(type = speccodetype, code = speccode)
# tests
sp |> filter(is.na(aphia))
sp |> filter(valid_aphia == "0")

##   there is a repeat of aphia, may be ok but would be Kosher to check why
sp$aphia |> na.omit() |> unique() |> length()
sp |>
  group_by(aphia) |>
  mutate(n.rep = n()) |>
  filter(n.rep > 1) |>
  arrange(aphia)

## trial with DATRASWebService -------------------------------------------------

pfix <- "https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSpecies?codename=aphia&code="
res <-
  tibble::tibble(aphia = sp$aphia |> na.omit() |> unique()) |>
  dplyr::mutate(q = paste0(pfix, aphia),
                raw = map(q, icesDatras:::readDatras))
# write_rds(res, "tmp_res.rds")
dws <-
  res |>
  # this record won't parse
  slice(-114) |>
  mutate(parsed = map(raw, icesDatras:::parseDatras)) |>
  select(parsed) |>
  unnest(parsed) |>
  select(aphia, latin = latinname) |>
  distinct() |>
  # the record 114 added again, manual lookup on ices webpage
  add_row(aphia = 127178, latin = "Coregonus albula")

# tests
dws |> count(aphia) |> filter(n > 1)
dws |> count(latin) |> filter(n > 1)

# tests
sp |>
  left_join(dws) |>
  filter(is.na(latin))
hl |>
  left_join(dws) |>
  filter(is.na(latin)) |>
  pull(speccode) |>
  unique()
ca |>
  left_join(dws) |>
  filter(is.na(latin)) |>
  pull(speccode) |>
  unique()

aphia_latin <- dws
usethis::use_data(aphia_latin)

## Vessel codes https://vocab.ices.dk/?ref=315 -------------------------------------------------

reco <-
  read.csv("data-raw/RECO_Export_09-26-2023-06-26-45.csv", colClasses = "character")  %>%
  dplyr::rename_all(tolower) %>%
  dplyr::select(
    code,
    vessel = description) %>%
  dplyr::mutate(
    vessel = ifelse(code=="29JR", "JOSE RIOJA", vessel)
  )
save(reco, file="data/reco.rda")
usethis::use_data(reco, overwrite=TRUE)

## ICES rectangle weights for IBTS index calculation -------------------------------------------------
## Annex 3 in https://www.ices.dk/data/Documents/DATRAS/Indices_Calculation_Steps_IBTS.pdf

ibts_rect <-
  read.csv(file="data-raw/ibts_rects.csv")  %>%
  dplyr::rename_all(tolower)

save(ibts_rect, file="data/ibts_rect.rda")
usethis::use_data(ibts_rect, overwrite=TRUE)
