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
  map_df(fil, read_rds) |> tidyices::dr_tidy() |>
  mutate(aphia = case_when(!is.na(valid_aphia) & valid_aphia != "0" ~ valid_aphia,
                           speccodetype == "W" ~ speccode,
                           TRUE ~ NA_character_),
         aphia = as.integer(aphia))
fil <- fs::dir_ls("~/stasi/datras/data-raw/datras", glob = "*_ca.rds")
ca <-
  map_df(fil, read_rds) |>
  tidyices::dr_tidy() |>
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

## trial with joining by SpecWoRMS - not used ----------------------------------
# this gives much fewer matches than the one above
worms <-
  icesVocab::getCodeList("SpecWoRMS") |>
  as_tibble() |>
  rename(aphia = Key,
         latin = Description) |>
  arrange(aphia)
sp |>
  left_join(worms) |>
  filter(!is.na(latin))
# check also out
icesVocab::findCodeType("species")
icesVocab::findCodeType("speccode")
# ... etc


##






pfix <- "https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSpecies?codename=aphia&code="
res <-
  tibble::tibble(aphia = sp$aphia[sp$aphia != "0"] |> na.omit() |> unique() |> as.integer()) |>
  dplyr::mutate(queary = paste0(pfix, aphia),
                raw = map(queary, icesDatras:::readDatras))
write_rds(res, "tmp_res.rds")
res |> filter(aphia == 0)
res[145,] |> mutate(parsed = map(raw, icesDatras:::parseDatras))
res |>
  slice(c(1:113, 115:2557)) |>
  mutate(parsed = map(raw, icesDatras:::parseDatras)) |>
  select(aphia_org = aphia, parsed) |>
  unnest(parsed) |>
  select(aphia_org, aphia, latin = latinname) |>
  distinct()
res |> slice(114) |> unnest(raw) |> select(aphia, raw) |> knitr::kable()




# shapes -----------------------------------------------------------------------
library(sf)
fao_area <- gisland::read_sf_ftp("FAO_AREAS_CWP_NOCOASTLINE")
usethis::use_data(fao_area)

ns_ibts_rf <- gisland::read_sf_ftp("NS_IBTS_RF")
usethis::use_data(ns_ibts_rf)
