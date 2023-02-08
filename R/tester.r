# -------------------------------------------------------------------------------
# Tester
#
# 14/08/2018 first coding
# 15/03/2019 coding during HAWG
# 19/07/2019 small change in mutate_at
# 02/02/2023 full recoding of the original codes from Einar
#
# -------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(icesDatras)  # install.packages("icesDatras")

library(tidyices) # remotes::install_github("fishvice/tidyices", ref = "dev")

source("../tidyices/R/get_datras.R")
source("../tidyices/R/id_generator.R")
source("../tidyices/R/calc_datras.R")

afsis <- read_rds(file = "data/afsis.rds") %>% dplyr::select(latin, species, english_name, dutch_name)

# ---------------------------------------------------------------------------------------------
# Selections
# ---------------------------------------------------------------------------------------------

surveys <- c("NS-IBTS", "FR-CGFS")
yrs <- 1990:2022
qs  <- 1:4

hh <-
  dr_getdata("HH", surveys, yrs, qs, quiet=FALSE) %>%
  dr_tidy() %>%
  dr_idunite(., remove = FALSE)

hl <-
  dr_getdata("HL", surveys, yrs, qs, quiet=FALSE) %>%
  dr_tidy() %>%
  dr_idunite(., remove = FALSE) %>%
  left_join(aphia_latin) %>%
  left_join(afsis) %>%
  dr_calccpue(hh)

ca <-
  dr_getdata("CA", surveys, yrs, qs, quiet=FALSE) %>%
  dr_tidy() %>%
  dr_idunite(., remove = FALSE) %>%
  left_join(aphia_latin) %>%
  left_join(afsis) %>%
  dr_calccpue(hh)

#construct filenames
tt <-
  paste(
    paste(unique(hh$survey), collapse ="_"),
    paste(min(hh$year, na.rm=TRUE), max(hh$year, na.rm=TRUE), sep="-")
  ) %>%
  as.character()

datapath <- "D:/ICES/DATRAS"

# save files
hh %>% write_rds(file = paste0(datapath, "/tidy/", tt, "_hh.rds"))
hl %>% write_rds(file = paste0(datapath, "/tidy/", tt, "_hl.rds"))
ca %>% write_rds(file = paste0(datapath, "/tidy/", tt, "_ca.rds"))

# afsis <-
#   readxl::read_excel("C:/TEMP/ASFIS_sp_2022_REV1.xlsx")  %>%
#   dplyr::rename_all(tolower) %>%
#   dplyr::select(
#     species = "3a_code",
#     latin = scientific_name,
#     english_name,
#     family,
#     order
#   ) %>%
#   arrange(species)



  mutate(aphia = as.character(aphia))
