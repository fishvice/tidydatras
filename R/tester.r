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

# source("../tidyices/R/get_datras.R")
# source("../tidyices/R/id_generator.R")
# source("../tidyices/R/calc_datras.R")

# ---------------------------------------------------------------------------------------------
# Selections
# ---------------------------------------------------------------------------------------------

surveys <- c("NS-IBTS", "FR-CGFS")
yrs <- 2020:2022
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
  dr_calccpue(hh)

ca <-
  dr_getdata("CA", surveys, yrs, qs, quiet=FALSE) %>%
  dr_tidy() %>%
  dr_idunite(., remove = FALSE) %>%
  left_join(aphia_latin) %>%
  dr_calccpue(hh)

