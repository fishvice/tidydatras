library(tidyverse)
library(tidydatras)

# d <- tidydatras::dr_doodle(survey_quarter = "FR-CGFS_4",
#                 years = 2020:2022)

d <- readxl::read_excel(file.path("data","test.xlsx"))

d %>% group_by(survey, haul, species) %>% summarise(n=sum(n))

d %>%
  group_by(survey) %>%
  tidyr::complete(nesting(id, haul), nesting(species, latin), length, fill=list(n=0))  %>%
  group_by(survey, haul, species) %>%
  # View()

  summarise(n=sum(n), .groups="drop") %>%
  tidyr::pivot_wider(names_from = species, values_from = n) %>%
  View()


