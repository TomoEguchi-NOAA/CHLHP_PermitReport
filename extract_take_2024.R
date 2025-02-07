# Extracts take numbers and create a table for reporting
# 

# 2025-02-06

rm(list=ls())
library(tidyverse)
library(readxl)

source("CHLHP_PermitReport_fcns.R")

data.Jan_Jun <- read_xlsx(path = "data/ALEXAK_Field Data 2024-01-09 to 2024-06-13.xlsx") %>%
  select(Date, Time, Lat, Long, Species,
         "GroupSizeBest", "CalvesPresent",
         "UAS_GroupSize") %>%
  filter(!is.na(Species))

Spp.table <- data.frame(Species = c("E rob", "G gri", "T tru", "D cap", "B mus", "M nov"),
                        CommonName = c("Gray whale",
                                       "Risso's dolphin",
                                       "Bottlenose dolphin",
                                       "Long-beaked common dolphin",
                                       "Blue whale",
                                       "Humpback whale"))

data.Jan_Jun %>%
  left_join(Spp.table) %>%
  group_by(CommonName) %>%
  select(-c("Species")) %>%
  summarize(Best = sum(GroupSizeBest, na.rm = T),
            Best.calves = NA,
            n.sightings = n()) -> data.Jan_Jun.summary 

data.Jun_Dec <- extract_take_data(2024, save.file = T)

data.Jan_Jun.summary %>%
  full_join(data.Jun_Dec$summary, by = "CommonName") %>%
  transmute(CommonName = CommonName,
            Best = ifelse(is.na(Best.x), 0, Best.x) + 
              ifelse(is.na(Best.y), 0, Best.y),
            Best.calves = ifelse(is.na(Best.calves.x), 0, Best.calves.x) + 
              ifelse(is.na(Best.calves.y), 0, Best.calves.y),
            n.sightings = ifelse(is.na(n.sightings.x), 0, n.sightings.x) + 
              ifelse(is.na(n.sightings.y), 0, n.sightings.y)) -> data.all


write.csv(data.all, file = "data/take_numbers.csv")
