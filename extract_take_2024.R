# Extracts take numbers and create a table for reporting
# 

# 2025-02-06

rm(list=ls())
library(tidyverse)
library(readxl)

source("CHLHP_PermitReport_fcns.R")

# Get data for 2024-01-01 to 2024-06-30. Data for July onwards are
# in the small boat database
data.Jan_Jun05 <- read_xlsx(path = "data/ALEXAK_Field Data.xlsx") %>%
  select(Date, Time, Lat, Long, Species,
         "GroupSizeBest", "CalvesPresent",
         "UAS_GroupSize") %>%
  filter(!is.na(Species)) %>%
  filter(Date < as.Date("2024-07-01"),
         Date > as.Date("2023-12-31"))

# Create a look up table for species code and common names
Spp.table <- data.frame(Species = c("E rob", 
                                    "G gri", 
                                    "T tru", 
                                    "D cap", 
                                    "B mus", 
                                    "M nov",
                                    "Z cal"),
                        CommonName = c("Gray whale",
                                       "Risso's dolphin",
                                       "Bottlenose dolphin",
                                       "Long-beaked common dolphin",
                                       "Blue whale",
                                       "Humpback whale",
                                       "California sea lion"))

# combine the data and species table and summarize how many were sighted
# Combine also the date and time
data.Jan_Jun05 %>%
  left_join(Spp.table, by = "Species")  %>%
  select(-c("Species")) %>%
  mutate(hr = floor(Time/100),
         min = Time - (floor(Time/100)*100),
         new.time = paste(hr, ifelse(min<10, paste0("0", min), min), sep = ":"))  %>% 
  mutate(Date_Time = as.POSIXct(paste(Date, new.time, sep = " "),
                                tz = "America/Los_Angeles")) %>%
  select(-c("Date", "Time", "hr", "min", "new.time")) -> data.Jan_Jun05.all

data.Jan_Jun05.all %>%
  group_by(CommonName) %>%
  summarize(Best = sum(GroupSizeBest, na.rm = T),
            Best.calves = NA,
            n.sightings = n()) -> data.Jan_Jun05.summary 

# Extract the same information from the small boat database
data.Jun12_Dec <- extract_take_data(2024)

# sightings
data.Jan_Jun05.summary %>%
  full_join(data.Jun12_Dec$sightings.summary, 
            by = "CommonName") %>%
  transmute(CommonName = CommonName,
            Best = ifelse(is.na(Best.x), 0, Best.x) + 
              ifelse(is.na(Best.y), 0, Best.y),
            Best.calves = ifelse(is.na(Best.calves.x), 0, 
                                 Best.calves.x) + 
              ifelse(is.na(Best.calves.y), 0, Best.calves.y),
            n.sightings = ifelse(is.na(n.sightings.x), 0, 
                                 n.sightings.x) + 
              ifelse(is.na(n.sightings.y), 0, 
                     n.sightings.y)) -> summary.all

write.csv(summary.all, 
          file = paste0("data/sightings_take_numbers_", Sys.Date(), ".csv"))
write.csv(data.Jun12_Dec$sightings.data,
          file = paste0("data/sightings_Jun_Dec_", Sys.Date(), ".csv"))

# UAS flights
# Jan flights are in a separate summarized file
Jan.flights <- read_xlsx("data/2024_CI_UAS animal totals.xlsx") %>%
  left_join(Spp.table, by = "Species") %>%
  group_by(CommonName) %>%
  summarize(Best = sum(n),
            #Best.calves = NA,
            n.sightings = n())

data.Jan_Jun05.all %>%
  filter(Date_Time > as.Date("2024-01-31 23:59:59")) %>%
  group_by(CommonName) %>%
  summarize(Best = sum(UAS_GroupSize, na.rm = T),
            #Best.calves = NA,
            n.sightings = n()) -> data.Feb_Jun05.UAS.summary 

Jan.flights %>% 
  full_join(data.Feb_Jun05.UAS.summary, 
                          by = "CommonName") %>% #-> tmp
  replace_na(replace = list(Best.x = 0, Best.y = 0,
                            #Best.calves.x = 0, Best.calves.y = 0,
                            n.sightings.x = 0, n.sightings.y = 0)) %>% # -> tmp
  transmute(CommonName = CommonName, 
            Best = Best.x + Best.y,
            n.sightings = n.sightings.x + n.sightings.y) -> data.Jan_Jun05.UAS.summary

data.Jun12_Dec$UAS.summary %>%
  group_by(CommonName) %>%
  summarize(Best = sum(Target),
            n.sightings = n()) -> data.Jun12_Dec.UAS.summary

data.Jan_Jun05.UAS.summary %>%
  full_join(data.Jun12_Dec.UAS.summary, by = "CommonName") %>% #-> tmp
  replace_na(replace = list(Best.x = 0, Best.y = 0,
                            #Best.calves.x = 0, Best.calves.y = 0,
                            n.sightings.x = 0, n.sightings.y = 0)) %>% # -> tmp
  transmute(CommonName = CommonName, 
            Best = Best.x + Best.y,
            n.sightings = n.sightings.x + n.sightings.y) -> summary.all.UAS

write.csv(summary.all.UAS, 
          file = paste0("data/UAS_take_numbers_", Sys.Date(), ".csv"))
