# functions


get.spp.table <- function(){
  library(RODBC)
  library(tidyverse)
  
  con.Common <- odbcDriverConnect(connection = "Driver=ODBC Driver 18 for SQL Server;Server=161.55.235.187; Database=SWFSCCommon;Uid=; Pwd=; trusted_connection=yes; Encrypt=Optional")
  
  # Common.tbls <- sqlTables(con.Common)
  # Common.tbls.names <- Common.tbls$TABLE_NAME[grep(pattern = "tbl", 
  #                                                Common.tbls$TABLE_NAME)]
  spp.tbl <- sqlQuery(con.Common, 'select * from tblSpecies') %>%
    select(SpName, CommonName, Genus, Species)
  
  odbcClose(con.Common)
  
  return(spp.tbl)
}

extract_take_data <- function(years, run.date = Sys.Date()){
  library(RODBC)
  library(tidyverse)
  
  #out.file.name <- paste0('data/take_report_', run.date, '.csv')
  
  # load a couple databases through ODBC - new as of 2024-10-31 Tomo Eguchi
  con.boat <- odbcDriverConnect(connection = "Driver=ODBC Driver 18 for SQL Server;Server=161.55.235.187; Database=CetaceanHealth;Uid=; Pwd=; trusted_connection=yes; Encrypt=Optional")
  # LIMS <- odbcDriverConnect(connection = "Driver=ODBC Driver 18 for SQL Server;Server=161.55.235.187; Database=LIMS;Uid=; Pwd=; trusted_connection=yes; Encrypt=Optional")
  # 
  #boat.tbls <- sqlTables(con.boat)
  #LIMS.tbls <- sqlTables(LIMS)
  
  # TO FIND ALL TABLE NAMES:
  # boat.tbls.names <- boat.tbls$TABLE_NAME[grep(pattern = 'tbl',
  #                                              boat.tbls$TABLE_NAME)]
  # 
  # boat.vw.names <- boat.tbls$TABLE_NAME[grep(pattern = 'vw',
  #                                            boat.tbls$TABLE_NAME)]
  
  boat.sighting <- sqlQuery(con.boat, 'select * from vwSighting')
  boat.UAS <- sqlQuery(con.boat, 'select * from vwUASFlight')
  boat.survey <- sqlQuery(con.boat, 'select * from vwSurvey')
  
  boat.survey %>%
    select(SurveyID, SurveyDate, SurveyNumber, PurposeName) %>%
    mutate(year = lubridate::year(SurveyDate)) %>%
    filter(year %in% years) -> survey.data
  
  boat.sighting %>% 
    select(SurveyID, SightingID, 
           SightingTime, Latitude, Longitude, CommonName,
           MinEstimate, BestEstimate, MaxEstimate, 
           Calves, MinCalvesEstimate, BestCalvesEstimate,
           MaxCalvesEstimate) %>%
    filter(year(SightingTime) %in% years) -> sightings.data
  
  sightings.data[sightings.data$Calves == 0, c("MinCalvesEstimate",
                                               "BestCalvesEstimate",
                                               "MaxCalvesEstimate")] <- 0
  
  sightings.data %>%
    group_by(CommonName) %>%
    summarize(Best = sum(BestEstimate, na.rm = T),
              Best.calves = sum(BestCalvesEstimate),
              n.sightings = n()) -> sightings.summary
  
  boat.UAS %>%
    select(SurveyID, SightingID, 
           TargetCount, NonTargetCount) %>%
    left_join(sightings.data,
              by = c("SurveyID", "SightingID")) %>%
    filter(!is.na(CommonName)) %>%
    left_join(survey.data, by = "SurveyID") -> UAS.data
  
  UAS.data %>%
    group_by(SightingID) %>%
    summarise(SightingTime = first(SightingTime),
              Target = max(TargetCount, na.rm = T),
              CommonName = first(CommonName),
              Best = max(BestEstimate, na.rm = T),
              BestCalves = max(BestCalvesEstimate, na.rm = T),
              Purpose = first(PurposeName))-> UAS.summary

  # if (save.file){  
  #   write.csv(sightings.summary, 
  #             file = paste0("data/sightings_take_",
  #                           Sys.Date(), ".csv"))
  #   write.csv(UAS.summary, 
  #             file = paste0("data/UAS_take_",
  #                           Sys.Date(), ".csv"))
  # }  
  
  return(list(survey.data = survey.data,
              sightings.data = sightings.data,
              sightings.summary = sightings.summary,
              UAS.data = UAS.data,
              UAS.summary = UAS.summary))
  
  odbcCloseAll()
}


