# functions


extract_take_data <- function(years, save.files = T, run.date = Sys.Date(), save.file = F){
  library(RODBC)
  library(tidyverse)
  
  out.file.name <- paste0('data/take_report_', run.date, '.csv')
  
  # load a couple databases through ODBC - new as of 2024-10-31 Tomo Eguchi
  con.boat <- odbcDriverConnect(connection = "Driver=ODBC Driver 18 for SQL Server;Server=161.55.235.187; Database=CetaceanHealth;Uid=; Pwd=; trusted_connection=yes; Encrypt=Optional")
  # LIMS <- odbcDriverConnect(connection = "Driver=ODBC Driver 18 for SQL Server;Server=161.55.235.187; Database=LIMS;Uid=; Pwd=; trusted_connection=yes; Encrypt=Optional")
  # 
  boat.tbls <- sqlTables(con.boat)
  #LIMS.tbls <- sqlTables(LIMS)
  
  # TO FIND ALL TABLE NAMES:
  boat.tbls.names <- boat.tbls$TABLE_NAME[grep(pattern = 'tbl',
                                               boat.tbls$TABLE_NAME)]
  
  boat.vw.names <- boat.tbls$TABLE_NAME[grep(pattern = 'vw',
                                             boat.tbls$TABLE_NAME)]
  
  # LIMS.tbls.names <- LIMS.tbls$TABLE_NAME[grep(pattern = 'tbl_',
  #                                              LIMS.tbls$TABLE_NAME)]
  # 
  # LIMS.vw.names <- LIMS.tbls$TABLE_NAME[grep(pattern = 'vw_',
  #                                            LIMS.tbls$TABLE_NAME)]
  
  # vw tables are useful because code is translated, e.g., species
  boat.sighting <- sqlQuery(con.boat, 'select * from vwSighting')
  boat.UAS <- sqlQuery(con.boat, 'select * from vwUASFlight')
  boat.survey <- sqlQuery(con.boat, 'select * from vwSurvey')
  
  boat.sighting %>% 
    select(SightingTime, Latitude, Longitude, CommonName,
           MinEstimate, BestEstimate, MaxEstimate, 
           MinCalvesEstimate, BestCalvesEstimate, MaxCalvesEstimate, Comments) %>%
    filter(year(SightingTime) %in% years) -> sighting.data
  
  sighting.data %>%
    group_by(CommonName) %>%
    summarize(Best = sum(BestEstimate, na.rm = T),
              Best.calves = sum(BestCalvesEstimate, na.rm = T),
              n.sightings = n()) -> summary.data
  
  if (save.file)
    write.csv(summary.data, file = out.file.name)
  
  return(list(sightings = sighting.data,
              summary = summary.data))
  
  odbcCloseAll()
}


