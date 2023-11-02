#This script takes AZ Rx permit data and processes it so it is ready to be used in the Rx database

library(tidyverse)


### BRING IN THE DATA ----

raw <- read_csv("in/AZ_ADEQ_Prescribed_Fire.csv") %>%
  rename(DATE = "Ignition Date") %>%
  rename(COMPLETED_ACRES = "Acres Burned") %>%
  rename(BurnNumber = "Burn Number")


### PROCESS ----

process <- raw 

#lat, lon
process$Location <- gsub("[()]", "", process$Location) #remove parentheses
process <- process%>%
  separate_wider_delim("Location", ";", names = c("LAT_PERMIT", "LON_PERMIT")) %>%
  mutate(LON_PERMIT = as.numeric(LON_PERMIT)) %>%
  mutate(LAT_PERMIT = as.numeric(LAT_PERMIT))

#entity requesting, burn status, burn type
process <- process %>%
  mutate(ENTITY_REQUESTING = paste(Agency, Office, District, sep = ", ")) %>%

  mutate(BURN_STATUS = case_when(COMPLETED_ACRES == 0 ~ "Incomplete",
                                 COMPLETED_ACRES > 0 ~ "Complete",
                                 .default = "Unknown")) %>%
  mutate(BURNTYPE_REPORTED = BurnNumber) %>%
  mutate(BURNTYPE_REPORTED = case_when(str_ends(BurnNumber, "B") ~ "Broadcast",
                                       str_ends(BurnNumber, "P") ~ "Pile"))

#date 
process$DATE <- mdy(process$DATE) 

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
az_ready <- process %>%
  rename("SOURCE_ID" = "BurnNumber") %>% 
  #rename("DATE" = "") %>%
  rename("PERMITTED_ACRES" = "Daily Burn Acres Requested") %>%
  #rename("COMPLETED_ACRES" = "") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "Burn Name") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  # rename("ENTITY_REQUESTING" = "") %>%
  # rename("LAT_PERMIT" = "") %>%
  # rename("LON_PERMIT" = "") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  #write if exists here
  select(any_of(c("SOURCE_ID", "DATE", "PERMITTED_ACRES", "COMPLETED_ACRES", "PILE_VOLUME", "BURN_NAME", "BURNTYPE_REPORTED", 
                  "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", "LEGAL_DESCRIP", "TONS", "BURN_STATUS")))


###EXPORT

write_csv(az_ready, "out/az_ready.csv")
