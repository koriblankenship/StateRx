#This script takes NV burn permit data and processes it so it is ready to be used in the Rx database.
#Data was provided in 2 spreadsheets: pre 2020 and 2020-2022.
#This script processes the 2 spreadsheets so that they can be bound together and formatted for the RX database.

library(tidyverse)


### BRING IN DATA

post2020 <- read_csv("in/NV_PrescribedBurn_DataRequest_2023_nv2020.csv") 

pre2020 <- read_csv("in/NV_PrescribedBurn_DataRequest_2023_nvpre2020.csv") %>%
  select(-OBJECTID)


### PREPROCESS 

#Remove duplicate rows
#If all columns, except Object ID, are identical for a set of rows, consider these rows as a single burn. 
#Do not sum acres, just retain 1 row.
post2020 <- distinct(post2020)
pre2020 <- distinct(pre2020)

#post2020: select and rename columns of interest to rx database name, manage date
post2020 <- post2020 %>%
  select(-c(StartTime, BurnEndDate, EndTime, LocationDesc, BurnAmount)) %>%
  rename(SOURCE_ID = PermitID) %>% 
  rename(ENTITY_REQUESTING = AgencyName) %>%
  rename(BURNTYPE_REPORTED = BurnType) %>%
  rename(LAT_PERMIT = LatDD) %>%
  rename(LON_PERMIT = LongDD) 
post2020$DATE <- mdy(post2020$BurnStartDate) 
post2020 <- post2020 %>%
  select(-BurnStartDate)

#pre2020: select and rename columns of interest to match post2020 names, get burn type
pre2020 <- pre2020 %>%
  select(-c(Burn_Location, Burn_End_Date, Reason, Material, Burn_Notes)) %>%
  rename(SOURCE_ID = Authorization_ID) %>% 
  rename(DATE = Burn_Start_Date) %>% 
  rename(ENTITY_REQUESTING = Agency_Name) %>%
  rename(LAT_PERMIT = Latitude) %>%
  rename(LON_PERMIT = Longitude) %>%
  rename(BURN_QUANTITY = Acres_Piles) %>%
  rename(BURN_UNITS = Acres_Piles_Units) %>%
  #BurnType: assume pile except for when BURN_UNITS(Acres_Piles_Units) is “acres”
  mutate(BURNTYPE_REPORTED = case_when(BURN_UNITS == "acres" ~ "Broadcast Burn",
                              .default = "Pile Burn"))

### PROCESS

#bind all years
processed <- rbind(post2020, pre2020)

#calc permitted acres, completed acres, pile volume
processed <- processed %>%
  mutate(PERMITTED_ACRES = case_when(BURN_UNITS == "acres" | BURN_UNITS == "acres of piles" ~ BURN_QUANTITY,
                                     .default = NA)) %>%
  #completed acres = permitted acres, NV only provided data on completed burns
  mutate(COMPLETED_ACRES = PERMITTED_ACRES) %>%
  mutate(PILE_VOLUME = case_when(BURN_UNITS == "cubic feet" ~ BURN_QUANTITY,
                                 #convert cubic yards to cubic feet (1 cu yd = 27 cu ft)
                                 BURN_UNITS == "cubic yards" ~ BURN_QUANTITY * 27,
                                 .default = NA)) %>%
  mutate(BURN_STATUS = "Complete")

nv_ready <- processed %>%
  select(-c(BURN_QUANTITY, BURN_UNITS))


###EXPORT

write_csv(nv_ready, "out/nv_ready.csv")

