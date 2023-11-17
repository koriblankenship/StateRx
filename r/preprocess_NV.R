#This script takes NV's completed burn info and processes it so it is ready to be used in the Rx database.
#Before 2023 NV only has records on completed burns.
#Data was provided in 2 spreadsheets: pre 2020 and 2020-2022.
#This script processes the 2 spreadsheets so that they can be bound together and formatted for the RX database.

library(tidyverse)


### BRING IN DATA ----

post2020 <- read_csv("in/NV_PrescribedBurn_DataRequest_2023_nv2020.csv") 

pre2020 <- read_csv("in/NV_PrescribedBurn_DataRequest_2023_nvpre2020.csv") %>%
  select(-OBJECTID)


### PREPROCESS ----

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

### PROCESS ----

#bind all years
processed <- rbind(post2020, pre2020)

# permitted acres
processed <- processed %>%
  mutate(COMPLETED_ACRES = case_when(BURN_UNITS == "acres" | BURN_UNITS == "acres of piles" ~ BURN_QUANTITY)) %>%
  # pile volume
  mutate(PILE_VOLUME = case_when(BURN_UNITS == "cubic feet" ~ BURN_QUANTITY,
                                 #convert cubic yards to cubic feet (1 cu yd = 27 cu ft)
                                 BURN_UNITS == "cubic yards" ~ BURN_QUANTITY * 27)) %>%
  #burn status
  # data were only provided on completed burns, but I classify status as unknown when there were no completed acres
  mutate(BURN_STATUS2 = case_when(COMPLETED_ACRES <= 0 | is.na(COMPLETED_ACRES) ~ "Unknown", 
                               COMPLETED_ACRES > 0 ~ "Complete"))

# select the rx database columns
nv_ready <- processed %>%
  select(any_of(c("SOURCE_ID", "DATE", "PERMITTED_ACRES", "COMPLETED_ACRES", "PILE_VOLUME", "BURN_NAME", "BURNTYPE_REPORTED", 
                "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()


###EXPORT

write_csv(nv_ready, "out/nv_ready.csv")

