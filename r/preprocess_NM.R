#This script takes NM Rx permit data and processes it so it is ready to be used in the Rx database

library(tidyverse)


### BRING IN DATA

raw <- read_csv("in/NM_smoke_export all.csv") %>%
  select(DB_REG_ID, R_BURN_START, R_TOTAL_AREA_ACRES, R_TOTAL_VOLUME_CU_FT, T_TOTAL_AREA_ACRES, T_TOTAL_VOLUME_CU_FT, BURN_NAME, ORG_NAME, 
         R_LATITUDE, R_LONGITUDE, OWNERSHIP, REG_TYPE, T_WILDLAND_FIRE_USE, T_BURN_DAYS) %>%
  filter(REG_TYPE != "Wildfire") %>% #remove wildfires
  filter(T_WILDLAND_FIRE_USE == "No") %>% #remove wildland fire use fires
  mutate_at("DB_REG_ID", as.character) #changing to character b/c that is the standard for the source id column


### PREPROCESS 

#BURN STATUS, ENTITY REQUESTING
processed <- raw %>% 
  #burns are complete when the burn days is > 0 and area or volume > 0 was reported
  mutate(BURN_STATUS = case_when(T_BURN_DAYS == 0 ~ "Incomplete",
                                 T_BURN_DAYS > 0 & T_TOTAL_AREA_ACRES | T_TOTAL_VOLUME_CU_FT > 0 ~ "Complete",
                                 .default = "Unknown"))%>% 
  mutate(ENTITY_REQUESTING = case_when(is.na(ORG_NAME) ~ OWNERSHIP,
                                       .default = ORG_NAME))

#BURNTYPE REPORTED
#guidance from NM was if a burn was tracked in acres it was broadcast, if tracked in volume it was piles
#first classify based on registrations (R_); if acres and volume are reported, burn type is unknown
processed <- processed%>%   
  mutate(BURNTYPE_REPORTED = case_when(R_TOTAL_AREA_ACRES > 0 & R_TOTAL_VOLUME_CU_FT <= 0 | is.na(R_TOTAL_VOLUME_CU_FT) ~ "Broadcast", 
                                       R_TOTAL_VOLUME_CU_FT > 0 & R_TOTAL_AREA_ACRES <= 0 | is.na(R_TOTAL_AREA_ACRES) ~ "Pile",
                                       .default = "Unknown"))
#for Unknown rows, classify based on tracked burns (T_) if possible, else leave as unknown
processed <- processed%>%   
  mutate(BURNTYPE_REPORTED = ifelse(BURNTYPE_REPORTED == "Unknown" & T_TOTAL_AREA_ACRES > 0 & T_TOTAL_VOLUME_CU_FT <= 0, "Broadcast", BURNTYPE_REPORTED)) %>%
  mutate(BURNTYPE_REPORTED = ifelse(BURNTYPE_REPORTED == "Unknown" & T_TOTAL_VOLUME_CU_FT > 0 & T_TOTAL_AREA_ACRES <= 0, "Pile", BURNTYPE_REPORTED))

#DATE
processed$DATE <- mdy(processed$R_BURN_START) #parse the existing date format

#XWALK
nm_ready <- processed %>%
  rename(SOURCE_ID = DB_REG_ID) %>% 
  rename(PERMITTED_ACRES = R_TOTAL_AREA_ACRES) %>% 
  rename(COMPLETED_ACRES = T_TOTAL_AREA_ACRES) %>%
  rename(PILE_VOLUME = T_TOTAL_VOLUME_CU_FT) %>%
  rename(LAT_PERMIT = R_LATITUDE) %>%
  rename(LON_PERMIT = R_LONGITUDE) %>%
  select(SOURCE_ID, DATE, PERMITTED_ACRES, COMPLETED_ACRES, PILE_VOLUME, BURN_NAME, BURNTYPE_REPORTED, BURN_NAME, ENTITY_REQUESTING, 
         LAT_PERMIT, LON_PERMIT, BURN_STATUS)


###EXPORT

write_csv(nm_ready, "out/nm_ready.csv")
