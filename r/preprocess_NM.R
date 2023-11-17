#This script takes NM Rx permit data and processes it so it is ready to be used in the Rx database
# Any column w/ R refers to information gathered during burn registration (i.e. permitted)
# Any column w/ T refers to information gathered during burn tracking (i.e. accomplished)


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
process <- raw %>% 
  #burns are complete when the burn days is > 0 and area or volume > 0 was reported
  mutate(BURN_STATUS = case_when(T_BURN_DAYS == 0 ~ "Incomplete",
                                 T_BURN_DAYS > 0 & T_TOTAL_AREA_ACRES | T_TOTAL_VOLUME_CU_FT > 0 ~ "Complete",
                                 .default = "Unknown"))%>% 
  mutate(ENTITY_REQUESTING = case_when(is.na(ORG_NAME) ~ OWNERSHIP,
                                       .default = ORG_NAME))

#BURNTYPE REPORTED
#guidance from NM was if a burn was tracked in acres it was broadcast, if tracked in volume it was piles
#first classify based on registrations (R_); if acres and volume are reported, burn type is unknown
process <- process%>%   
  mutate(BURNTYPE_REPORTED = case_when(
    R_TOTAL_AREA_ACRES > 0 & R_TOTAL_VOLUME_CU_FT <= 0 | is.na(R_TOTAL_VOLUME_CU_FT) ~ "Broadcast", 
    R_TOTAL_VOLUME_CU_FT > 0 & R_TOTAL_AREA_ACRES <= 0 | is.na(R_TOTAL_AREA_ACRES) ~ "Pile"))
#for unclassified rows, classify based on tracked burns (T_) if possible
process <- process%>%  
  mutate(BURNTYPE_REPORTED = ifelse(
    is.na(BURNTYPE_REPORTED) & T_TOTAL_AREA_ACRES > 0 & T_TOTAL_VOLUME_CU_FT <= 0, "Broadcast", BURNTYPE_REPORTED)) %>%
  mutate(BURNTYPE_REPORTED = ifelse(
    is.na(BURNTYPE_REPORTED) & T_TOTAL_VOLUME_CU_FT > 0 & T_TOTAL_AREA_ACRES <= 0, "Pile", BURNTYPE_REPORTED))

#DATE
process$DATE <- mdy(process$R_BURN_START) #parse the existing date format

#XWALK
nm_ready <- process %>%
  rename(SOURCE_ID = DB_REG_ID) %>% 
  rename(PERMITTED_ACRES = R_TOTAL_AREA_ACRES) %>% 
  rename(COMPLETED_ACRES = T_TOTAL_AREA_ACRES) %>%
  rename(PILE_VOLUME = T_TOTAL_VOLUME_CU_FT) %>%
  rename(LAT_PERMIT = R_LATITUDE) %>%
  rename(LON_PERMIT = R_LONGITUDE) %>%
  select(SOURCE_ID, DATE, PERMITTED_ACRES, COMPLETED_ACRES, PILE_VOLUME, BURN_NAME, BURNTYPE_REPORTED, BURN_NAME, ENTITY_REQUESTING, 
         LAT_PERMIT, LON_PERMIT, BURN_STATUS)

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
nm_ready <- process %>%
  rename("SOURCE_ID" = "DB_REG_ID") %>% 
  #rename("DATE" = "") %>%
  rename("PERMITTED_ACRES" = "R_TOTAL_AREA_ACRES") %>%
  rename("COMPLETED_ACRES" = "T_TOTAL_AREA_ACRES") %>%
  rename("PILE_VOLUME" = "T_TOTAL_VOLUME_CU_FT") %>%
  #rename("BURN_NAME" = "") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  #rename("ENTITY_REQUESTING" = "") %>%
  rename("LAT_PERMIT" = "R_LATITUDE") %>%
  rename("LON_PERMIT" = "R_LONGITUDE") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  select(any_of(c("SOURCE_ID", "DATE", "PERMITTED_ACRES", "COMPLETED_ACRES", "PILE_VOLUME", "BURN_NAME", "BURNTYPE_REPORTED", 
                  "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()

###EXPORT

write_csv(nm_ready, "out/nm_ready.csv")


### CHECKING FOR DUPLICATES
checking <- nm_ready %>%
  mutate(year = year(ymd(DATE))) %>%
  distinct(SOURCE_ID, year)