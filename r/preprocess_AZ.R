#This script takes AZ Rx permit data and processes it so it is ready to be used in the Rx database

# Improvements:
# -Remove NAs from Entity Requesting that result from concatenating a field w/ NA, e.g. "U.S. Fish & Wildlife Service, NA, San Bernardino"

library(tidyverse)


### BRING IN THE DATA ----

raw <- read_csv("in/AZ_accomplishment_data.csv") %>%
  select("Ignition Date", "Agency", "District", "Burn Number", "Burn Name", "Daily Burn Acres Requested", 
         "Acres Treated", "Acres Burned", "Location")
#remove spaces in column names
names(raw) <- make.names(names(raw), unique=TRUE) 


### PROCESS ----

process <- raw %>%
  #date
  mutate(DATE = mdy(Ignition.Date)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  # entity requesting
  mutate(ENTITY_REQUESTING = paste(Agency, District, sep = ", ")) %>%
  # burn status
  mutate(BURN_STATUS = case_when(Acres.Burned == 0 ~ "Incomplete",
                                 Acres.Burned > 0 ~ "Complete",
                                 .default = "Unknown")) %>%
  # burn type
  mutate(BURNTYPE_REPORTED = case_when(str_ends(Burn.Number, "B") ~ "Broadcast",
                                       str_ends(Burn.Number, "P") ~ "Pile"))

#lat lon remove parentheses
process$Location <- gsub("[()]", "", process$Location) 
# lat lon formatting
process <- process%>%
  separate_wider_delim("Location", ";", names = c("LAT_PERMIT", "LON_PERMIT")) %>%
  mutate(LON_PERMIT = as.numeric(LON_PERMIT)) %>%
  mutate(LAT_PERMIT = as.numeric(LAT_PERMIT)) 

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
az_ready <- process %>%
  rename("SOURCE_ID" = "Burn.Number") %>% 
  #rename("DATE" = "") %>%
  rename(ACRES_REQUESTED = "Daily.Burn.Acres.Requested") %>%
  #rename("ACRES_PERMITTED" = "") %>%
  rename("ACRES_COMPLETED" = "Acres.Burned") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "Burn.Name") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  # rename("ENTITY_REQUESTING" = "") %>%
  # rename("LAT_PERMIT" = "") %>%
  # rename("LON_PERMIT" = "") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  select(any_of(c("SOURCE_ID", "DATE", "ACRES_REQUESTED", "ACRES_PERMITTED", "ACRES_COMPLETED", "PILE_VOLUME", 
                  "BURN_NAME", "BURNTYPE_REPORTED", "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", 
                  "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()


###EXPORT

write_csv(az_ready, "out/az_ready.csv")

