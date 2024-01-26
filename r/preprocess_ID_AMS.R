#This script takes ID rx request data and processes it so it is ready to be used in the Rx database

library(tidyverse)


### BRING IN THE DATA ----

raw <- read_csv("in/ID_AMS_data_CY_2010.csv") %>%
  #remove burns when status is draft
  # these burns were drafted in the system but never proposed or implemented
  filter(Status != "Draft") %>%
  mutate_at("ID", as.character) #changing to character b/c that is the standard for the source id column
#remove spaces in column names
names(raw) <- make.names(names(raw), unique=TRUE) 


### PROCESS ----

process <- raw %>%
  #date 
  mutate(DATE = mdy(Proposal.Date)) %>%
  # acres completed
  mutate(Acres.Burned = case_when(Acres.Burned == "None" ~ "-9999", # make all values numbers
                                  .default = Acres.Burned)) %>% 
  mutate_at("Acres.Burned", as.numeric) %>% # make acres burned a numeric field
  mutate(ACRES_COMPLETED = case_when(Acres.Burned < 0 ~ 0, 
                                     .default = Acres.Burned)) %>%
  #tons
  mutate(TONS = Tons.Acre * ACRES_COMPLETED) %>%
  #burn status
  # Status can be "completed" and burned acres "0"; ~80% of these have a No.Burn.Reason that suggests
  # no burning occured so if acres is 0 I'm classifying the burn as incomplete
  mutate(BURN_STATUS = case_when(ACRES_COMPLETED > 0 ~ "Complete",
                                 .default = "Incomplete"))

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
ready <- process %>%
  rename("SOURCE_ID" = "ID") %>% 
  #rename("DATE" = "") %>%
  rename("ACRES_REQUESTED" = "Proposed.Acres") %>% 
  #rename("ACRES_PERMITTED" = "") %>%
  #rename("ACRES_COMPLETED" = "") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "Unit.Name") %>%
  rename("BURNTYPE_REPORTED" = "Burn.Type") %>%
  rename("ENTITY_REQUESTING" = "Member") %>%
  rename("LAT_PERMIT" = "Latitude") %>%
  rename("LON_PERMIT" = "Longitude") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  #write if exists here
  select(any_of(c("SOURCE_ID", "DATE", "ACRES_REQUESTED", "ACRES_PERMITTED", "ACRES_COMPLETED", "PILE_VOLUME", 
                  "BURN_NAME", "BURNTYPE_REPORTED", "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", 
                  "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()


###EXPORT

write_csv(ready, "out/id_ams_ready.csv")
