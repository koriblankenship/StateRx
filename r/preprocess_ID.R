#This script takes ID rx permit data and processes it so it is ready to be used in the Rx database

library(tidyverse)


### BRING IN THE DATA ----

raw <- read_csv("in/ID_IDL Burn Permit Data - 2004 to current.csv") 


### PROCESS ----

process <- raw %>%
  #date
  mutate(DATE = mdy_hm(IssueDate)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  #permitted acres
  mutate(PERMITTED_ACRES = case_when(BurnAcres == "NULL" ~ NA,
                                     .default = BurnAcres)) %>%
  mutate_at("PERMITTED_ACRES", as.numeric) %>%
  #burn status
  mutate(BURN_STATUS = "Unknown")

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
id_ready <- process %>%
  #rename("SOURCE_ID" = "") %>% 
  #rename("DATE" = "") %>%
  #rename("PERMITTED_ACRES" = "") %>%
  #rename("COMPLETED_ACRES" = "") %>%
  #rename("PILE_VOLUME" = "") %>%
  #rename("BURN_NAME" = "") %>%
  rename("BURNTYPE_REPORTED" = "BurnType") %>%
  #rename("ENTITY_REQUESTING" = "") %>%
  rename("LAT_PERMIT" = "Latitude") %>%
  rename("LON_PERMIT" = "Longitude") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  #write if exists here
  select(any_of(c("SOURCE_ID", "DATE", "PERMITTED_ACRES", "COMPLETED_ACRES", "PILE_VOLUME", "BURN_NAME", "BURNTYPE_REPORTED", 
                  "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", "LEGAL_DESCRIP", "TONS", "BURN_STATUS")))


### LOOKING AT DUPLICATE RECORDS
id_unique <- distinct(id_ready) #65% of records are unique

#types of burning
burntypes <- unique(id_ready$BURNTYPE_REPORTED)
#list of residential burning types
burntype_res <- list("Yard Waste", "Burn Barrel", "Burn Barrel (residential solid waste disposal)", 
                     "Residential yard waste/Orchard Clippings", "Residential Yard Waste/Small Slash Piles (Max size 5 feet high 5 feet wide)")
#all residential burns
id_subset_residential <- id_ready %>% 
  filter(BURNTYPE_REPORTED %in% burntype_res)
#177204 of 263812 burns are residential; 67% of records are residential

#remove ag records from ID ready df
id_subset_notres <- anti_join(id_ready, id_subset_residential, by = c("DATE", "PERMITTED_ACRES", "BURNTYPE_REPORTED", 
                                                                      "LAT_PERMIT", "LON_PERMIT"))
#how many non res burns are distinct
id_unique_notres <- distinct(id_subset_notres) # 52089 of 86608; 60% of records are unique

# starting in ~2013 data are most consistent
id_ready_all_2013_2022 <- id_ready %>%
  filter(between(as_date(DATE), as_date("2013-01-01"), as_date("2022-12-31")))
#remove ag 
id_post2013notres <- anti_join(id_ready_all_2013_2022, id_subset_residential, by = c("DATE", "PERMITTED_ACRES", "BURNTYPE_REPORTED", 
                                                                      "LAT_PERMIT", "LON_PERMIT"))
#how many non res burns are distinct in post 2013 df
id_post2013notres_distinct <- distinct(id_post2013notres) # 45780 of 48497; 94% of records are unique

#suspicious lat
id_ready_super_lat <- id_ready %>%
  filter(LAT_PERMIT == 44.24046)


###EXPORT

write_csv(id_ready, "out/id_ready_DRAFT.csv")
