#This script takes MT rx permit data and processes it so it is ready to be used in the Rx database

library(tidyverse)


### BRING IN THE DATA ----

raw <- read_csv("in/MT_Rx_burns_Majors.csv") %>%
  select("ID", "Proposal Date", "Proposed Acres", "Acres Burned", "Unit Name", "Burn Type", "Member", "Latitude", "Longitude", 
         "Tons/Acre", "Status", "No Burn Reason") 
#remove spaces in column names
names(raw) <- make.names(names(raw), unique=TRUE) 

### PROCESS ----

process <- raw %>%
#date 
  mutate(DATE = mdy(Proposal.Date)) %>%
#completed acres
  mutate(COMPLETED_ACRES = case_when(Acres.Burned < 0 ~ "0",
                                     Acres.Burned == "None" ~ "0",
                                     .default = Acres.Burned)) %>%
  mutate_at("COMPLETED_ACRES", as.numeric) %>%
#tons
  mutate(TONS = Tons.Acre * COMPLETED_ACRES) 


#Status, no burn reason? ***********************************************************?
####once this is answered, recheck the tons calculation when completed acres is 0, should tons be 0 or -9999?
status <- unique(process$Status)

process <- process %>%
  mutate(BURN_STATUS = case_when(Status == "Approved" ~ "Incomplete",
                                 Status == "Completed" ~ "Complete"))
  
#what about completed burn w/ 0 acres that was broadcast ******************************?
# e.g. id 173666, acres burned is 0 but status is completed, status is not in prescription
# what does completed vs approved mean; does completed meant a burn was done?


#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
mt_ready <- process %>%
  rename("SOURCE_ID" = "ID") %>% 
  #rename("DATE" = "") %>%
  rename("PERMITTED_ACRES" = "Proposed.Acres") %>%
  #rename("COMPLETED_ACRES" = "") %>%
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
  select(any_of(c("SOURCE_ID", "DATE", "PERMITTED_ACRES", "COMPLETED_ACRES", "PILE_VOLUME", "BURN_NAME", "BURNTYPE_REPORTED", 
                  "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", "LEGAL_DESCRIP", "TONS", "BURN_STATUS")))


###EXPORT

write_csv(mt_ready, "out/mt_ready_DRAFT.csv")
