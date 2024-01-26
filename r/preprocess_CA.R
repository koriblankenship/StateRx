#This script takes CA rx permit data and processes it so it is ready to be used in the Rx database

## TO DO: 
# -look at the legal description formatting, change those w/ special characters?
#

library(tidyverse)


### BRING IN THE DATA ----

#make a list of all CA data sheets
file_list <- list.files(path="in/", pattern = "CA_", full.names=TRUE ) 
#read all CA sheets into 1 df
raw <- read_csv(file_list, id = "file_name") 
#remove spaces in column names
names(raw) <- make.names(names(raw), unique=TRUE) 


### PROCESS ----

process <- raw %>%
  select(-file_name) %>% #remove the file name column
  filter_at(vars(Burn.Date, Acres.Requested, Acres.Burned, Latitude, Longitude),all_vars(!is.na(.))) %>%
  #all_vars(!is.na(.)) means that all the variables listed need to be not NA.
#date
  mutate(DATE = mdy(Burn.Date)) %>%
  mutate(DATE = as.Date(DATE)) %>%
#requested acres
  mutate(ACRES_REQUESTED = as.numeric(Acres.Requested)) %>%
#burn status
  mutate(BURN_STATUS = case_when(Acres.Burned == 0 ~ "Incomplete",
                                 Acres.Burned > 0 ~ "Complete",
                                 .default = "Unknown")) %>%
#total tons
  mutate(TONS = as.numeric(Total.Tons)) 
process$TONS[process$TONS < 0] <- NA #change negative tons values to NA

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
ca_ready <- process %>%
  #rename("SOURCE_ID" = "") %>% 
  #rename("DATE" = "") %>%
  #rename("ACRES_REQUESTED = "") %>%
  #rename("ACRES_PERMITTED" = "") %>%
  rename("ACRES_COMPLETED" = "Acres.Burned") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "Burn.Unit") %>%
  rename("BURNTYPE_REPORTED" = "Burn.Type") %>%
  rename("ENTITY_REQUESTING" = "Agency") %>%
  rename("LAT_PERMIT" = "Latitude") %>%
  rename("LON_PERMIT" = "Longitude") %>%
  rename("LEGAL_DESCRIP" = "Legal.Location") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  select(any_of(c("SOURCE_ID", "DATE", "ACRES_REQUESTED", "ACRES_PERMITTED", "ACRES_COMPLETED", "PILE_VOLUME", 
                  "BURN_NAME", "BURNTYPE_REPORTED", "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", 
                  "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()


###EXPORT

write_csv(ca_ready, "out/ca_ready.csv")




########## SOME CHECKING -----

checking1 <- ca_ready %>%
  mutate(year = year(ymd(DATE))) %>%
  distinct(DATE, PERMITTED_ACRES, COMPLETED_ACRES, BURN_NAME, BURNTYPE_REPORTED, ENTITY_REQUESTING, LAT_PERMIT, LON_PERMIT)


#check distinct for year, lat/lon
checking2 <- ca_ready %>%
  mutate(year = year(ymd(DATE))) %>%
  distinct(year, BURN_NAME, LAT_PERMIT, LON_PERMIT)

