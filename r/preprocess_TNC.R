#This script takes TNC Rx records from Blane Heumann and processes them so they can be compared to the state permit data.
#TNC assists on other people's lands but there are burns at least partially on TNC properties.

# NOTE: I think the Latitude is probably wrong on Kill’s devil units (Nagshead Woods, NC)
# I'm not worrying about it b/c current focus is on western states

library(tidyverse)
library(sf)


### BRING IN THE DATA ----

# tnc points
raw <- read_csv("in/TNC_Burns20230614_clean.csv") 
#remove spaces in column names 
names(raw) <- make.names(names(raw), unique=TRUE, allow_ = FALSE) 

# rx_west database points
rx_west <- read_sf("out/shp/rx_west.shp")


### PROCESS ----

process <- raw %>%
  # date
  mutate(DATE = mdy(Start.Date)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  # entity requesting
  mutate(ENTITY_REQUESTING = "TNC") %>%
  # burn status
  mutate(BURN_STATUS = "Complete") %>% # only completed burns are in this spreadsheet
  # burn type
  # assume that Non-broadcast is pile, most indicate pile in the name
  mutate(BURNTYPE_REPORTED = Type.of.Burn) %>%
  mutate(BURNTYPE_CLASSIFIED = case_when(BURNTYPE_REPORTED == "Non-broadcast" ~ "Pile",
                                       .default = "Broadcast")) %>%
  # state name abbreviations 
  mutate(STATE = case_when(StateName == "Arizona" ~ "AZ",
                           StateName == "California" ~ "CA",
                           StateName == "Colorado" ~ "CO",
                           StateName == "Idaho" ~ "ID",
                           StateName == "Montana" ~ "MT",
                           StateName == "New Mexico" ~ "NM",
                           StateName == "Nevada" ~ "NV",
                           StateName == "Oregon" ~ "OR",
                           StateName == "Utah" ~ "UT",
                           StateName == "Washington" ~ "WA",
                           StateName == "Wyoming" ~ "WY",
                           .default = "East"))

# remove extract [] and "" characters in some fields
process$Other.Landowner <-gsub("\\[|\\]", "", (process$Other.Landowner))
process$Other.Landowner <- gsub('"', '', process$Other.Landowner)
process$Burn.Lead <-gsub("\\[|\\]", "", (process$Burn.Lead))
process$Burn.Lead <- gsub('"', '', process$Burn.Lead)

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
tnc_ready <- process %>%
  #rename("SOURCE_ID" = "") %>% 
  #rename("DATE" = "") %>%
  #ename(ACRES_REQUESTED = "") %>%
  #rename("ACRES_PERMITTED" = "") %>%
  rename("ACRES_COMPLETED" = "Total.Acres.of.Burn") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "Burn.Unit.Name") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  # rename("ENTITY_REQUESTING" = "") %>%
  rename("LAT_PERMIT" = "Latitude") %>%
  rename("LON_PERMIT" = "Longitude") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  select("DATE", "ACRES_COMPLETED", "BURN_NAME", "BURNTYPE_REPORTED", "BURNTYPE_CLASSIFIED",
         "ENTITY_REQUESTING","LAT_PERMIT", "LON_PERMIT", "BURN_STATUS", "STATE", "TNC.Acres.Burned", 
         "Managed.Area.Name", "Other.Landowner", "Burn.Lead") %>%
  distinct()


### STANDARDIZE (do stuff that is done in MakeDatabase.R for state permit data) ----

tnc <- tnc_ready %>%
  # filter to west
  filter(STATE != "East") %>%
  # make a year column
  mutate(YEAR = year(DATE)) %>%
  # remove years > 2022
  filter(YEAR != "2024") %>%
  filter(YEAR != "2023") %>%
## Adjust NA values in summary fields
# NOTE: acres not reported for TNC pile burns
# step 1: make a field to track if a record had the info
# step 2: change negative and NA values to 0
  # acres completed
  mutate(na_complete = case_when(is.na(ACRES_COMPLETED) ~ "FALSE",
                                 .default = "TRUE")) %>%
  mutate(ACRES_COMPLETED = case_when(ACRES_COMPLETED < 0 ~ 0, # this catches any negative values
                                     is.na(ACRES_COMPLETED) ~ 0,
                                     .default = ACRES_COMPLETED))


### MAKE TNC POINTS SPATIAL OBJECT

# make sf object
tnc_west <- tnc %>%
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs = 4269) %>%
  # match projection
  st_transform(st_crs(rx_west))

# write shp
#st_write(tnc_west, "out/shp/tnc_west.shp", append = FALSE) #append set to overwrite existing data


### OVERLAY TNC + STATE PERMITS ----  

# buffer TNC points by 1000 meters
tnc_buf <- st_buffer(tnc_west, dist = 1000) 

# write shp
#st_write(tnc_buf, "out/shp/tnc_buf.shp", append = FALSE) #append set to overwrite existing data

# intersect the state points w/ tnc buffer
tnc_state <- st_intersection(tnc_buf, rx_west) 

# df and -select na_complete
tnc_state_df <- st_drop_geometry(tnc_state)

# check for many to 1 relationships; buf as 194 points; tnc_state has 273
  
###EXPORT
write_csv(tnc_state_df, "out/tnc_state_points_compare.csv")

