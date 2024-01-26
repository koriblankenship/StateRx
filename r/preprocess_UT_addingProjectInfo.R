#This script takes UT emissions data and processes it so it is ready to be used in the Rx database
#UT provided spreadsheets for burn permits, requests, and emissions report. 
#I tried to join them based on the burn name, but this resulted in illogical combinations 
#(e.g. dates on the requests can be in different years than the burn date and the burn date can be 
#before the request date). I couldn't be sure that the right request was going w/ the right emissions 
#report especially for units with multiple entries. Therefore, I used emissions data only for burns 
#that were reported as completed. This probably means we have relatively complete info for completed 
#burns but nothing for burns that were requested but did not occur.

#According to the data provider: "Burn bosses must file an emissions report for each burn request, 
#whether they actually burn or not. So if a row in the emissions table has "TRUE" under "no_burn", 
#that was not a burn day."  I removed NO burn days b/c it seemed onerous and not super useful to track 
#a bunch of requests that resulted in no burn.


library(tidyverse)

### BRING IN DATA

# project
project <- read_csv("in/UT_projectReport06-23-2023.csv") %>%
  select(name, acres, added_on) %>%
  # rename columns to make tracking easier when everything is combined
  rename(project_acres = acres) %>%
  rename(project_date = added_on) %>%
  unique()

# request
request <- read_csv("in/UT_requestReport06-23-2023.csv") %>%
  select(name, start_date, acres, type, lat, lng) %>%
  # rename columns to make tracking easier when everything is combined
  rename(date_request = start_date) %>%
  rename(request_acres = acres) %>%
  # remove date_requests that are null b/c I need year to join info
  filter(date_request != "NULL") %>%
  unique()

# emissions data (which report completion for each request)
emissions <- read_csv("in/UT_emissionReport06-23-2023.csv") %>%
  select(name, burn_date, agency, unit, total_acres, black_acres, no_burn, lat, lng) %>%
  rename(emissions_unit = unit) %>%
  # remove date_requests that are null b/c I need year to join info
  filter(!is.na(burn_date)) %>%
  unique()

# parse the date and add year to help line up requests and emissions
request$date_request <- mdy(request$date_request) 
emissions$burn_date <- mdy(emissions$burn_date) 
# 8 failed to parse; NA values; but I take these out below when year is added

request <- request %>%
  mutate(year_request = year(date_request)) # make a year column

emissions <- emissions %>%
  mutate(year_emissions = year(burn_date)) %>%
  filter(!is.na(year_emissions)) 


### JOIN ----

# join emissions and requests by name, location
# 12657 rows
# e.g. see Ahlstrom Hollow; the same name/lat/long combination can have requests over multiple years 
#that don't align w/ emissions year
#join1 <- full_join(request, emissions, by = c("name" = "name", "lat" = "lat", "lng" = "lng"))

# join emissions and requests by name, location, year
# 4941
# emissions data get entered multiple times; e.g. Ahlstrom Hollow, year request = 2017
#join2 <- full_join(request, emissions, by = c("name" = "name", "lat" = "lat", "lng" = "lng", 
#                                             "year_request" = "year_emissions"))
# LEFT join emissions and requests by name, location, year
# 3527
# many emissions rows do NOT join with a request row
# this method will miss requests w/ no emissions report, but all requests are supposed to have an emissions report
join3 <- left_join(emissions, request, by = c("name" = "name", "lat" = "lat", "lng" = "lng", 
                                              "year_emissions" = "year_request")) 

#remove columns I'm not using
join4 <- join3 %>%
  #select(-c(date_request, request_acres)) %>%
  #select(-c(date_request)) %>%
  # this removes the extra requests from the request table
  unique()

write_csv(join_fin, "out/TEST_UT_join_fin.csv")



write_csv(join2, "out/TEST_UT_join2.csv")
# write_csv(join_full2, "out/TEST_UT_joinfull2.csv")

### PROCESS ----

#filter for only burns that occurred
#normally I keep burns that don't occur, but since I'm not tracking projects and requests I'm missing 
#info on burns that were not implemented even before the emissions report stage
# process <- raw %>%
#   filter(no_burn == "FALSE" | no_burn == "0")

# entity_requesting 
process <- raw %>%
  mutate(ENTITY_REQUESTING = paste(agency, project_unit, sep = ", ")) %>%
  # completed acres
  # "total_acres" is what is reported to dispatch 
  # "black_acres" is for emissions calculations and could be less than total acres, if it was a mosaic burn.
  mutate(ACRES_COMPLETED = case_when(total_acres == 0 & black_acres > 0 ~ black_acres, #when total acres is not reported, use black acres
                                     .default = total_acres)) %>% #default is completed acres = total acres
  # burn status
  # data are for completed burns, but I classify status as unknown when there were no completed acres
  mutate(BURN_STATUS = case_when(ACRES_COMPLETED <= 0 ~ "Unknown",  
                                 ACRES_COMPLETED > 0 ~ "Complete"))

# date
process$DATE <- mdy(process$burn_date) #parse the existing date format

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
ut_ready <- process %>%
  #rename("SOURCE_ID" = "") %>% 
  #rename("DATE" = "") %>%
  #rename("ACRES_REQUESTED = "") %>%
  #rename("ACRES_PERMITTED" = "") %>%
  #rename("ACRES_COMPLETED" = "") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "name") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  #rename("ENTITY_REQUESTING" = "") %>%
  rename("LAT_PERMIT" = "lat") %>%
  rename("LON_PERMIT" = "lng") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  select(any_of(c("SOURCE_ID", "DATE", "ACRES_REQUESTED", "ACRES_PERMITTED", "ACRES_COMPLETED", "PILE_VOLUME", 
                  "BURN_NAME", "BURNTYPE_REPORTED", "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", 
                  "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()

###EXPORT

write_csv(ut_ready, "out/ut_ready.csv")
