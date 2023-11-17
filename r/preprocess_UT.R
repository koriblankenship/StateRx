#This script takes UT emissions data and processes it so it is ready to be used in the Rx database
#UT provided spreadsheets for burn permits, requests, and emsissions report. 
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

raw <- read_csv("in/UT_emissionReport06-23-2023.csv") %>%
  select(name, burn_date, agency, unit, total_acres, black_acres, no_burn, lat, lng)


### PROCESS ----

#filter for only burns that occurred
#normally I keep burns that don't occur, but since I'm not tracking projects and requests I'm missing 
#info on burns that were not implemented even before the emissions report stage
process <- raw %>%
  filter(no_burn == "FALSE" | no_burn == "0")

# entity_requesting 
process <- process %>%
  mutate(ENTITY_REQUESTING = paste(agency, unit, sep = ", ")) %>%
  # completed acres
  # "total_acres" is what is reported to dispatch 
  # "black_acres" is for emissions calculations and could be less than total acres, if it was a mosaic burn.
  mutate(COMPLETED_ACRES = case_when(total_acres == 0 & black_acres > 0 ~ black_acres, #when total acres is not reported, use black acres
                                     .default = total_acres)) %>% #default is completed acres = total acres
  # burn status
  # data are for completed burns, but I classify status as unknown when there were no completed acres
  mutate(BURN_STATUS = case_when(COMPLETED_ACRES <= 0 ~ "Unknown",  
                                 COMPLETED_ACRES > 0 ~ "Complete"))

# date
process$DATE <- mdy(process$burn_date) #parse the existing date format

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
ut_ready <- process %>%
  #rename("SOURCE_ID" = "") %>% 
  #rename("DATE" = "") %>%
  #rename("PERMITTED_ACRES" = "") %>%
  #rename("COMPLETED_ACRES" = "") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "name") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  #rename("ENTITY_REQUESTING" = "") %>%
  rename("LAT_PERMIT" = "lat") %>%
  rename("LON_PERMIT" = "lng") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  select(any_of(c("SOURCE_ID", "DATE", "PERMITTED_ACRES", "COMPLETED_ACRES", "PILE_VOLUME", "BURN_NAME", "BURNTYPE_REPORTED", 
                  "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()

###EXPORT

write_csv(ut_ready, "out/ut_ready.csv")
