#This script takes UT emissions data and processes it so it is ready to be used in the Rx database

library(tidyverse)

### BRING IN DATA

raw <- read_csv("in/UT_emissionReport06-23-2023.csv") %>%
  select(name, burn_date, agency, unit, total_acres, black_acres, no_burn, lat, lng)


### PREPROCESS 

#filter for only burns that occurred
#normally I keep burns that don't occur, but since I'm not tracking projects and requests I'm missing 
#info on burns that were not implemented even before the emissions report stage
processed <- raw %>%
  filter(no_burn == "FALSE" | no_burn == "0")

#put together data for entity_requesting field
processed <- processed %>%
  mutate(ENTITY_REQUESTING = paste(agency, unit, sep = ", "))

#manage date
processed$DATE <- mdy(processed$burn_date) #parse the existing date format

processed <- processed %>%
  # completed acres
  mutate(COMPLETED_ACRES = case_when(total_acres == 0 & black_acres > 0 ~ black_acres, #when total acres is not reported, use black acres
                                     .default = total_acres)) %>% #default is completed acres = total acres
  # burn status
  mutate(BURN_STATUS = case_when(COMPLETED_ACRES <= 0 ~ "Incomplete",
                                 COMPLETED_ACRES > 0 ~ "Complete"))

# xwalk to the rx database column names and formats
ut_ready <- processed %>%
  rename("BURN_NAME" = "name") %>% 
  rename("LAT_PERMIT" = "lat") %>%
  rename("LON_PERMIT" = "lng") %>%
  select(BURN_NAME, DATE, COMPLETED_ACRES, BURN_NAME, ENTITY_REQUESTING, LAT_PERMIT, LON_PERMIT, BURN_STATUS)


###EXPORT

write_csv(ut_ready, "out/ut_ready.csv")
