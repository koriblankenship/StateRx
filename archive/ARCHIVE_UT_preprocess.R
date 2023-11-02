library(tidyverse)

### BRING IN DATA

# project <- read_csv("in/UT_projectReport06-23-2023.csv") %>%
#   select(name, agency, unit, lat, lng, added_on) %>%
#   rename(lat_project = lat) %>%
#   rename(lng_project = lng)
request <- read_csv("in/UT_requestReport06-23-2023.csv") %>%
  select(name, start_date, agency, unit, acres, type, lat, lng) %>%
  rename(lat_request = lat) %>%
  rename(lng_request = lng)
emission <- read_csv("in/UT_emissionReport06-23-2023.csv") %>%
  select(name, burn_date, total_acres, black_acres, no_burn, agency, unit) %>%
  rename(agency_emissions = agency) %>%
  rename(unit_emissions = unit)


### PREPROCESS 

#join
rawjoin <- full_join(request, emission, by = "name")

#put together data for entity_requesting field
rawjoin <- rawjoin %>%
  mutate(agency_final = case_when( #agency comes from request w/ exceptions
    is.na(agency) ~ agency_emissions, #exception: if request agency is NA, use emissions agency
    agency == "DAQ" & !is.na(agency_emissions) ~ agency_emissions, #exception if request agency is DAQ and emissions has an agency, use emissions agency
    .default = agency)) %>% 
  mutate(unit_final = ifelse(is.na(unit), unit_emissions, unit)) %>% #unit_final comes from the request; if the request unit is NA, use emissions unit 
  mutate(ENTITY_REQUESTING = paste(agency_final, unit_final, sep = ", ")) #concatenate the final agency and final unit to create the entity requesting

#date

#if burn_date = 0000-00-00 or 1-Dec; date = start_date
rawjoin2 <- rawjoin %>%
  mutate(DATE = case_when(burn_date == "0000-00-00" ~ start_date,
    burn_date == "1-Dec" ~ start_date,
    is.na(burn_date) ~ start_date,
    .default = burn_date))


#date = burn date, if burn_date is blank use Request start_date
rawjoin <- rawjoin %>%
  mutate(DATE = ifelse(is.na(unit), start_date, burn_date))  #date = burn_date; if burn_date is NA then date = start_date 
  
#calc completed acres
#if no_burn = true , a burn didn't happen; completed acres = 0
#if no_burn = false, a burn occurred; 
  # if total acres = 0 = black acres
  #if total acres > 0, completed acres = total acres


#xwalk to the rx database column names and formats


###EXPORT

write_csv(raw_join2, "out/UT_test2.csv")
