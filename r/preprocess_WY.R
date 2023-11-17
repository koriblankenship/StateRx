#This script takes WY permit data and processes it so it is ready to be used in the Rx database
#WY provided 3 tables: SMP1 pre 2022, SMP2 pre 2022, and SMP1 and 2 permits for 2022 
#in this script:
#the 3 tables are brought in and processed individually 
#they are then joined to create a master table ready to add to the Rx database


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> UNRESOLVED ISSUE
#lat/lon stuff; some notes on this in the archive at the bottom of this script


library(tidyverse)

### BRING IN DATA

smp1 <- read_csv("in/WY_pre2022smp1.csv") %>%
  select("BurnID", "Burn Name", "Latitude", "Longitude", "Agency or Company", "Daily Burn Information (DBI) Day 1 Date",
         "Burned?", "BURNTYPE", "PERMITTED_ACRES", "PILE_VOLUME") %>%
  rename("Day1Date" = "Daily Burn Information (DBI) Day 1 Date")
smp2 <- read_csv("in/WY_pre2022smp2.csv") %>%
  select(BurnID, Date1, TotalArea, TotalPileVolume, BurnName, AgencyOrCompany, LAT_PERMIT, LON_PERMIT, "Burned?")
wyo <- read_csv("in/WY_2022.csv") %>%
  select("BURN ID", "Date1", "Burn Name", "CONTACT AGENCY/COMPANY", 
         "BURN LOCATION LAT", "BURN LOCATION LONG", "PERMITTED_ACRES", "PILE_VOLUME", "Burned?")


### PROCESS: smp1

#xwalk to the rx database column names and formats
smp1_processed <- smp1 %>%
  rename("SOURCE_ID" = "BurnID") %>%
  mutate_at("SOURCE_ID", as.character) %>% # this will be character in the master RX database
  rename("ACRES_PERMITTED" = "PERMITTED_ACRES") %>%
  rename("BURN_NAME" = "Burn Name") %>%
  rename("ENTITY_REQUESTING" = "Agency or Company") %>%
  rename("LAT_PERMIT" = "Latitude") %>%
  rename("LON_PERMIT" = "Longitude") 

#mange date
# there are many missing dates
smp1_processed <- smp1_processed %>%
  mutate(DATE = parse_date_time(Day1Date, orders = c("dmy", "mdy"))) #parse the existing dates - data format is both dmy and mdy
smp1_processed <- smp1_processed %>%
  mutate(DATE = ymd(DATE)) #change date format from POSIXct to ymd


### PROCESS: smp2

#xwalk to the rx database column names
smp2_processed <- smp2 %>%
  rename("SOURCE_ID" = "BurnID") %>%
  mutate_at("SOURCE_ID", as.character) %>% # this will be character in the master RX database
  rename("ACRES_PERMITTED" = "TotalArea") %>%
  rename("PILE_VOLUME" = "TotalPileVolume") %>%
  rename("BURN_NAME" = "BurnName") %>%
  rename("ENTITY_REQUESTING" = "AgencyOrCompany") %>%
  mutate_at("LAT_PERMIT", as.numeric) #was character, needs to be numeric like other lat fields

#mange date 
# there are many missing dates
smp2_processed <- smp2_processed %>%
  mutate(DATE = parse_date_time(Date1, orders = c("dmy", "mdy"))) #parse the existing dates - data format is both dmy and mdy
smp2_processed <- smp2_processed %>%
  mutate(DATE = ymd(DATE)) #change date format from POSIXct to ymd


### PROCESS: wyo

#xwalk to the rx database column names
wyo_processed <- wyo %>%
  rename("SOURCE_ID" = "BURN ID") %>%
  rename("ACRES_PERMITTED" = "PERMITTED_ACRES") %>%
  rename("BURN_NAME" = "Burn Name") %>%
  rename("ENTITY_REQUESTING" = "CONTACT AGENCY/COMPANY") %>%
  rename("LAT_PERMIT" = "BURN LOCATION LAT") %>% 
  rename("LON_PERMIT" = "BURN LOCATION LONG")  %>%
# remove 2023 data - doing this in the make database script
# this dataframe has 2022 and 2023 data; I'm removing 2023 b/c its incomplete and I'm not processing that year for any state
#  mutate(year = str_sub(SOURCE_ID, 1, 4)) %>% # pull out the year from the source ID  
#  filter(year == "2022") %>%
#mange date
# there are many missing dates, when date is missing make date = 1/1/2022
  mutate(DATE = case_when(is.na(Date1) ~ "01/01/2022",
                           .default = Date1)) %>%
  mutate(DATE = mdy(DATE))


### BIND ALL DATA BACK TOGETHER

wy_ready <- bind_rows(wyo_processed, smp2_processed, smp1_processed) %>%
  rename("WasBurned" = "Burned?") %>% #get rid of that pesky ?
  #completed acres
  # according to WY: "The "Burned?" fields indicate that the planned burn was conducted. 
  # if that field is blank, we did not receive confirmation of the burn being conducted."
  mutate(ACRES_COMPLETED = case_when(WasBurned == "Burned" ~ ACRES_PERMITTED)) %>%
  #burn status
  mutate(BURN_STATUS = case_when(is.na(WasBurned) ~ "Incomplete",
                                 WasBurned == "Burned" ~ "Complete")) %>%
  #select final columns
  select(any_of(c("SOURCE_ID", "DATE", "ACRES_REQUESTED", "ACRES_PERMITTED", "ACRES_COMPLETED", "PILE_VOLUME", 
                  "BURN_NAME", "BURNTYPE_REPORTED", "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", 
                  "LEGAL_DESCRIP", "TONS", "BURN_STATUS")))  %>%
  distinct()
 

###EXPORT

write_csv(wy_ready, "out/wy_ready.csv")



#########ARCHIVE#################

### CHECKING FOR DUPLICATES
checking <- wy_ready %>%
  mutate(year = year(ymd(DATE))) %>%
  distinct(SOURCE_ID, year)
checking2 <- distinct(wy_ready)

### LAT/LON STUFF

#lat/lon corrections:{(for smp2 data)}
#burn ID 1460, lat/long is transposed
#if "Variable" change to NA
#if "See maps on P Drive" change to NA


#lat/lon corrections:
#lats in 1000s and lons in 10000s (fix stuff below then see if this is a problem)
#negative latitudes (might be listed more specifically below)
##########################figure out how to code these in the lat/lon corrections below#######################

#1 wrong sign on lat or lon
#1a: long is positive but should be negative, (displays in mongolia)
#1b: #lat is neg, long is positive, reverse neg/pos, id: 	2022-91 (display Australia)

#2: things I can't fix:
#2a: lat/long are the same number (display s. america)
#2b: lat/lon - 1/1 or 2/2, delete lat/long, (display Africa)

#"correct" clearly erroneous lat/lons
wyo_processed_locations <- wyo_processed %>%        
  mutate(LON_PERMIT2 = ifelse(LON_PERMIT > 0, LON_PERMIT * -1, LON_PERMIT)) %>% #1a/b) positive lon should be negative
  mutate(LAT_PERMIT2 = ifelse(LAT_PERMIT < 0, LAT_PERMIT * -1, LAT_PERMIT)) %>% #1a/b) negative lat should be positive
  mutate(LAT_PERMIT2 = na_if(LAT_PERMIT, LON_PERMIT)) %>% #2A IF LAT/LON ARE THE SAME, CANT FIX, MAKE NA
  mutate(LON_PERMIT2 = na_if(LAT_PERMIT, NA)) %>% #2a IF THE LAT PERMIT IS NA, THE LON PERMIT NEEDS TO BE NA
  mutate(LAT_PERMIT2 = na_if(LON_PERMIT, NA)) %>% #2a IF THE LON PERMIT IS NA, THE LAT PERMIT NEEDS TO BE NA
  #  mutate(LAT_PERMIT2 = ifelse(LAT_PERMIT == LON_PERMIT ~ NA, LAT_PERMIT)) %>% #2A IF LAT/LON ARE THE SAME, CANT FIX, MAKE NA
  #  mutate(LON_PERMIT2 = ifelse(is.na(LAT_PERMIT), ~ NA, LON_PERMIT)) %>%   #2aIF THE LAT PERMIT IS NA, THE LON PERMIT NEEDS TO BE NA
  #  mutate(LAT_PERMIT2 = replace(LAT_PERMIT, which(LAT_PERMIT == LON_PERMIT), NA)) %>% #2a
  mutate(LAT_PERMIT2 = na_if(LAT_PERMIT, 1 | 2)) %>% #2b) if Lat = 1, give it a value of NA
  mutate(LON_PERMIT2 = na_if(LON_PERMIT, 1 | 2)) #2b) Lon = -1, give it a value of NA
############if the or statements above work than the two lines below are not needed
#  mutate(LAT_PERMIT2 = na_if(LAT_PERMIT, 2)) %>% #2b) if Lat = 2, give it a value of NA
#  mutate(LON_PERMIT2 = na_if(LON_PERMIT, -2)) #2b) Lon = -2, give it a value of NA

#lat/long reversed, id: 	2022-20, 	2022-146 (display s. pole)
wyo_processed_locations2 <- wyo_processed_locations %>%  
  mutate(LAT_PERMIT2 = ifelse(SOURCE_ID == "2022-20" | SOURCE_ID == "2022-146", LON_PERMIT, LAT_PERMIT)) %>%
  mutate(LON_PERMIT2 = ifelse(SOURCE_ID == "2022-20" | SOURCE_ID == "2022-146", LAT_PERMIT, LON_PERMIT))  