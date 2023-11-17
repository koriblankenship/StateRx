#This script takes WA permit and request data and processes it so it is ready to be used in the Rx database
#In WA permits are issued for burns then requests are made if a burner wants to burn >100 tons on a permit
#in this script:
#permits are combined with requests then the data are split into permits w/o requests and permits w/ requests 
#processing is done a little differently on permits w/ and w/o requests
#after processing, permits w/ and w/o requests are joined to create a master table ready to add to the Rx database


library(tidyverse)

### BRING IN DATA

#permit data
allexpired <- read_csv("in/WA_AllExpiredPermits05122023.csv")
allvalid <- read_csv("in/WA_AllValidPermits05122023.csv")
#burn request data
burnrequest <- read_csv("in/WA_BurnRequestExport.csv") %>%
  select(BurnPermitNumber, BurnRequestId, PostBurnDate, PlannedIgnitionDate, ProposedBurnArea, BurnAcres, PostBurnArea, 
         BurnType, Agency, Latitude, Longitude, TotalProposedBurnQuantity)


### PREPROCESS PERMIT DATA

#bind data from expired and valid permits
burnpermit <- bind_rows(allexpired, allvalid) %>%
  rename("PermitNumber" = "Permit Number") %>% #remove space so it is easier to call this column later
  rename("LatLong" = "Lat, Long") %>%
  rename("BurnType_permit" = "Burn Type(s)") %>% #distinguish from burn type field in the burnrequest data
  rename("Agency_permit" = "Agency") %>% #distinguish from the agency column in burnrequest data
  separate(LatLong, c("Lat", "Long"), sep = ",") %>% #split that lat/long field 
  select("PermitNumber", "Expiration Date", "Harvest Acres", "Unit Name", "BurnType_permit", "Agency_permit", 
         "Lat", "Long", "Legal Description", "Est. Permit Tonnage")

#remove words in these fields and make them numeric: Est. Permit Tonnage, Harvest Acres
burnpermit <- burnpermit %>%
  mutate_at("Est. Permit Tonnage", str_replace, "tons", "") %>%
  mutate_at("Est. Permit Tonnage", as.numeric) %>%
  mutate_at("Harvest Acres", str_replace, "acres", "") %>%
  mutate_at("Harvest Acres", as.numeric) 

#****
#ERROR CHECK: 
#look for duplicate permits
#ERROR CHECK: if this value equals the number of obs in the permit df then there are no duplicate permit numbers in the permit df
CountPermitNumbers <- n_distinct(burnpermit$PermitNumber) 
#****

### SEPERATE PERMITS INTO THOSE W/ AND W/O BURN REQUESTS

#join the burn requests to the permits 
permit_request_fulljoin <- full_join(burnpermit, burnrequest, by = c("PermitNumber" = "BurnPermitNumber")) 

#Permits only - remove rows with burn request
permit_only <- permit_request_fulljoin %>%
  filter(is.na (BurnRequestId))

#Permits with requests - keep only rows with burn request
permit_with_request <- permit_request_fulljoin %>%
  filter(BurnRequestId > 0)

#****
#ERROR CHECK: does the permit_request_fulljoin have the same number of unique permits as the burnpermit df? 
CountPermitNumbersJoin <- n_distinct(permit_request_fulljoin$PermitNumber)
#there are ~60 additional permit numbers in the joined data - there must be some requests for burn permit numbers that are not found in the permits df
#the difference is small and I have no way to rectify missing data
#count unique permits in the permits data
CountPermitsPermits <- n_distinct(burnpermit$PermitNumber)
#count unique permit #s in the requests 
CountPermitsRequest <- n_distinct(burnrequest$BurnPermitNumber)
# there are more permit numbers in permit data which makes sense b/c not every permit has requests
#****


### PROCESS: permit only

#xwalk to the rx database column names
permit_only_processed <- permit_only %>%
  rename("SOURCE_ID" = "PermitNumber") %>%
  rename("DATE" = "Expiration Date") %>% #this is the only date in the permit, see date below for more info
  rename("ACRES_PERMITTED" = "Harvest Acres") %>%
  rename("BURN_NAME" = "Unit Name") %>%
  rename("BURNTYPE_REPORTED" = "BurnType_permit") %>%
  rename("ENTITY_REQUESTING" = "Agency_permit") %>%
  rename("LAT_PERMIT" = "Lat") %>%
  mutate_at("LAT_PERMIT", as.numeric) %>%
  rename("LON_PERMIT" = "Long") %>%
  mutate_at("LON_PERMIT", as.numeric) %>%
  rename("LEGAL_DESCRIP" = "Legal Description") %>%
  rename("TONS" = "Est. Permit Tonnage")

# date
# the minimum time that a permit is valid for is generally 1 year.  
# Burn permits for more than 100 tons are typically issued for 2 years.
# date was set to Expiration Date above and I adjust here to estimate the issue date by subtracting 
# 1 year from the Expiration Date. I didn't sort out permits > 100 tons which can be issued for 2 years.
permit_only_processed$DATE <- dmy(permit_only_processed$DATE) #format the Date column as a date
permit_only_processed$DATE <-  permit_only_processed$DATE %m-% years(1) 

#burn status 
permit_only_processed <- permit_only_processed %>%
  mutate(BURN_STATUS = case_when(is.na(TONS) ~ "Unknown", #don't know type if tons is NA
                                 TONS < 100 ~ "Unknown", #for tons < 100 a request is not required, so there is no info on burn status
                                 TONS >= 100 ~ "Incomplete")) #incomplete b/c tons >= 100 & the permit has no requests

### PROCESS: permit w/ request

#xwalk to the rx database column names
permit_with_request_processed <- permit_with_request %>%
  rename("SOURCE_ID" = "PermitNumber") %>%
  rename("DATE" = "PlannedIgnitionDate") %>%
  rename("ACRES_REQUESTED" = "ProposedBurnArea") %>%
  # Harvest Acres comes from the permit and is the most reliable estimate of the intended acreage
  # there are a small portion of burns that end up with completed acres that far exceed permitted 
  # so this calculation is a the best approximation, not perfect; 
  rename("ACRES_PERMITTED" = "Harvest Acres") %>% 
  rename("ACRES_COMPLETED" = "PostBurnArea") %>%
  rename("BURN_NAME" = "Unit Name") %>% 
  rename("BURNTYPE_REPORTED" = "BurnType") %>%
  rename("ENTITY_REQUESTING" = "Agency") %>%
  rename("LAT_PERMIT" = "Latitude") %>%
  mutate_at("LAT_PERMIT", as.numeric) %>%
  rename("LON_PERMIT" = "Longitude") %>%
  mutate_at("LON_PERMIT", as.numeric) %>%
  rename("LEGAL_DESCRIP" = "Legal Description") %>%
  rename("TONS" = "TotalProposedBurnQuantity")

# burn status 
permit_with_request_processed <- permit_with_request_processed %>%
  mutate(BURN_STATUS = case_when(is.na(ACRES_COMPLETED) ~ "Unknown", 
                                 ACRES_COMPLETED == 0 ~ "Incomplete",
                                 ACRES_COMPLETED > 0 ~ "Complete"))
# date
permit_with_request_processed$DATE <- mdy_hm(permit_with_request_processed$DATE) #parse the existing date format
permit_with_request_processed <- permit_with_request_processed %>% #change date format from POSIXct to ymd
  mutate(DATE = ymd(DATE)) 


### BIND PERMIT AND REQUEST DATA BACK TOGETHER

wa_ready <- bind_rows(permit_only_processed, permit_with_request_processed) %>%
  select(any_of(c("SOURCE_ID", "DATE", "ACRES_REQUESTED", "ACRES_PERMITTED", "ACRES_COMPLETED", "PILE_VOLUME", 
                  "BURN_NAME", "BURNTYPE_REPORTED", "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", 
                  "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()

#"correct" clearly erroneous lat/lons
wa_ready <- wa_ready %>%         
  mutate(LON_PERMIT = case_when(LON_PERMIT > 0 ~ LON_PERMIT * -1, #positive lon is in China, making them negatives moves them to WA
                                LON_PERMIT < 0 ~ LON_PERMIT)) %>%
  mutate(LAT_PERMIT = case_when(LAT_PERMIT == 84.9723 ~ 48.9723, #lat of 84 is way N; probably should be 48
                                LAT_PERMIT != 84.9723 ~ LAT_PERMIT)) %>%
  mutate(LAT_PERMIT = na_if(LAT_PERMIT, 0)) %>% #if Lat = 0, give it a value of NA
  mutate(LON_PERMIT = na_if(LON_PERMIT, 0)) #if Lon = 0, give it a value of NA

  
###EXPORT

write_csv(wa_ready, "out/wa_ready.csv")


### ARCHIVE

#legals

#remove legal description when a lat/long is given  [[Decide what to do with legals]]
#wa_ready <- wa_ready %>%
#  mutate(LEGAL_DESCRIP = replace(LEGAL_DESCRIP, which(LAT_PERMIT > 0), NA))


# look at the difference between the Harvest Acres in the permit and the Sum of the reported post burn area for each distinct permit

TEST2 <- permit_request_fulljoin %>%
  rename(Harvest.Acres = "Harvest Acres") %>%
  group_by(PermitNumber) %>%
  mutate(SUM_BURNAREA = sum(PostBurnArea)) %>%
  mutate(DIFF = Harvest.Acres - SUM_BURNAREA) %>%
  ungroup() %>%
  distinct(PermitNumber, Harvest.Acres, SUM_BURNAREA, DIFF)


#compare lat/long between permit and request
permit_request2 <- permit_request %>%
  filter(!is.na(Latitude)) %>%
  mutate(CHECK = (Lat == Latitude))
permit_request2 %>% group_by(CHECK) %>% summarize(count = n())
#232 times out of 17585 the Latitude in burn request is different than the Lat in the permit