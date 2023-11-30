# Take pre-processed rx permit data from western states and put it into one database

#### TO DO ************* 


library(tidyverse)
#library(sf)
#library(tigris)

### BRING IN THE DATA ----

# pre processed permit data for each state
az <- read_csv("out/az_ready.csv") %>%
  mutate(STATE = "AZ")
ca <- read_csv("out/ca_ready.csv") %>%
  mutate(STATE = "CA")
co <- read_csv("out/co_ready.csv") %>%
  mutate(STATE = "CO")
id <- read_csv("out/id_ready_DRAFT.csv") %>% # DRAFT DATA 
  mutate(STATE = "ID")
mt <- read_csv("out/mt_ready.csv") %>% 
  mutate_at("SOURCE_ID", as.character) %>%
  mutate(STATE = "MT")
nm <- read_csv("out/nm_ready.csv") %>%
  mutate_at("SOURCE_ID", as.character) %>%
  mutate(STATE = "NM")
nv <- read_csv("out/nv_ready.csv") %>%
  mutate(STATE = "NV")
or <- read_csv("out/or_ready.csv") %>%
  mutate_at("SOURCE_ID", as.character) %>%
  mutate(STATE = "OR")
ut <- read_csv("out/ut_ready.csv") %>%
  mutate(STATE = "UT")
wa <- read_csv("out/wa_ready.csv") %>%
  mutate(STATE = "WA")
wy <- read_csv("out/wy_ready.csv") %>%
  mutate(STATE = "WY")

# burn type classification 
burn_class <- read_csv("in/burntypes_classified.csv") %>%
  select(BURNTYPE_REPORTED, BURNTYPE_CLASSIFIED)

# spatial data
# west <- states() %>%
#   filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
# #  st_transform(crs = "ESRI:102004") %>% # lamberts conformal conic
#   select(STUSPS)

#pad_west <- st_read("in/pad/pad_west.shp") ###########


### PUT ALL PERMITS INTO 1 DF ----

binder <- bind_rows(az, ca, co, id, mt, nm, nv, or, ut, wa, wy) %>%
  mutate(burn_name_lower = tolower(BURN_NAME)) %>% # make burn name lower case 
  mutate(YEAR = year(DATE)) # make a year column


### PROCESS ----

## Remove 2023 and 2024 data (some WA data had a 2024 expiration date)
binder <- binder %>%
  filter(YEAR != "2024") %>%
  filter(YEAR != "2023")

## Quick lat/lon adjustment
binder <- binder %>%
  mutate(edited = case_when(LON_PERMIT > 0 ~ 1,
                            LAT_PERMIT < 0 ~ 1,
                            .default = 0)) %>% #flag rows w/ incorrect sign
  mutate(LON_PERMIT = case_when(LON_PERMIT > 0 ~ LON_PERMIT * -1, #positive lon should be neg
                                .default = LON_PERMIT)) %>% 
  mutate(LAT_PERMIT = case_when(LAT_PERMIT < 0 ~ LAT_PERMIT * -1, #neg lat should be positive
                                .default = LAT_PERMIT)) 
#sum(binder$edited) # edited 449 rows

## Classify Burn Type
# use keywords found in burn name to classify rows where burn type is unknown
# join the xwalk to classify the burn types reported into standard classes
binder <- left_join(binder, burn_class)
# list the keywords for burn types
keywords_pile <- c("pile", "piles", "handpile", "handpiles")
keywords_broadcast <- c("broadcast", "natural fuel", "underburn", "understory", "prescribed burn", "rx burn")
# find keywords in the burn name
binder <- binder %>%
  rowwise() %>%
  mutate(is.pile = str_detect(burn_name_lower, paste0(keywords_pile, collapse = '|'))) %>%
  mutate(is.broadcast = str_detect(burn_name_lower, paste0(keywords_broadcast, collapse = '|')))
# classify unknown burn types based on the keywords in the burn name
binder <- binder %>%
  mutate(BURNTYPE_CLASSIFIED = case_when(
    BURNTYPE_CLASSIFIED == "Unknown" | is.na(BURNTYPE_CLASSIFIED) & is.pile == TRUE & is.broadcast == FALSE ~ "Pile", 
    BURNTYPE_CLASSIFIED == "Unknown" | is.na(BURNTYPE_CLASSIFIED) & is.pile == FALSE & is.broadcast == TRUE ~ "Broadcast", 
    .default = BURNTYPE_CLASSIFIED)) 
binder <- binder %>%
  mutate(BURNTYPE_CLASSIFIED = case_when(is.na(BURNTYPE_CLASSIFIED) ~ "Unknown",
                                         .default = BURNTYPE_CLASSIFIED))

## Adjust NA values in summary fields
# step 1: make a field to track if a record had the info
# step 2: change negative and NA values to 0
binder <- binder %>%
  # acres permitted
  mutate(na_permit = case_when(is.na(ACRES_PERMITTED) ~ "FALSE",
                                     .default = "TRUE")) %>%
  mutate(ACRES_PERMITTED = case_when(ACRES_PERMITTED < 0 ~ 0, # this catches any negative values
                             is.na(ACRES_PERMITTED) ~ 0,
                             .default = ACRES_PERMITTED)) %>%
  # acres requested
  mutate(na_request = case_when(is.na(ACRES_REQUESTED) ~ "FALSE",
                                     .default = "TRUE")) %>%
  mutate(ACRES_REQUESTED = case_when(ACRES_REQUESTED < 0 ~ 0, # this catches any negative values
                             is.na(ACRES_REQUESTED) ~ 0,
                             .default = ACRES_REQUESTED)) %>%
  # acres completed
  mutate(na_complete = case_when(is.na(ACRES_COMPLETED) ~ "FALSE",
                                     .default = "TRUE")) %>%
  mutate(ACRES_COMPLETED = case_when(ACRES_COMPLETED < 0 ~ 0, # this catches any negative values
                             is.na(ACRES_COMPLETED) ~ 0,
                             .default = ACRES_COMPLETED))


### GET UNIQUE BURNS ----

# limit by time and type
# removing known ag types here; other ag burns will be removed in spatial processing
# list ag burning types from ID:
list_ag <- c("Field Burn", "Crop Residue Disposal and CRP on Reservations", 
             "Crop Residue, Pasture, and CRP burning")
rx <- binder %>% 
  filter(YEAR > 2016 & YEAR < 2023) %>% # filter for years of interest
  filter(!(BURNTYPE_REPORTED %in% list_ag)) %>% # remove known ag types 
  filter(BURNTYPE_CLASSIFIED != "Yard waste") # remove yard waste
  #distinct(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_CLASSIFIED, LAT_PERMIT, LON_PERMIT) # unique records
#>>> 87,302 rows

# SUMMARY TABLE: count records by state
state_records <- table(rx$STATE)

# unique burns
rx <- rx %>%
  group_by(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_CLASSIFIED, LAT_PERMIT, LON_PERMIT) %>%
  summarise(SUM_PERMIT = sum(ACRES_PERMITTED), across()) %>% # across retains all the rows
  summarise(SUM_REQUEST = sum(ACRES_REQUESTED), across()) %>%
  summarise(MAX_REQUEST = max(ACRES_REQUESTED), across()) %>%
  summarise(SUM_COMPLETE = sum(ACRES_COMPLETED), across()) %>%
  summarise(ENTITY = max(ENTITY_REQUESTING), across()) %>%
  distinct(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_CLASSIFIED, LAT_PERMIT, LON_PERMIT,
           SUM_PERMIT, SUM_REQUEST, MAX_REQUEST, SUM_COMPLETE, ENTITY) %>%
  # acre_planned 
  mutate(ACRE_PLANNED = case_when(SUM_PERMIT > 0 ~ SUM_PERMIT,
                                  MAX_REQUEST > 0 ~ MAX_REQUEST, 
                                  .default =  SUM_COMPLETE))
#>>> 57,430 [63,148 rows, w/o ID ag remove]

# write out rx so it can be used in the spatial operations script
write_csv(rx, "out/rx.csv")







### THIS WAS MOVED, DELETE WHEN COMFORTABLE
#***********************************************************************************
### SPATIAL OPERATIONS ----
# To do: remove clip to west when lat/lon is fixed 

# crop to west
rx_west <- rx %>%
  drop_na(LAT_PERMIT) %>%
  drop_na(LON_PERMIT) %>%
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs = 4269) %>% # crs = west
  #st_transform(crs = "ESRI:102004") %>% # lamberts
  st_intersection(west) %>% # crop to west
  #st_drop_geometry () %>%
  select(-STUSPS) %>%
  #assign a unique id
  mutate(ID = row_number())

# buffer points by acres
#calc the burn buffer based on radius of burn area -- assuming circular sites

burn_radius <- rx_west %>%  
  # burn area is assumed to be completed, then permitted then max requested
  mutate(area_burn_ac = case_when(SUM_COMPLETE > 0 ~ SUM_COMPLETE,
                                     SUM_PERMIT > 0 ~ SUM_PERMIT,
                                     MAX_REQUEST > 0 ~ MAX_REQUEST)) %>%
  # when no acres, default values are given based on the burn type
  mutate(area_burn_ac = case_when(is.na(area_burn_ac) & BURNTYPE_CLASSIFIED == "Pile" ~ .5,
                                  is.na(area_burn_ac) & BURNTYPE_CLASSIFIED == "Broadcast" ~ 10,
                                  is.na(area_burn_ac) & BURNTYPE_CLASSIFIED == "Unknown" ~ .5,
                                  .default = area_burn_ac)) %>%
  # set an upper limit on the buffer size
  mutate(area_burn_4radius = case_when(area_burn_ac > 1000 ~ 1000,
                                       .default = area_burn_ac))
  # radius = square root of area/pi
  # use a conversion to convert the area to square meters (1 ac = 4046.85642 meters squared)
  mutate(radius = sqrt((area_burn_4radius * 4046.85642) / pi)) %>%
  select(ID, radius)
 
#buffer the sites 
rx_buf <- st_buffer(rx_west, burn_radius$radius) 

# export to check
#st_write(rx_buf, "out/shp/rx_buf.shp", append = FALSE) 
#st_write(rx_west, "out/shp/rx_west.shp", append = FALSE) 


# intersect with pad
# moving this to Arc b/c of issues w/ overlapping polygons
# export the rx west points to shape for use in Arc
st_write(rx_west, "out/shp/rx_west.shp", append = FALSE) 
# in arc overlay the points
# bring back in the attribute table from the overlay
# populate the null values as "NoPad"

########################################################## PAD
#this code should work with a clean flat pad file if I get one 
rx_pad <- rx_west %>%
  st_intersection(pad_west) %>%
  st_drop_geometry () %>%
  select(ID, MangName, MangType)
# join back the pad attribute 
rx_west2 <- left_join(rx_west, rx_pad)
###??? ~200 extra rows?
#pull out duplicates 
check_dups <- rx_west2 %>%
  group_by(ID) %>%
  summarise(count_id = n(), across()) %>%
  filter(count_id > 1)
  mutate(count_my = count_id) %>%
  select(count_my > 1)
# populate value for non-pad rows
##########################################################



### EXPORT ----  

  # export for plotting
  #write_csv(rx_west, "out/rx_west.csv")
  
## png
ggsave("./Projects/LANDFIRE/Output/nafsn_sites.png", width=7, height=5, units='in', dpi=300)
## shp
st_write(binder_all, "out/shp/binder_all.shp", append = FALSE) #append set to overwrite existing data
st_write(rx_west, "out/shp/rx_west.shp", append = FALSE) #append set to overwrite existing data

# df
write_csv(binder, "out/BINDER.csv")
write_csv(binder_unique2, "out/BINDER_unique2.csv")

# make it an sf object
# all points
binder_all <- binder %>%
  filter(LAT_PERMIT != 0) %>% # only sites with valid lat
  filter(LON_PERMIT != 0) %>% # only sites with valid lon
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs=4326, remove=FALSE)
# unique points
rx_sf <- rx %>%
  filter(LAT_PERMIT != 0) %>% # only sites with valid lat
  filter(LON_PERMIT != 0) %>% # only sites with valid lon
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs=4326, remove=FALSE)


##########archive

# get count by type error checking
# count burn type frequency based on xwalk 
(type_by_reported <- table(binder$BURNTYPE_CLASSIFIED))


# permit input files
#state_file_list <- list.files(path="state_data", pattern = ".csv", full.names=TRUE ) 
file_list <- list.files(path="out/", pattern = "_ready", full.names=TRUE )
###TC: SHOULD THIS BE MAPPED IN? OR JUST READ THE FILES IN A FOR LOOP?
state_data <- map(file_list, read_csv) #I use map here to bring in the permit data for each state (I have AZ and GA as test data; I made up GA)
state_data_dfr <- map_dfr(file_list, read_csv) 
#source id

binder <- rbind(state_data)
binder2 <- bind_rows(state_date)


#read all state ready data into 1 df
raw <- read_csv(file_list, id = "file_name") 
#remove spaces in column names
names(raw) <- make.names(names(raw), unique=TRUE) 

#make a list of all the "ready" data sheets in the StateRx/out folder
file_list <- list.files(path="out/", pattern = "_ready", full.names=TRUE ) 

#try lapply
test_read <- lapply(file_list, read_csv)
test_rbind <- rbind(test_read)

type(test_read)

filenames <- list.files(full.names=TRUE)  
All <- lapply(filenames,function(i){
  read.csv(i, header=TRUE, skip=4)
})


### figuring out unique burns

#######*****************2ND try 
binder_unique2 <- binder2 %>%
  group_by(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_CLASSIFIED, LAT_PERMIT, LON_PERMIT) %>%
  summarise(SUM_PERMITTED = sum(ACRES_PERMITTED), across()) %>% # across retains all the rows
  summarise(SUM_REQUESTED = sum(ACRES_REQUESTED), across()) %>%
  summarise(SUM_COMPLETED = sum(ACRES_COMPLETED), across()) %>%
  distinct(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_CLASSIFIED, LAT_PERMIT, LON_PERMIT, .keep_all = TRUE) %>% # keep all columns here
  select(c(STATE, YEAR, SOURCE_ID, BURN_NAME, LAT_PERMIT, LON_PERMIT,
           SUM_PERMITTED, SUM_REQUESTED, SUM_COMPLETED, ENTITY_REQUESTING)) #select columns I want



#######*****************1st try
# state group a: has permitted and completed
stategroup_a <- c("CO", "NM", "WA", "WY")
# state group b: has completed
stategroup_b <- c("CA", "MT", "OR", "UT")
# request only ************************add NV here

# permitted and completed
binder_perm <- binder %>%
  filter(STATE %in% stategroup_a) %>%
  group_by(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_REPORTED, LAT_PERMIT, LON_PERMIT) %>%
  summarise(SUM_PERMIT_ACRES = sum(PERMITTED_ACRES), across()) %>% # across retains all the rows
  summarise(SUM_COMPLETED_ACRES = sum(COMPLETED_ACRES), across()) %>%
  distinct(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_REPORTED, LAT_PERMIT, LON_PERMIT, .keep_all = TRUE) # distinct will remove duplicate records, .keep all keeps all columns
# completed only
binder_comp <- binder %>%
  filter(STATE %in% stategroup_b) %>%
  group_by(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_REPORTED, LAT_PERMIT, LON_PERMIT) %>%
  summarise(SUM_COMPLETED_ACRES = sum(COMPLETED_ACRES), across()) %>%
  distinct(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_REPORTED, LAT_PERMIT, LON_PERMIT, .keep_all = TRUE) 

# put it back together
binder_unique <- bind_rows(binder_perm, binder_comp) %>%
  select(-c(PERMITTED_ACRES, COMPLETED_ACRES, LEGAL_DESCRIP, TONS, PILE_VOLUME, burn_name_lower, is.pile, is.broadcast))


### SPATIAL OPERATIONS ----

# >>> remove clip to west when lat/lon is fixed *************

# there is an issue w/ the spherical geometry of PAD
# code below helps with that
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
#sf::sf_use_s2(FALSE) 

### 111
rx_west <- rx %>%
  drop_na(LAT_PERMIT) %>%
  drop_na(LON_PERMIT) %>%
  #st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), "ESRI:102004") %>%
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs = 4326) %>%
  st_transform(crs = "ESRI:102004") %>%
  st_intersection(west) %>% # crop to west
  st_intersection(pad_west) %>% # intersect with pad
  st_drop_geometry () %>%
  select(-STUSPS)

#333333
# >>> intersect pad, join, clip <<<
rx_west1 <- rx %>%
  drop_na(LAT_PERMIT) %>%
  drop_na(LON_PERMIT) %>%
  #st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), "ESRI:102004") %>%
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs = 4326) %>%
  st_transform(crs = "ESRI:102004") 
#crop here?
rx_pad <- rx_west1 %>%
  st_intersection(pad_west) %>% # intersect with pad
  st_drop_geometry () %>%
  select(ID, MangName, MangType)
### make a df and join back the pad attribute 
rx_west2 <- left_join(rx_west1, rx_pad)
#crop to west

