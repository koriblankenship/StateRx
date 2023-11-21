### wtf ----
#what does source id come in as num for mt, or and nm? I specified as character in preprocessing
#straight line of points at ID border??????????????

#### TO DO ************* 
# few rows in OR w/ burn status = NA

library(tidyverse)
library(sf)

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


### PUT ALL PERMITS INTO 1 DF ----

binder <- bind_rows(az, ca, co, id, mt, nm, nv, or, ut, wa, wy) %>%
  mutate(burn_name_lower = tolower(BURN_NAME)) %>% # make burn name lower case 
  mutate(YEAR = year(DATE)) # make a year column


### PROCESS ----

## Remove 2023 and 2024 data (some WA data had a 2024 expiration date)
binder <- binder %>%
  filter(YEAR != "2024") %>%
  filter(YEAR != "2023")

## Classify Burn Type

# 1. join the xwalk to classify the burn types reported into standard classes
binder <- left_join(binder, burn_class)

# 2. use keywords found in burn name to classify rows where burn type is unknown

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


## Deal with multiple requests -- get 1 burn at a lat/lon per year and sum permit area
# if status = complete, sum completed area????????????????

# remove lat/lon = o and NA, remove yard waste
binder2 <- binder %>%
  filter(LAT_PERMIT > 0) %>%
  drop_na(LAT_PERMIT) %>%
  drop_na(LON_PERMIT) %>%
  filter(BURNTYPE_CLASSIFIED != "Yard waste")

#######*****************3D try 
#consider removing entity and doing the spatial combine w/ PAD

binder_unique2 <- binder2 %>%
  group_by(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_CLASSIFIED, LAT_PERMIT, LON_PERMIT) %>%
  summarise(SUM_PERMITTED = sum(ACRES_PERMITTED), across()) %>% # across retains all the rows
  summarise(SUM_REQUESTED = sum(ACRES_REQUESTED), across()) %>%
  summarise(SUM_COMPLETED = sum(ACRES_COMPLETED), across()) %>%
  summarise(ENTITY = max(ENTITY_REQUESTING), across()) %>%
  distinct(STATE, YEAR, SOURCE_ID, BURN_NAME, BURNTYPE_CLASSIFIED, LAT_PERMIT, LON_PERMIT,
           SUM_PERMITTED, SUM_REQUESTED, SUM_COMPLETED, ENTITY)

##############LOGIC CHECK#################
# how many unique locations in simple?
binder_unique5 <- binder_unique2 %>%
  distinct(STATE, YEAR, LAT_PERMIT, LON_PERMIT) 
# how many unique locations in binder2?
binder_unique6 <- binder2 %>%
  distinct(STATE, YEAR, LAT_PERMIT, LON_PERMIT)
############ WHY ARE THESE DIFFERENT ??


############ TEST LOGIC
#count number of unique BURN STATUS and BURN TYPES at a given point/year
test <- binder2 %>%
  group_by(STATE, YEAR, LAT_PERMIT, LON_PERMIT) %>%
  # burn status
  mutate(status_code = 0) %>%
  mutate(status_code = case_when(BURN_STATUS == "Complete" ~ status_code + 1000,
                                 BURN_STATUS == "Incomplete" ~ status_code + 100,
                                 BURN_STATUS == "Unknown" ~ status_code + 1,
                                 .default = status_code)) %>%
  # burn type
  mutate(bt_code = 0) %>%
  mutate(bt_code = case_when(BURNTYPE_CLASSIFIED == "Broadcast" ~ bt_code + 1000,
                             BURNTYPE_CLASSIFIED == "Unknown" ~ bt_code + 100,
                             BURNTYPE_CLASSIFIED == "Pile" ~ bt_code + 1,
                             .default = bt_code)) %>%
  #summarize
  summarise(sum_status = sum(status_code), across()) %>%
  summarise(sum_bt = sum(bt_code), across()) %>%
  # get unique rows
  distinct(STATE, YEAR, LAT_PERMIT, LON_PERMIT, sum_status, sum_bt)

# count burns in 1 place per year 
###****************might need a threshold for removing 
###**** exculde ID for now?
test2 <- binder2 %>%
  group_by(STATE, YEAR, LAT_PERMIT, LON_PERMIT) %>%
  summarise(count_burns = n()) 
####################### END LOGIC CHECK #############################


# limit by time and type
binder_unique2 <- binder_unique2 %>%
  filter(YEAR > 2016 & YEAR < 2023) %>% # common time for all states is 2017-2022
  filter(BURNTYPE_CLASSIFIED != "Yard Waste") ### did this above, keep the one above until the logic checking is done; 
# prefer removing yard waste here rather than above
# this might be the place to remove ID???
# clip to west????


# look at the df
write_csv(binder, "out/BINDER.csv")
write_csv(binder_unique2, "out/BINDER_unique2.csv")

# make it an sf object
# all points
binder_all <- binder %>%
  filter(LAT_PERMIT != 0) %>% # only sites with valid lat
  filter(LON_PERMIT != 0) %>% # only sites with valid lon
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs=4326, remove=FALSE)
# unique points
binder_unique2 <- binder_unique2 %>%
  filter(LAT_PERMIT != 0) %>% # only sites with valid lat
  filter(LON_PERMIT != 0) %>% # only sites with valid lon
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs=4326, remove=FALSE)


### SUMMARY INFO


# # permits/time
p_permits_time <- binder_unique2 %>%
  group_by(STATE, YEAR, BURNTYPE_CLASSIFIED) %>%
  distinct(LAT_PERMIT, LON_PERMIT) %>%
  summarise(count = n())
 
#  box plot of permited acres for broadcast over time
p_permited_acres_broadcast <- binder_unique2 %>%
  group_by(STATE, YEAR) %>%
  distinct(LAT_PERMIT, LON_PERMIT) %>%
  filter(BURNTYPE_CLASSIFIED = "Broadcast") %>%
  summarise(sum_ = ACRES_PERMITTED) %>%
  summarise(max_request = max(SUM_REQUESTED))

# fire size histogram for permited burns 
# percent of area permited and number of permits by entity (public vs. pvt), bar chart




## MAKE A MAP GRAPHIC ----
us <- map_data("state")
world <- map_data("world")

ggplot()+
  geom_map(data=world, map=world, 
           aes(map_id=region), fill="white", color="black") +
  geom_map(data=us, map=us, 
           aes(map_id=region), fill="white", color="black", size=.25) +
  geom_point(data=binder_sf, aes(x=LON_PERMIT, y=LAT_PERMIT), 
             alpha=.9, size=1, shape=19, color="blue") +
  coord_map("albers", lat0 = 39, lat1 = 60) + #****************************albers?????????????????
  xlab("") + ylab("")


## EXPORT  ***********MODIFY FOR RX
## png
ggsave("./Projects/LANDFIRE/Output/nafsn_sites.png", width=7, height=5, units='in', dpi=300)
## shp
st_write(binder_all, "out/shp/binder_all.shp", append = FALSE) #append set to overwrite existing data
st_write(binder_unique2, "out/shp/binder_unique2.shp", append = FALSE) #append set to overwrite existing data



#**********************************look at the warnings and try to address???????????????


# sample plots

#make a year variable for plotting
binder_plots <- binder %>%
  mutate(year = year(ymd(DATE)))
#summarize the entity info
#summarize burn type
#TO DO: Stacked bar w/ acres permitted/completed as fill


## Completed Acres, State, year
ggplot(data = binder_plots) +
  aes(x = year, y = COMPLETED_ACRES) + 
  geom_bar(position='stack', stat='identity') +
  facet_wrap(vars(STATE)) + 
  theme(axis.text.x = element_text(angle=90)) + #rotate x labels
  scale_x_discrete(limits = c(2010, 2015, 2020, 2022)) + # control x axis range
  scale_y_continuous(limits = c(0, 275000)) + # control x axis range
  theme(panel.background = element_blank(), #remove grey background
        text = element_text(size = 11, family = "A"))  #font size and style

## Permitted Acres, State, year
ggplot(data = binder_plots) +
  aes(x = year, y = PERMITTED_ACRES) + 
  geom_bar(position='stack', stat='identity') +
  facet_wrap(vars(STATE)) + 
  theme(axis.text.x = element_text(angle=90)) + #rotate x labels
  scale_x_discrete(limits = c(2010, 2015, 2020, 2022)) + # control x axis range
  scale_y_continuous(limits = c(0, 900000)) + # control x axis range
  theme(panel.background = element_blank(), #remove grey background
        text = element_text(size = 11, family = "A"))  #font size and style













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


