### wtf ----
#what does source id come in as num for mt, or and nm? I specified as character in preprocessing

library(tidyverse)
library(sf)

### BRING IN THE DATA ----


az <- read_csv("out/az_ready.csv") %>%
  mutate(STATE = "AZ")
ca <- read_csv("out/ca_ready.csv") %>%
  mutate(STATE = "CA")
co <- read_csv("out/co_ready.csv") %>%
  mutate(STATE = "CO")
id <- read_csv("out/id_ready_DRAFT.csv") %>%
  mutate(STATE = "ID")
mt <- read_csv("out/mt_ready_DRAFT.csv") %>%
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

binder <- bind_rows(az, ca, co, id, mt, nm, nv, or, ut, wa, wy)

#burn type reported -- ask T if we want to do this ********************
# binder <- binder %>%
#   mutate(BURNTYPE_REPORTED = case_when(is.na(BURNTYPE_REPORTED) ~ "Not reported"))

# burn type classified
burntypes <- table(binder$BURNTYPE_REPORTED) 
burntypes <- as.data.frame(burntypes) 

burntype_range <- binder%>% 
  filter(BURNTYPE_REPORTED == "Rangeland" | 
           BURNTYPE_REPORTED == "Rangeland (not cultivated or seeded in the last 10 years)" | 
           BURNTYPE_REPORTED == "Range")

# in OR ("Rangeland") - can get more info on type from burn name
#look at burntypes reported in OR
burntype_or <- binder%>% 
  filter(STATE == "OR") %>%
  distinct(BURNTYPE_REPORTED)
#look at rangeland burn rows in OR
burntype_or_rangeland <- binder %>%
  filter(STATE == "OR") %>%
  filter(BURNTYPE_REPORTED == "Rangeland")
#test classification
keywords_pile <- c("pile", "piles", 
                   "Pile", "Piles",
                   "handpile", "handpiles",
                   "Handpile", "Handpiles",
                   "jackpot", "jackpots",
                   "Jackpot", "Jackpots") #see if I can designate any case combination, upper, lower, sentence, etc
#find one type
burntype_or_rangeland <- burntype_or_rangeland %>%
  rowwise() %>%
  mutate(is.pile = str_detect(BURN_NAME, "Handpiles")) # this finds one
#find list of types
burntype_or_rangeland2 <- burntype_or_rangeland %>%
  rowwise() %>%
  mutate(is.pile = str_detect(BURN_NAME, paste0(keywords_pile, collapse = '|'))) 



# in ID ("Rangeland (not cultivated or seeded in the last 10 years)") - no burn name 
burntype_id <- binder%>% 
  filter(STATE == "ID") %>%
  distinct(BURNTYPE_REPORTED)


write_csv(burntypes, "in/burntypes.csv")




# look at the df
write_csv(binder, "out/BINDER.csv")


# make it a shp

binder_sf <- binder %>%
  filter(LAT_PERMIT != 0) %>% # only sites with valid lat
  filter(LON_PERMIT != 0) %>% # only sites with valid lon
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs=4326, remove=FALSE)


## MAKE A MAP GRAPHIC
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
# create a directory for the shp files if it doesn't already exist 
# ifelse(!dir.exists("./Projects/LANDFIRE/Output/nafsnshp/"), 
#        dir.create("./Projects/LANDFIRE/Output/nafsnshp/"), FALSE)

st_write(binder_sf, "out/shp/binder.shp", append = FALSE) #append set to overwrite existing data
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
