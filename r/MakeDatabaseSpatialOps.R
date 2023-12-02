library(tidyverse)
library(terra)
library(sf)
library(tigris)

### BRING IN THE DATA ----

# lifeform raster 
life <- rast("in/raster/lifeform.tif") 

# pad raster & agency name xwalk table
pad <- rast("in/raster/pad.tif") 
pad_dod_tribe <- rast("in/raster/pad_dod_trib.tif")
#agency <- read_csv("in/raster/Agency_Name.csv") %>% ### may not need this
#  select(-c(OID_, Code, Dom))

# western states
west <- states() %>%
  filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
  st_transform(st_crs(life)) %>% # match projection
  select(STUSPS)

# rx database w/ clip to west
rx_west <- read_csv("out/rx.csv") %>% #> 57,430 rows
  # remove na permit values
  drop_na(LAT_PERMIT) %>%
  drop_na(LON_PERMIT) %>%
  # make sf object
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs = 4269) %>% # crs = west
  # match projection
  st_transform(st_crs(life)) %>% 
  # clip to west
  st_intersection(west) %>% 
  # drop some values
  select(-STUSPS) %>%
  #assign a unique id
  mutate(ID = row_number()) #> 57,091


### BUFFER THE RX POINTS ----
# buffer points by acres burned

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
                                       .default = area_burn_ac)) %>%
# radius = square root of area/pi
# use a conversion to convert the area to square meters (1 ac = 4046.85642 meters squared)
mutate(radius = sqrt((area_burn_4radius * 4046.85642) / pi)) %>%
  select(ID, radius)

#buffer the sites 
rx_buf <- st_buffer(rx_west, burn_radius$radius) 

# export to check
#st_write(rx_buf, "out/shp/rx_buf.shp", append = FALSE) 
#st_write(rx_west, "out/shp/rx_west.shp", append = FALSE) 


### EXTRACT MAX PAD & LIFEFORM VALUES FROM BUFFERED POINTS 

# extract lifeform
extract_life <- terra::extract(life, vect(rx_buf)) %>% 
  count(ID, EVT_LF_1) %>% # count lifeform by rx burn ID
  group_by(ID) %>%  #group by rx burn id
  slice(which.max(n)) %>% #retains only the row w/ the highest count for each ID/value combo
  ungroup() %>%
  rename(lifeform = EVT_LF_1)

# extract pad
# this is the fee data and supplies most of the managment info we need.
# a few important DOD facilities and American Indian Lands are not included.
extract_pad <- terra::extract(pad, vect(rx_buf)) %>% 
  count(ID, Manager_Na) %>% # count pad manager by rx burn ID
  group_by(ID) %>%  #group by rx burn id
  slice(which.max(n)) %>% #retains only the row w/ the highest count for each ID/value combo
  ungroup() %>%
  rename(Manager_Name = Manager_Na)

# extract pad - dod_trib
# this is designation data for dod and tribal lands that are not present in the fee data
extract_pad_dod_tribe <- terra::extract(pad_dod_tribe, vect(rx_buf)) %>% 
  count(ID, Mang_Name) %>% # count pad manager by rx burn ID
  group_by(ID) %>%  #group by rx burn id
  slice(which.max(n)) %>% #retains only the row w/ the highest count for each ID/value combo
  ungroup()

#join the  attributes to rx_west
#max pad and lifeform and dod/tribe
rx_west <- rx_west %>%
  bind_cols(extract_life %>%
              select(-ID, -n)) %>%
  bind_cols(extract_pad %>%
              select(-ID, -n)) %>%
  bind_cols(extract_pad_dod_tribe %>%
              select(-ID, -n))
# update the pad manager field w/ dod and tribe only on what is considered private in pad
rx_west <- rx_west %>%
  mutate(Manager_Name = case_when(Manager_Name == "Private" & Mang_Name == "DOD" 
                                 ~ "Department of Defense",
                                 Manager_Name == "Private" & Mang_Name == "TRIB"
                                 ~ "American Indian Lands",
                                 .default = Manager_Name)) %>%
  select(-Mang_Name) # get rid of the column with the dod/tribe info
# classify manager type
rx_west <- rx_west %>%
  mutate(Manager_Type = case_when(Manager_Name == "State" ~ "State",
                                  Manager_Name == "Private" ~ "Private", 
                                  Manager_Name == "Forest Service" |
                                    Manager_Name == "Bureau of Land Management" |
                                    Manager_Name == "Fish and Wildlife Service" |
                                    Manager_Name == "National Park Service" |
                                    Manager_Name == "Department of Defense" |
                                    Manager_Name == "Other Federal" ~ "Federal",
                                  .default = "Other"))
# classify manager federal
rx_west <- rx_west %>%
  mutate(Manager_Fed = case_when(Manager_Type == "Federal" ~ Manager_Name,
                                 .default = "Not Federal"))
                              

### REMOVE AG POINTS
#> 57091
# ag_by_state <- rx_west %>%
#   filter(lifeform == "Agriculture") #>> 569 rows
# identify ag burns based on lifeform and ownership
rx_west <- rx_west %>%
  mutate(ag_remove = case_when(STATE == "ID" & lifeform == "Agriculture" & Manager_Name == "Private" ~ "remove",
                               STATE == "WA" & lifeform == "Agriculture" & Manager_Name == "Private" ~ "remove",
                               .default = "keep"))
# ag_remove <- rx_west2 %>%
#   filter(ag_remove == "remove") %>%
#   group_by(STATE) #>> 481 rows
# remove the ag burns
rx_west <- rx_west %>%
  filter(ag_remove == "keep") %>%
  select(-ag_remove)

### EXPORT ----  

## shp
st_write(rx_west, "out/shp/rx_west.shp", append = FALSE) #append set to overwrite existing data

# df
rx_west <- rx_west %>%
  st_drop_geometry %>%
  write_csv("out/rx_west.csv")
