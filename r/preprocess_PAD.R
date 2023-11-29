library(tidyverse)
library(sf)
library(tigris)

# west states for clip
west <- states() %>%
  filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
  st_transform(crs = "ESRI:102004") %>%
  select(STUSPS)
st_crs(west)

# manager name to agency name xwalk
agency_name <- read_csv("in/pad/Agency_Name.csv")

# pad 3 fee shp
pad <- st_read("in/pad/pad3fee.shp") %>%
  #st_transform(west) %>%
  st_transform(crs = "ESRI:102004") %>% # projection
  
#pad2 <- st_intersection(st_geometry(pad), st_geometry(west))

# check projection
st_crs(pad)

# join the agency name
pad <- left_join(pad, agency_name, by = c("Mang_Name" = "Code")) 

# select final columns
pad <- pad %>%
  select("Manager Name", "Manager Type") %>%
  rename(MangName = "Manager Name") %>%
  rename(MangType = "Manager Type")

#keep as sf????

# export shp
st_write(pad, "in/pad/pad_west.shp", 
         append = FALSE) #append set to overwrite existing data
