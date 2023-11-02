#CHECK MY LAT/LONG

library(tidyverse)
library(sf)
library(tigris)

### SET UP
#put in the 2 character alpha code for the state you want to check
state_code <- "wy"
#enter relative path to the state permit data w/ lat/lon you want checked
permit_path <- paste0("out/", state_code, "_ready.csv")
#permit_path <- "out/wyo_ready.csv"


### DATA

#state boundary
state_poly <- states() %>%
  filter(STUSPS %in% c(state_code)) %>%
  select(STUSPS, geometry)

#state permit data
state_permit <- read_csv(permit_path) %>%
  drop_na(LAT_PERMIT) %>% #remove lat = na %>%
  drop_na(LON_PERMIT) %>% #remove lon = na %>%
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs = 4326) %>% 
  st_transform(st_crs(state_poly)) #match state prj

#check projections are the same
st_crs(state_poly)==st_crs(state_permit) 


### FIND POINTS OUTSIDE OF THE STATE BOUNDARY

out_state <- st_difference(state_permit, state_poly)

#PLOT(state_permit)

### MAKE INITIAL CORRECTIONS 

#make a df and pull out lat/lon fields to edit
out_state_edit1 <- out_state %>%
  mutate(lon = unlist(map(out_state$geometry,1)),
         lat = unlist(map(out_state$geometry,2))) %>%
  select(SOURCE_ID, lon, lat)

#edit values
out_state_edit1 <- out_state_edit1 %>%
  mutate(LON_EDIT = ifelse(lon > 0, lon * -1, lon)) %>% #positive lon should be negative
  mutate(EDITED = ifelse(lon > 0, 1, 0)) %>% #flag edited rows
  mutate(LAT_EDIT = ifelse(lat < 0, lat * -1, lat)) %>% #negative lat should be positive
  mutate(EDITED = ifelse(lat < 0, EDITED + 1, EDITED)) #flag edited rows

#make the edited table an sf object
out_state2 <- out_state_edit1 %>%
  st_as_sf(coords = c("LON_EDIT", "LAT_EDIT"), crs = 4326) %>% 
  st_transform(st_crs(state_poly)) #match state prj


### FIND POINTS STILL OUTSIDE OF THE STATE BOUNDARY

out_state2_new <- st_difference(out_state2, state_poly) ###FIGURE OUT BETTER NAMES!!!!!!!!!!
#out_state2

#THE "CORRECTED" COORDINATES STILL HAVE THE SAME NUMBER OUTSIDE OF THE STATE????
ggplot() + 
  geom_sf(data = out_state2_new) +
  geom_sf(data = state_poly) 
#MAKE A SHP OF CORRECTED COORDINATES AND LOOK AT THEM IN GIS



### LIST OF LAT/LONG ISSUES TO FIX
#reversed sign (neg/pos)
#lat or lon missing
#values of 0, NA, NaN




  
### DO A FIRST PASS THEN LOOK AT REMAINING ISSUES
#if EDITED = 0 then there is an issue w/ the lat/long that was not fixed above
#review these for making hand corrections

### REDO ST_DIFFERENCE

### MARK BAD LAT/LONS somehow in the "ready" data?

#### deal w/ NAs 
# some NAs were dropped when I made an sf object, add those back in  or just add edited info for flagged records?

# DO THIS CHECK LATER B/C NAs are removed when making an sf object
#   mutate(EDITED = ifelse(is.na(lon), EDITED + 1, EDITED)) %>% #lon is missing
#   mutate(EDITED = ifelse(is.na(lat), EDITED + 1, EDITED)) #lat is missing
#############NAs

  


#try the intersection
intersect <- st_intersection(state_permit, state_poly)
layername_intersect <- paste0("gis/intersect", state_code, ".shp")
st_write(intersect, layername_intersect,
         append = FALSE) #append set to overwrite existing data






### EXPORT SHP 

layername <- paste0("gis/out_state", state_code, ".shp") #make a name for the output shp
st_write(out_state, layername,
         append = FALSE) #append set to overwrite existing data
         

### PLOT
ggplot() + 
#  geom_sf(data = intersect) +
  geom_sf(data = out_state2_new) +
#  geom_sf(data = state_permit) +
  geom_sf(data = state_poly) 

