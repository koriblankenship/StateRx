### wtf ----
#what does source id come in as num for mt, or and nm? I specified as character in preprocessing

library(tidyverse)
library(sf)
library(tigris)
library(leaflet)


### BRING IN THE DATA ----

burns_unique <- read_csv("out/BINDER_unique2.csv")
  # select(c(STATE, YEAR, BURN_NAME, LAT_PERMIT, LON_PERMIT,
  #          SUM_COMPLETED, SUM_REQUESTED, SUM_PERMITTED, DATE, ENTITY_REQUESTING,
  #          BURN_STATUS, BURNTYPE_CLASSIFIED))

### MAP LAYERS ----

west <- states() %>%
  filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
  st_transform(crs = 4326) %>%
  select(STUSPS)


### CLIP TO WEST ---- (this can be removed when lat/lon is fixed)
burns_west <- burns_unique %>%
  drop_na(LAT_PERMIT) %>%
  drop_na(LON_PERMIT) %>%
  st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs = 4326) %>%
  st_intersection(west) #crop to west


#check projections are the same
st_crs(west)==st_crs(burns_west) 


### MAP IT ----

###>>> https://r-charts.com/spatial/interactive-maps-leaflet/#google_vignette

popup <- paste(
  "Year:", burns_west$YEAR, "<br>",
  "Burn Name:", burns_west$BURN_NAME, "<br>",
  "Burn Type:", burns_west$BURNTYPE_CLASSIFIED, "<br>", 
  #"Burn Status", burns_west$BURN_STATUS, "<br>",
  "Entity Requesting:", burns_west$ENTITY, "<br>",
  "Acres Completed:", burns_west$SUM_COMPLETED, "<br>",
  "Acres Permitted:", burns_west$SUM_PERMITTED,  "<br>")

# Discrete palette
pal <- colorFactor("viridis", levels = burns_west$BURNTYPE_CLASSIFIED)


## basic burn type
leaflet() %>%
  addTiles() %>%
  setView(-114.029336, 42.087209, zoom = 5.25) %>% #***try 5 (5.5 was too close***
  addCircleMarkers(data = burns_west,
                   #weight = 1,
                   #fillColor = "blue",
                   radius = .25,
                   color = ~pal(BURNTYPE_CLASSIFIED),
                   popup = ~popup) %>%
  addLegend(data = burns_west,
            position = "bottomright",
            pal = pal, values = ~BURNTYPE_CLASSIFIED,
            title = "Burn Type",
            opacity = 1) 
# save as html
# experot in viewer as webpage

# build index file
#open a new script and save

# stage comit push
# on git for this repo, set pages settings
# wait, then get url to share

## basic completed vs. not?



### ARCHIVE

# conus <- states() %>%
#   filter(STUSPS %in% c("AZ", "CA", "CO", "ID", "MT", "NV", "OR")) %>%
#   st_transform(crs = 4326)


# get coords
#lass(burns_west)
burns_west_map <- burns_west %>%
  mutate(lng = unlist(map(burns_west$geometry,1)),
         lat = unlist(map(burns_west$geometry,2)))

#class(separated_coord)
