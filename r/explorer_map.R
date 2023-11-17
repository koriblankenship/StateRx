### wtf ----
#what does source id come in as num for mt, or and nm? I specified as character in preprocessing

library(tidyverse)
library(sf)
library(tigris)
library(leaflet)


### BRING IN THE DATA ----

burns_unique <- read_csv("out/BINDER_unique.csv")


### MAP LAYERS ----

west <- states() %>%
  filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
  st_transform(crs = 4326)


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

# Discrete palette
pal <- colorFactor("viridis", levels = burns_west$BURNTYPE_CLASSIFIED)


## basic burn type
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = burns_west,
                   #weight = 1,
                   #fillColor = "blue",
                   radius = .25,
                   color = ~pal(BURNTYPE_CLASSIFIED)) %>%
  addLegend(data = burns_west,
            position = "bottomright",
            pal = pal, values = ~BURNTYPE_CLASSIFIED,
            title = "Burn Type",
            opacity = 1)


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
