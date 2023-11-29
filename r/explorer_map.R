library(tidyverse)
library(sf)
#library(tigris)
library(leaflet)


### BRING IN THE DATA ----
#df <- read_csv("out/rx_west.csv") #> 57,091 rows
burns_west <- st_read("out/shp/rx_west.shp") %>% # 57,091 rows
  st_transform(crs = 4326) # try going back to decimal degrees for mapping

#burns_unique <- read_csv("out/BINDER_unique2.csv")
  # select(c(STATE, YEAR, BURN_NAME, LAT_PERMIT, LON_PERMIT,
  #          SUM_COMPLETED, SUM_REQUESTED, SUM_PERMITTED, DATE, ENTITY_REQUESTING,
  #          BURN_STATUS, BURNTYPE_CLASSIFIED))

### MAP LAYERS ----

# west <- states() %>%
#   filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
#   st_transform(crs = 4326) %>%
#   select(STUSPS)


### CLIP TO WEST ---- (this can be removed when lat/lon is fixed)
# burns_west <- burns_unique %>%
#   drop_na(LAT_PERMIT) %>%
#   drop_na(LON_PERMIT) %>%
#   st_as_sf(coords = c("LON_PERMIT", "LAT_PERMIT"), crs = 4326) %>%
#   st_intersection(west) #crop to west


#check projections are the same
#st_crs(west)==st_crs(burns_west) 



### MAP IT ----

###>>> https://r-charts.com/spatial/interactive-maps-leaflet/#google_vignette

popup <- paste(
  "Year:", burns_west$YEAR, "<br>",
  "Burn Name:", burns_west$BURN_NA, "<br>",
  "Burn Type:", burns_west$BURNTYP, "<br>", 
  "Entity Requesting:", burns_west$ENTITY, "<br>",
  "Manager Name:", burns_west$Mngr_Nm, "<br>",
  "Cover Type:", burns_west$lifefrm, "<br>",
  "Acres Permitted:", burns_west$SUM_PER, "<br>",
  "Acres Max Requested:", burns_west$MAX_REQ, "<br>",
  "Acres Completed:", burns_west$SUM_COM, "<br>")

# Discrete palette
pal <- colorFactor("viridis", levels = burns_west$BURNTYP)


## basic burn type
leaflet() %>%
  addTiles() %>%
  #setView(-114.029336, 42.087209, zoom = 5.25) %>% #***try 5 (5.5 was too close***
  setView(-114.7358292, 41.2557021, zoom = 5.4) %>% # 5.25 is too far out
  addCircleMarkers(data = burns_west,
                   radius = .25,
                   color = ~pal(BURNTYP),
                   popup = ~popup) %>%
  addLegend(data = burns_west,
            position = "bottomright",
            pal = pal, values = ~BURNTYP,
            title = "Burn Type",
            opacity = 1) 

# STEPS FOR PUBLISHING LEAFLET MAP ONLINE

# save as html:
# In R Viewer, export as webpage (map.html; ok to overwrite when I update)

# build index file (only do this the first time)
#open a new script and save

# stage comit push
# on git for this repo, set pages settings
# go to actions & wait for "pages build and development
# click url and add "/map.html"
