library(tidyverse)
library(sf)
#library(tigris)
library(leaflet)


### BRING IN THE DATA ----
#df <- read_csv("out/rx_west.csv") #> 56,610 rows
burns_west <- st_read("out/shp/rx_west.shp") %>% #> 56,610 rows
  st_transform(crs = 4326) # try going back to decimal degrees for mapping


### MAP IT ----

popup <- paste(
  "Burn ID:", burns_west$ID, "<br>",
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
  setView(-114.7358292, 41.2557021, zoom = 5.48) %>% # 5.5 zoom is too close
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
