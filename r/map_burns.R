library(tidyverse)
library(sf)
library(tigris)


### BRING IN THE DATA ----
burns_west <- st_read("out/shp/rx_west.shp") %>% #> 56,610 rows
  st_transform(crs = 4326) # try going back to decimal degrees for mapping

# spatial data
west <- states() %>%
  filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
  st_transform(st_crs(burns_west)) %>% 
  select(STUSPS)

### just points
rx_west <- ggplot() +
  geom_sf(data = burns_west, shape = 21, size = .1, color = "#F99301", 
          fill = "#F99301") +
  geom_sf(data = west, fill = NA) +
  theme_bw() +
  theme(text = element_text(size = 9)) 
rx_west


### good map by burn type
map <- ggplot() +
  geom_sf(data = burns_west, aes(fill = BURNTYP), shape = 21, size = .75, color = "#6B6E70", alpha=0.9) +
  #geom_sf(data = burns_west, aes(fill = BURNTYP), shape = 21, size = 1.5, alpha=0.9) +
  scale_fill_manual(values = c("#F99301", "grey", "black")) +
  #scale_fill_manual(values = c("#F7BC00", "#F56300")) + 
  geom_sf(data = west, fill = NA) +
  theme_bw() +
  theme(text = element_text(size = 9)) +
  labs(fill = "Burn type")  #legend label
map

ggsave(plot = map, width = 5, height = 7, dpi = 300, filename = "out/plot/map.jpg")
