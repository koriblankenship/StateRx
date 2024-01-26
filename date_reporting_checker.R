# DATE

library(tidyverse)

# BRING IN THE DATABASE TABLE
rx4piles <- read_csv("out/rx4piles.csv")

dat <- rx4piles %>%
  mutate(doy = yday(DATE)) %>%
  filter(BURN_STATUS == "Complete") %>% # completed burns ONLY!
  # in CO I only have dates for the year 2022, remove years before 2022
  subset(!(STATE == "CO" & YEAR < 2022))

dat <- dat %>%
  group_by(STATE, doy, BURNTYPE_CLASSIFIED) %>%
  summarise(count = n()) 


# make seasons table for graph shading regions
# used non-leapyear julian dates to get seasons based on month
# https://nsidc.org/data/user-resources/help-center/day-year-doy-calendar
############# CONSIDER DEALING W/ LEAP YEARS (which have differnt doy's for each season)

# https://statisticsglobe.com/draw-ggplot2-plot-different-background-colors-region-r

seasons <- data.frame(start = c(1, 60, 152, 244, 335),  # Create data with breaks
                      end = c(59, 151, 243, 334, 366),
                      colors = c("Winter", "Spring", "Summer", "Fall", "Winter"))

# MAKE A RAD PLOT

## CHART

### PILES W/ SEASONS
dat_piles <- dat %>%
  filter(BURNTYPE_CLASSIFIED == "Pile") 

ggplot() +
  geom_rect(data = seasons,
          aes(xmin = start,
              xmax = end,
              ymin = - Inf,
              ymax = Inf,
              fill = colors),
          alpha = 0.2) +
  geom_col(data = dat_piles, aes(x = doy, y = count)) + 
  facet_wrap(~ STATE, ncol = 3) +
  theme(panel.background = element_blank(), #remove grey background
        #text = element_text(size = 11, family = "serif"), #font size
        legend.position = "bottom") + 
  scale_x_continuous(name = "Day of year", #x label
                     breaks=c(15, 46, 74, 105, 135, 166, 
                              196, 227, 258, 288, 319, 349), #*see below
                     labels=c("J", "F", "M", "A", "M", "J", 
                              "J", "A", "S", "O", "N", "D")) + #month labels
  coord_cartesian(ylim = c(0,150)) +
  labs(fill = "Season") +   #legend label 
  ggtitle("Number Piles Burns Completed per Season")

ggsave(width = 9, height = 7, dpi = 300, filename = "out/plot/DRAFT_piles_season.jpg")

### BROADCAST W/ SEASONS
dat_broadcast <- dat %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") 

ggplot() +
  geom_rect(data = seasons,
            aes(xmin = start,
                xmax = end,
                ymin = - Inf,
                ymax = Inf,
                fill = colors),
            alpha = 0.2) +
  geom_col(data = dat_broadcast, aes(x = doy, y = count)) + 
  facet_wrap(~ STATE, ncol = 3) +
  theme(panel.background = element_blank(), #remove grey background
        #text = element_text(size = 11, family = "serif"), #font size
        legend.position = "bottom") + 
  scale_x_continuous(name = "Day of year", #x label
                     breaks=c(15, 46, 74, 105, 135, 166, 
                              196, 227, 258, 288, 319, 349), #*see below
                     labels=c("J", "F", "M", "A", "M", "J", 
                              "J", "A", "S", "O", "N", "D")) + #month labels
  #coord_cartesian(ylim = c(0,150)) +
  labs(fill = "Season") +   #legend label 
  ggtitle("Number Broadcast Burns Completed per Season")

ggsave(width = 9, height = 7, dpi = 300, filename = "out/plot/DRAFT_broadcast_season.jpg")






# facet wrap BOTH
mycolors <- c("#F99301", "grey", "black") #dk blue, light blue

ggplot(data = dat) +
  aes(x = doy, y = count, fill = BURNTYPE_CLASSIFIED) + 
  geom_col() +
  facet_wrap(~ STATE, ncol = 3) +
  scale_fill_manual(values = mycolors) +
  theme(panel.background = element_blank(), #remove grey background
        #text = element_text(size = 11, family = "serif"), #font size
        legend.position = "bottom") + 
  scale_x_continuous(name = "Day of year", #x label
                     breaks=c(15, 46, 74, 105, 135, 166, 
                              196, 227, 258, 288, 319, 349), #*see below
                     labels=c("J", "F", "M", "A", "M", "J", 
                              "J", "A", "S", "O", "N", "D")) + #month labels
  # scale_y_continuous(name = "Number of ignitions", #y label
  #                    breaks=c(0, 100, 200, 300),
  #                    expand = expansion(mult = c(0, .1))) + #remove extra space between labels and axis
  labs(fill = "Cause")   #legend label






