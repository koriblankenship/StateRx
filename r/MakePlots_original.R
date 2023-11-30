library(tidyverse)
#library(sf)

# GET DATA
rx_west <- read_csv("out/rx_west.csv")
#rx_west_pad <- read_csv("in/pad/rx_west_pad.csv")

# PRE PROCESS THE PAD INFO

# filter out ag
# remove if lifeform is ag and pad is private

# definetly don't report: CA, CO, NV
# dont think they report: AZ
# do report: ID
# probably : MT
#? : NM, OR (review flow chart), UT, WA, WY

# ag_by_state <- rx_west %>%
#   filter(lifeform == "Agriculture") #>> 569 rows
rx_west <- rx_west %>%
  mutate(ag_remove = case_when(STATE == "ID" & lifeform == "Agriculture" & Manager_Na == "Private" ~ "remove",
                               STATE == "WA" & lifeform == "Agriculture" & Manager_Na == "Private" ~ "remove",
                               .default = "keep"))
# ag_remove <- rx_west %>%
#   filter(ag_remove == "remove") %>%
#   group_by(STATE) #>> 481 rows 

rx_west2 <- rx_west %>%
  filter(ag_remove == "keep") %>%
  select(-ag_remove)
  
# simplify land manager classes #######UPDATE
rx_west <- rx_west %>%
  mutate(manager = case_when(Manager_Ty == "Local" | Manager_Ty == "Unknown" |
                               Manager_Ty == "NGO" | Manager_Ty == "Other"  ~ "Other",
                             is.na(Manager_Ty) ~ "Private",
                            .default = Manager_Ty))
 

### SUMMARY INFO - PLOTS! :) ----

#colors_bt <- c("#0583D2", "#61B0B7", "grey")
colors_bt <- c("#FB6A4A", "#FED976", "grey")
colors_bt <- c("#20639B", "#3CAEA3", "grey")
colors_bt <- c("#20639B", "grey", "#ED553B")

### PLANNED BURNS ----
# # planned burns (projects, units, ignitions?)/year (permits/time) 
# remove ID?
# CO - missing 2022 data?
# to do: add y axis commas, font family and size, theme improvement
p_plan_burns <- rx_west %>%
  group_by(STATE, YEAR, BURNTYPE_CLASSIFIED) %>%
  summarise(count = n())
ggplot(data = p_plan_burns) +
  aes(x = YEAR, y = count, fill = BURNTYPE_CLASSIFIED) + 
  geom_bar(position='stack', stat='identity') +
  facet_grid(cols = vars(STATE)) +
  scale_fill_manual(values = colors_bt) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(panel.background = element_blank()) + #remove grey background
  #       text = element_text(size = 11, family = "A")) + #font size and style
  scale_x_continuous(breaks = c(2018, 2021), name = "Year") + #x breaks and label
  scale_y_continuous("Number of planned burns") + #y label
  labs(fill = "Burn type")  #legend label

## PLANNED V. COMPLETE ----
# planned acres

#options(scipen=999)
p_plan_acres <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  group_by(STATE, YEAR) %>%
  summarise(acre_plan = sum(ACRE_PLANNED)) 
# ggplot(data = p_plan_acres) +
#   aes(x = YEAR, y = acre_plan) + 
#   geom_bar(position='stack', stat='identity') +
#   facet_grid(cols = vars(STATE)) 

#  completed acres for broadcast over time (no complete acres for nv and id)
p_complete_broadcast <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  group_by(STATE, YEAR) %>%
  summarise(acre_complete = sum(SUM_COMPLETE))  
# ggplot(p_complete_broadcast, aes(x = STATE, y = acre_complete)) +
#   geom_boxplot() 

# plan complete side by side
p_plan_complete2 <- left_join(p_plan_acres, p_complete_broadcast)
p_plan_complete2 <- p_plan_complete2 %>% 
  pivot_longer(cols=c("acre_plan", "acre_complete"),
               names_to = "status",
               values_to = "acres") 

ggplot(data = p_plan_complete2) +
  aes(x = YEAR, y = acres, fill = status) + 
  geom_bar(stat='identity', width=.5, position = "dodge") +
  facet_grid(cols = vars(STATE)) 


# plan and complete acres - stacked bar
# OR? planned = complete? # there are not planned acres in OR - need to adjust the planned code? 
# deal with negative values
# double check states that report only planned or only compelted ***************
p_plan_complete <- left_join(p_plan_acres, p_complete_broadcast)
# PLANNED - COMPLETED 
p_plan_complete <- p_plan_complete %>%
  mutate(plan_not_com = acre_plan - acre_complete)

p_plan_complete <- p_plan_complete %>% 
  pivot_longer(cols=c("plan_not_com", "acre_complete"),
               names_to = "status",
               values_to = "acres") 
 # drop_na() ####?????? make 0?

ggplot(data = p_plan_complete) +
  aes(x = YEAR, y = acres, fill = status) + 
  geom_bar(position='stack', stat='identity') +
  facet_grid(cols = vars(STATE)) 


### BROADCAST BURNS BY OWNERSHIP
p_bcast_owner_countcomp <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(SUM_COMPLETE > 0) %>%
  group_by(STATE, Manager_Na) %>%
  summarise(count = n())
p_bcast_owner_accomp <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(SUM_COMPLETE > 0) %>%
  group_by(STATE, Manager_Na) %>%
  summarise(ac_comp = sum(SUM_COMPLETE))
#p_bcast_owner <- full_join(p_bcast_owner_countcomp, p_bcast_owner_accomp)

ggplot(data = p_bcast_owner_countcomp) +
  aes(x = STATE, y = count, fill = Manager_Na) + 
  geom_bar(position='stack', stat='identity') 
ggplot(data = p_bcast_owner_accomp) +
  aes(x = STATE, y = ac_comp, fill = Manager_Na) + 
  geom_bar(position='stack', stat='identity') 

### AVERAGE SIZE OF COMPLETED BURNS
# average completed fire size - bar plot
#ID has not data on completed
p_complete_firesize <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(SUM_COMPLETE > 0) %>%
  group_by(STATE) 
ggplot(p_complete_firesize, aes(x = STATE, y = SUM_COMPLETE)) +
  geom_boxplot() 




#### OTHER STUFF ----

# fire size, planned burns, boxplot
p_plan_firesize <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(ACRE_PLANNED > 0)
# boxplot
ggplot(p_plan_firesize, aes(x = STATE, y = ACRE_PLANNED)) +
  geom_boxplot() +
  ylim(0, 6500) 

# number of permits and acres by entity (public vs. pvt), bar chart
p_plan_entity_burns <- rx_west_pad %>%
  group_by(STATE, manager) %>%
  summarise(count = n())
ggplot(data = p_plan_entity_burns) +
  aes(x = STATE, y = count, fill = manager) + 
  geom_bar(position='stack', stat='identity')  

p_plan_entity_acres <- rx_west_pad %>%
  filter(ACRE_PL > 0) %>% # no funciona sin esto, maybe because the the NA values
  group_by(STATE, manager) %>%
  summarise(acre_plan = sum(ACRE_PL)) 
ggplot(data = p_plan_entity_acres) +
  aes(x = STATE, y = acre_plan, fill = manager) + 
  geom_bar(position='stack', stat='identity')  

# fire size histogram for completed burns 
p_complete_firesize <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(SUM_COMPLETE > 0)
# boxplot
ggplot(p_complete_firesize, aes(x = STATE, y = SUM_COMPLETE)) +
  geom_boxplot() +
  ylim(0, 2000) ############ deal w/ visualizing outliers
scale_y_discrete()
#  scale_y_log10()
# histogram 
ggplot(p_complete_firesize, aes(x = SUM_COMPLETE)) +
  geom_histogram(bins = 10)
#warning: Removed 6984 rows containing non-finite values (`stat_bin()`).????
# density
ggplot(p_complete_firesize, aes(SUM_COMPLETE, colour = STATE)) +
  geom_density() +
  xlim(-1, 1500)





## MAKE A MAP GRAPHIC ----
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



# sample plots -- MOVE OR DELETE

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






### ARCHIVE
p_plan_burns_pad <- rx_west_pad %>%
  group_by(STATE, YEAR, BURNTYP) %>%
  summarise(count = n())
sum(p_plan_burns$count)
