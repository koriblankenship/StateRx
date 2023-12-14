library(tidyverse)

### GET DATA ----
rx_west <- read_csv("out/rx_west.csv")


### PLANNED BURNS ----

p_plan_burns <- rx_west %>%
  group_by(STATE, BURNTYPE_CLASSIFIED) %>%
  summarise(count = n())
#check <- sum(p_plan_burns$count)

# calculate west
p_plan_burns_w <- p_plan_burns %>%
  group_by(BURNTYPE_CLASSIFIED) %>%
  summarise(count = sum(count)) 
# Westwide - 8 times more pile than broadcast burn records.

#colors1 <- c("#EF2648", "grey", "black")
colors1 <- c("#F99301", "grey", "black")
p1 <- ggplot(data = p_plan_burns) +
  aes(x = STATE, y = count, fill = BURNTYPE_CLASSIFIED) + 
  geom_bar(position='stack', stat='identity') +
  scale_fill_manual(values = colors1) + 
  theme(panel.background = element_blank()) + #remove grey background
  theme(text = element_text(size = 6)) +
  scale_y_continuous(
    breaks = c(0, 5000, 10000, 15000),
    labels = c("0", "5,000", "10,000", "15,000"),
    name = "Number of reported burns") +
  scale_x_discrete(name = "State") + # x label
  labs(fill = "Burn type")  #legend label
p1
ggsave(plot = p1, width = 4.5, height = 2, dpi = 300, filename = "out/plot/p1.jpg")


## PLANNED/REQUESTED V. COMPLETE BROADCAST ----

options(scipen=999)

# plan/request acres 
acre_plan <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  mutate(acre_planned = case_when(SUM_PERMIT > 0 ~ SUM_PERMIT,
                                  MAX_REQUEST > 0 ~ MAX_REQUEST,
                                  .default = 0)) %>%
  group_by(STATE) %>%
  summarise(Planned = sum(acre_planned)) 
# completed acres 
acre_comp <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  group_by(STATE) %>%
  summarise(Completed = sum(SUM_COMPLETE))  
#sum_broadcast_comp <- sum(acre_comp$Completed) # 1,345,896 acres westwide over 7 years

# plan + complete 
p_plan_comp <- full_join(acre_plan, acre_comp)
p_plan_comp <- p_plan_comp %>% 
  filter(Planned > 0) %>%
  filter(Completed > 0) %>%
  pivot_longer(cols=c("Planned", "Completed"),
               names_to = "Status",
               values_to = "Acres") 

# calculate west
p_plan_comp_w <- p_plan_comp %>%
  group_by(Status) %>%
  summarise(Acres = sum(Acres)) %>%
  mutate(TotalAcres = sum(Acres)) %>%
  mutate(percent = Acres/TotalAcres)
#Westwide – 23 % of planned broadcast burns are completed. 

# reorder to get complete at bottom
p_plan_comp$Status <- factor(p_plan_comp$Status, levels=c('Planned', 'Completed'))

#colors2 <- c("#92c2cc", "#067d93")
colors2 <- c("#F7BC00", "#F56300")
p2 <- ggplot(p_plan_comp) +
  aes(x = STATE, y = Acres, fill = Status) + 
  geom_bar(stat='identity') +
  scale_fill_manual(values = colors2) + 
  scale_y_continuous(
    breaks = c(0, 500000, 1000000, 1500000, 2000000),
    labels = c("0", ".5 M", "1 M", "1.5 M", "2 M"),
    name = "Area broadcast burned (acres)") +
  scale_x_discrete(name = "State") + # x label
  labs(fill = "Burn status") + #legend label
  theme(panel.background = element_blank()) + #remove grey background
  theme(text = element_text(size = 6)) 
p2
ggsave(plot = p2, width = 4.5, height = 2, dpi = 300, filename = "out/plot/p2.jpg")


### PROPORTION OF BROADCAST ACRES COMPLETED BY OWNERSHIP

p_owner <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(SUM_COMPLETE > 0) %>%
  group_by(STATE, Manager_Type) %>%
  summarise(acres = sum(SUM_COMPLETE))
# add proportion
acre_state <- p_owner %>%
  group_by(STATE) %>%
  summarise(acre_state = sum(acres)) 
p_owner_percent <- left_join(p_owner, acre_state)
p_owner_percent <- p_owner_percent %>%
  mutate(percent = ((acres/acre_state)*100))
            
# calculate west
p_owner_percent_w <- p_owner_percent %>%
  group_by(Manager_Type) %>%
  summarise(Acres = sum(acres)) %>%
  mutate(TotalAcres = sum(Acres)) %>%
  mutate(percent = Acres/TotalAcres)
# Westwide - Federal lands account for 78% of the area broadcast burned. 
# Westwide - 78% of the  area broadcast burned is on federal lands.

# reorder to get complete at bottom
p_owner_percent$Manager_Type <- factor(p_owner_percent$Manager_Type, 
                               levels=c('Other', 'Private', 'State', 'Federal'))
# colors
#colors3 <- c("#0F4C81", "#E9B666", "#5C9090", "#BFD0CA")
colors3 <- c("#0F4C81", "#778899", "#5C9090", "#BFD0CA")

p3 <- ggplot(p_owner_percent) +
  aes(x = STATE, y = percent, fill = Manager_Type) + 
  geom_bar(stat='identity') +
  scale_fill_manual(values = colors3) + 
  scale_y_continuous("Area broadcast burned (%)") +
  scale_x_discrete(name = "State") + # x label
  labs(fill = "Manager type") + #legend label
  theme(panel.background = element_blank()) + #remove grey background
  theme(text = element_text(size = 6)) 
p3
ggsave(plot = p3, width = 4.5, height = 2, dpi = 300, filename = "out/plot/p3.jpg")


### AVERAGE SIZE OF COMPLETED BURNS
# average completed fire size - bar plot
#ID and NV have not data on completed
p_complete_firesize <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(SUM_COMPLETE > 0) %>%
  group_by(STATE) 

# calculate west
comp_size_w_median <- median(p_complete_firesize$SUM_COMPLETE)
comp_size_w_mean <- mean(p_complete_firesize$SUM_COMPLETE)
# Westwide - Median completed broadcast burn size is 70 acres.; mean is 331 acres.

p4 <- ggplot(p_complete_firesize, aes(x = STATE, y = SUM_COMPLETE)) +
  geom_boxplot(outlier.shape = NA) + # remove the outliers
  labs(x = "State", y = "Broadcast burn size (acres)") + 
  theme(panel.background = element_blank()) + #remove grey background
  theme(text = element_text(size = 6)) +
  scale_y_continuous(
    breaks = c(0, 300, 600, 900, 1200),
    labels = c("0", "300", "600", "900", "1200"),
    limits = c(0,1300)) +
  theme(text = element_text(size = 6)) 
p4
# a warning message is generated b/c I'm cutting off all the outliers above 1300 (y lim)
ggsave(plot = p4, width = 4.5, height = 2, dpi = 300, filename = "out/plot/p4.jpg")




  




#### OTHER STUFF ----

### --- ownership

# this is my draft graph
ggplot(data = p_owner) +
  aes(x = STATE, y = acres, fill = Manager_Type) + 
  geom_bar(position='stack', stat='identity') 

# started this but then thought percent by ownership would be better
# reorder 
p_owner$Manager_Type <- factor(p_owner$Manager_Type, 
                               levels=c('Other', 'Private', 'State', 'Federal'))
# colors
colors3 <- c("#0F4C81", "#E9B666", "#A5B2B5", "#BFD0CA")

p3 <- ggplot(p_owner) +
  aes(x = STATE, y = acres, fill = Manager_Type) + 
  geom_bar(stat='identity') +
  scale_fill_manual(values = colors3) + 
  scale_y_continuous(
    breaks = c(500000, 1000000, 1500000, 2000000),
    labels = c(".5 M", "1 M", "1.5 M", "2 M")) +
  scale_x_discrete(name = "State") + # x label
  labs(fill = "Burn status") + #legend label
  theme(panel.background = element_blank()) + #remove grey background
  theme(text = element_text(size = 6)) 
p3
ggsave(plot = p2, width = 5.5, height = 2, dpi = 300, filename = "out/plot/p2.jpg")


# west
# ggplot(data = p_bcast_owner_accomp_w) +
#   aes(x = STATE, y = acres, fill = Manager_Type) + 
#   geom_bar(position='stack', stat='identity') 

### --- end ownership

# FED
p_bcast_fed_accomp <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(SUM_COMPLETE > 0) %>%
  group_by(STATE, Manager_Fed) %>%
  summarise(ac_comp = sum(SUM_COMPLETE))
ggplot(data = p_bcast_fed_accomp) +
  aes(x = STATE, y = ac_comp, fill = Manager_Fed) + 
  geom_bar(position='stack', stat='identity') 


# Count of completed burns by ownership
p_bcast_owner_countcomp <- rx_west %>%
  filter(BURNTYPE_CLASSIFIED == "Broadcast") %>%
  filter(SUM_COMPLETE > 0) %>%
  group_by(STATE, Manager_Type) %>%
  summarise(count = n())
ggplot(data = p_bcast_owner_countcomp) +
  aes(x = STATE, y = count, fill = Manager_Type) + 
  geom_bar(position='stack', stat='identity') 



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
