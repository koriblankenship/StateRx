options(scipen=999)
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
# calculate west
p_plan_complete2_w <- p_plan_complete2 %>% 
  group_by(YEAR, status) %>%
  summarise(acres = sum(acres)) %>%
  mutate(STATE = "West")
ggplot(data = p_plan_complete2_w) +
  #ggplot(data = p_plan_complete2) +
  aes(x = YEAR, y = acres, fill = status) + 
  geom_bar(stat='identity', width=.5, position = "dodge") +
  facet_grid(cols = vars(STATE)) +
  scale_x_continuous(breaks = c(2018, 2021), name = "Year") #x breaks and label




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

