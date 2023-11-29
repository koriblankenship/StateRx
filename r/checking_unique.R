# rx %>%
#   filter(BURNTYPE_CLASSIFIED == "Unknown") %>%
#   group_by(STATE) %>%
#   summarise(count = n(), median = median(SUM_COMPLETE))

#hella big pile burns
# rx_bigpile -> rx
#   burntype


#permits by state
state_permits <- rx_west %>%
  group_by(STATE) %>%
  count(STATE)
state_permits0 <- rx_west %>%
  group_by(STATE) %>%
  filter(SUM_PERMIT == 0) %>%
  count(STATE)

id_test0 <- rx_west %>%
  filter(STATE == "ID") %>%
  filter(SUM_PERMIT == 0) # 7728 rows meet criteria
id_test_na <- rx_west %>%
  filter(STATE == "ID") %>%
  filter(is.na(SUM_PERMIT)) # 0 rows meet criteria

# remaining duplicates
#pull out duplicates from rx
check_rx_duplicates <- rx %>% ### this is not clipped to west
  group_by(STATE, YEAR, LAT_PERMIT, LON_PERMIT) %>%
  summarise(burns_unique = n(), across())


## Look for large permits, reqeusts, completed
# super_permit <- binder %>% #these get eleminated by the year criteria in the analysis
#   filter(ACRES_PERMITTED >= 100000)
# super_request <- binder %>%
#   filter(YEAR > 2016 & YEAR < 2023) %>%
#   filter(ACRES_REQUESTED >= 5000)
# super_complete <- binder %>%
#   filter(YEAR > 2016 & YEAR < 2023) %>%
#   filter(ACRES_COMPLETED >= 10000)
## NM big burn
# these seem legit
# nm_test <- binder %>%
#   filter(YEAR > 2016 & YEAR < 2023) %>%
#   filter(STATE == "NM") %>%
#   filter(ACRES_PERMITTED > 10000)


##############LOGIC CHECK/ERROR CHECK???#################
unique_locations <- binder %>%
  filter(YEAR > 2016 & YEAR < 2023) %>% # common time for all states is 2017-2022
  filter(BURNTYPE_CLASSIFIED != "Yard waste") %>%
  distinct(STATE, YEAR, LAT_PERMIT, LON_PERMIT)
# there are 3153 fewer burns when only location is used to find unique

# count number of burns per year per state and compare between methods
burns_yr_state_unique <- unique_locations %>%
  group_by(STATE, YEAR) %>%
  summarise(burns_unique = n())
burns_yr_state_rx <- rx %>%
  group_by(STATE, YEAR) %>%
  summarise(burns_rx = n())
burns_check <- left_join(burns_yr_state_unique, burns_yr_state_rx)
burns_check <- burns_check %>%
  mutate(diff = burns_unique - burns_rx)
#pull out duplicates from rx
check_rx_duplicates <- rx %>%
  group_by(STATE, YEAR, LAT_PERMIT, LON_PERMIT) %>%
  summarise(burns_unique = n(), across())
# some duplicates have lat/lon = NA; these will be removed below
# pile burns tend to have the same lat/lon but w/ different burn names and source IDs
# there are 209 rows where there were > 10 burns in 1 place/year and the lat/lon is not NA

## check permit acres vs. max request - how close are they?
#write_csv(rx, "out/RX_CHECKER.csv")
#write_csv(check_rx_duplicates, "out/CHECK_RX_DUPLICATES.CSV")
################################

rx %>%
  filter(is.na(ACRE_PLANNED) & is.na(SUM_REQUEST)) %>%
  group_by(STATE, BURNTYPE_CLASSIFIED) %>%
  summarise(count = n())

summary(rx$ACRE_PLANNED[which(rx$BURNTYPE_CLASSIFIED == "Pile")])

summary(rx_west$SUM_COMPLETE[which(rx$BURNTYPE_CLASSIFIED == "Unknown")])
