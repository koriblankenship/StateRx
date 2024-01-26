# This script is designed to show how pile burns are reported in each state.
# e.g. are they mostly tracked in acres or volume
# I used this to figure out how to track acres in each state and 
# espeically wich state I can track acres of piles for.

library(tidyverse)

# BRING IN THE DATABASE TABLE
rx4piles <- read_csv("out/rx4piles.csv")

# filter for pile burns
piles <- rx4piles %>%
  filter(BURNTYPE_CLASSIFIED == "Pile") 

# count pile records in state
records <- piles %>%
  group_by(STATE) %>%
  mutate(count_piles = n()) %>%
  distinct(STATE, count_piles)

# group by state, count requests > 0
requests <- piles %>%
  group_by(STATE) %>%
  filter(ACRES_REQUESTED > 0) %>%
  mutate(count_requests = n()) %>%
  distinct(STATE, count_requests)

# group by state, count permits > 0
permits <- piles %>%
  group_by(STATE) %>%
  filter(ACRES_PERMITTED > 0) %>%
  mutate(count_permits = n()) %>%
  distinct(STATE, count_permits)

# group by state, count completed > 0
complete <- piles %>%
  group_by(STATE) %>%
  filter(ACRES_COMPLETED > 0) %>%
  mutate(count_complete = n()) %>%
  distinct(STATE, count_complete)

# group by state, count volume > 0
volume <- piles %>%
  group_by(STATE) %>%
  filter(PILE_VOLUME > 0) %>%
  mutate(count_volume = n()) %>%
  distinct(STATE, count_volume)

# join the tables
pile_info <- left_join(records, permits)
pile_info <- left_join(pile_info, requests)
pile_info <- left_join(pile_info, complete)
pile_info <- left_join(pile_info, volume)

# calculate percents
pile_info <- pile_info %>%
  mutate(per_permit = (count_permits/count_piles)*100) %>%
  mutate(per_request = (count_requests/count_piles)*100) %>%
  mutate(per_complete = (count_complete/count_piles)*100) %>%
  mutate(per_volume = (count_volume/count_piles)*100)

# export table
write_csv(pile_info, "out/pile_info.csv")


### QC

# CO - piles are supposed to be tracked in volume; check permits w/ acres
co <- piles %>%
  filter(STATE == "CO")
# looks like one pile burn reported acres; not going to worry about it


# NM - piles are supposed to be tracked in volume; check permits w/ acres
nm <- piles %>%
  filter(STATE == "NM")
# looks like there is just some inconsistency rather than an error


