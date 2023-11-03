### 

library(tidyverse)


### BRING IN THE DATA ----

az <- read_csv("out/az_ready.csv") %>%
  mutate(STATE = "AZ")
ca <- read_csv("out/ca_ready.csv") %>%
  mutate(STATE = "CA")
co <- read_csv("out/co_ready.csv") %>%
  mutate(STATE = "CO")
id <- read_csv("out/id_ready_DRAFT.csv") %>%
  mutate(STATE = "ID")
mt <- read_csv("out/mt_ready_DRAFT.csv") %>%
  mutate_at("SOURCE_ID", as.character) %>%
  mutate(STATE = "MT")
nm <- read_csv("out/nm_ready.csv") %>%
  mutate_at("SOURCE_ID", as.character) %>%
  mutate(STATE = "NM")
nv <- read_csv("out/nv_ready.csv") %>%
  mutate(STATE = "NV")
or <- read_csv("out/or_ready.csv") %>%
  mutate_at("SOURCE_ID", as.character) %>%
  mutate(STATE = "OR")
ut <- read_csv("out/ut_ready.csv") %>%
  mutate(STATE = "UT")
wa <- read_csv("out/wa_ready.csv") %>%
  mutate(STATE = "WA")
wy <- read_csv("out/wy_ready.csv") %>%
  mutate(STATE = "WY")

# binder <- bind_rows(az, ca, co, id, mt, nm, nv, or, ut, wa, wy) %>%
#   mutate(burn_name_lower = BURN_NAME) %>%
#   mutate(tolower(burn_name_lower))

binder <- bind_rows(az, ca, co, id, mt, nm, nv, or, ut, wa, wy) %>%
  mutate(burn_name_lower = tolower(BURN_NAME)) # column with burn name as lower case 


### KEYWORDS
#see if I can designate any case combination, upper, lower, sentence, etc

# PILES

keywords_pile <- c("pile", "piles", "handpile", "handpiles", "jackpot", "jackpots")
#slash? could do when acres not reported and volume is but this doesn't pick up many burns

keywords_broadcast <- c("broadcast", "natural fuel", "underburn", "understory")
#tried rx but it picks up a lot of "rx piles"

#Ag was not very helpful, probably skip this
# keywords_ag <- c(" ag ", " Ag ",
#                  "agriculture", "Agriculture")  


### FIND KEYWORDS IN BURN NAME

binder <- binder %>%
  rowwise() %>%
  mutate(is.pile = str_detect(burn_name_lower, paste0(keywords_pile, collapse = '|'))) %>%
  mutate(is.broadcast = str_detect(burn_name_lower, paste0(keywords_broadcast, collapse = '|'))) 
  


### TEST FOR CONFLICTS 

# 1 apply xwalk for reported burn types; revise xwalk for types that have a logical criteria
# 2 for unknown or unclassified rows classify based on is.pile and is.broadcast
# 3 anything unclassified is unknown


#rules:
# favor burn type reported
# if piles and broadcast are in the name > broadcast



### Make DF of all reported burn types
burntypes <- table(binder$BURNTYPE_REPORTED) 
burntypes <- as.data.frame(burntypes) 


### TEST FOR A TYPE

burntype_tester <- binder%>% 
  filter(BURNTYPE_REPORTED == "Wildlife Habitat")

unique(burntype_tester$STATE)


### RANGE TYPES
burntype_range <- binder%>% 
  filter(BURNTYPE_REPORTED == "Rangeland" | 
           BURNTYPE_REPORTED == "Rangeland (not cultivated or seeded in the last 10 years)" | 
           BURNTYPE_REPORTED == "Range")

# in OR ("Rangeland") - can get more info on type from burn name
#look at burntypes reported in OR
burntype_or <- binder%>% 
  filter(STATE == "OR") %>%
  distinct(BURNTYPE_REPORTED)
#look at rangeland burn rows in OR
burntype_or_rangeland <- binder %>%
  filter(STATE == "OR") %>%
  filter(BURNTYPE_REPORTED == "Rangeland")
#use logic b/c some of these have pile in the names, else assume broadcast

# in ID ("Rangeland (not cultivated or seeded in the last 10 years)") - no burn name 
burntype_id <- binder%>% 
  filter(STATE == "ID") %>%
  distinct(BURNTYPE_REPORTED)
burntype_id_range <- binder%>% 
  filter(STATE == "ID") %>%
  filter(BURNTYPE_REPORTED == "Rangeland (not cultivated or seeded in the last 10 years)")
#no burn names in ID, assume broadcast

# in MT ("Range")
burntype_mt_range <- binder%>% 
  filter(STATE == "MT") %>%
  filter(BURNTYPE_REPORTED == "Range")
#these appear to be broadcast



write_csv(burntypes, "in/burntypes.csv")