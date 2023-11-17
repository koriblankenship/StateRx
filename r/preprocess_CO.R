#This script takes CO Rx permit data and processes it so it is ready to be used in the Rx database
#The data include separate tables for each year from 2010 to 2022.
#The data are in consistent between years; this script standardizes them before binding them together and processing.

library(tidyverse)

### BRING IN THE DATA ----

# get permits for all years
co2010 <- read_csv("in/CO_Copy of Annual Activity Summary 2010 for Public.csv")
co2011 <- read_csv("in/CO_Copy of Annual Activity Summary 2011 for Public.csv") 
co2012 <- read_csv("in/CO_Copy of Annual Activity Summary 2012 for Public.csv") 
co2013 <- read_csv("in/CO_Copy of Annual Activity Summary 2013 for Public.csv")
co2014 <- read_csv("in/CO_Copy of Annual Activity Summary 2014 for Public.csv") 
co2015 <- read_csv("in/CO_Copy of Annual Activity Summary 2015 for Public.csv")
co2016 <- read_csv("in/CO_Copy of Annual Activity Summary 2016 for Public.csv")
co2017 <- read_csv("in/CO_Copy of Annual Activity Summary 2017 for Public.csv")
co2018 <- read_csv("in/CO_Copy of Annual Activity Summary 2018 for Public.csv")  
co2019 <- read_csv("in/CO_Copy of Annual Activity Summary 2019 for Public.csv")
co2020 <- read_csv("in/CO_Copy of Annual Activity Summary 2020 for Public.csv") 
co2021 <- read_csv("in/CO_Copy of Annual Activity Summary 2021 for Public.csv")
co2022 <- read_csv("in/CO_Copy of Annual Activity Summary 2022 for Public.csv")

# 2013 legal description xwalk
# there are multiple legals for some permits, CO keeps the first legal which is done here w/ distinct
xwalk_legal <- read_csv("in/CO_2013_LegalDescription.csv") %>%
  select(-AgencyGroup) %>%
  distinct(ShortPermitNum, .keep_all = TRUE)

# 2018 agency id/group xwalk
xwalk_agency <- read_csv("in/CO_2018_AgencyIDTable.csv") %>%
  select(AgencyID, AgencyGroup)


### PREP ALL YEARS TO MAKE THEM COMPATIBLE ----

# 2010
co2010 <- co2010 %>%
  rename("AcresAnnMaxPERMITTED" = "Acres Max Permitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2010) %>% 
  mutate(DATE = "01/01/2010") #no date for this year, make a placeholder

# 2011
co2011 <- co2011 %>%
  rename("AcresAnnMaxPERMITTED" = "Acres Max Permitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2011) %>% 
  mutate(DATE = "01/01/2011") #no date for this year, make a placeholder

# 2012
co2012 <- co2012 %>%
  rename("AcresAnnMaxPERMITTED" = "Acres Max Permitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2012) %>% 
  mutate(DATE = "01/01/2012") #no date for this year, make a placeholder

# 2013
co2013 <- left_join(co2013, xwalk_legal, by = "ShortPermitNum") #add the legal description columns
co2013 <- co2013 %>%
  rename("AcresAnnMaxPERMITTED" = "Acres Max Permitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2013) %>% 
  mutate(DATE = "01/01/2013")  #no date for this year, make a placeholder

# 2014
co2014 <- co2014 %>%
  rename("AcresAnnMaxPERMITTED" = "Acres Max Permitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2014) %>% 
  mutate(DATE = "01/01/2014")  #no date for this year, make a placeholder

# 2015
co2015 <- co2015 %>%
  rename("AcresAnnMaxPERMITTED" = "Acres Max Permitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2015) %>% 
  mutate(DATE = "01/01/2015")  #no date for this year, make a placeholder

# 2016
co2016 <- co2016 %>%
  rename("AcresAnnMaxPERMITTED" = "AcresAnnMaxProject") %>%
  rename("Actual Acres Burned" = "ActAcres") %>%
  mutate(year = 2016) %>%
  mutate(DATE = "01/01/2016") %>% #no date for this year, make a placeholder
  #Calculate the actual pile volume consumed Act Pile Vol Consumed (PilePctCons*.01 converts % consumption to decimal form)
  mutate("Act Pile Vol Consumed (FT3)" = ActPiles * PileActVolume * PilePctCons * .01) 

# 2017
co2017 <- co2017 %>%
  rename("AcresAnnMaxPERMITTED" = "Acres Max Permitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2017) %>% 
  mutate(DATE = "01/01/2017") #no date for this year, make a placeholder

# 2018
co2018 <- left_join(co2018, xwalk_agency, by = "AgencyID") #add the AgencyGroup column 
co2018 <- co2018 %>%
  rename("AcresAnnMaxPERMITTED" = "Acres Max Permitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2018) %>% 
  mutate(DATE = "01/01/2018") #no date for this year, make a placeholder

# 2019
co2019 <- co2019 %>%
  mutate(year = 2019) %>% 
  mutate(DATE = "01/01/2019") #no date for this year, make a placeholder

# 2020
co2020 <- co2020 %>%
  rename("AcresAnnMaxPERMITTED" = "AcresAnnMaxProjectPermitted") %>%
  rename("Act Pile Vol Consumed (FT3)" = "Act Pile Vol Consumed") %>%
  mutate(year = 2020) %>% 
  mutate(DATE = "01/01/2020") #no date for this year, make a placeholder

# 2021
co2021 <- co2021 %>%
  mutate(year = 2021) %>% 
  mutate(DATE = "01/01/2021") #no date for this year, make a placeholder

# 2022
co2022 <- co2022 %>%
  rename("ShortPermitNum" = "Full Permit Number") %>%
  rename("AcresAnnMaxPERMITTED" = "AcresAnnMaxProject") %>%
  rename("BurnName" = "Burn Name") %>%
  rename("PileBcst" = "PileBcast") %>%
  rename("Lon" = "Long") %>%
  rename("PermitIssueDate" = "Permit Issue Date") %>% #rename here so I can fix the date below
  mutate(year = 2022) %>%
  mutate(DATE = mdy(PermitIssueDate)) %>%
  mutate(DATE = as.Date(DATE)) 


### BIND ALL YEARS ----

#bind 2010-2021
raw <- bind_rows(co2010, co2011, co2012, co2013, co2014, co2015, co2016, co2017, co2018, co2019, co2020, co2021) %>%
  mutate_at("ShortPermitNum", as.character) %>% #need as character to match 2022
  mutate(DATE = as.Date(mdy(DATE))) #format date

#bind 2022
raw <- bind_rows(raw, co2022) %>%
  select(any_of(c("ShortPermitNum", "year", "DATE", "AcresAnnMaxPERMITTED", "Actual Acres Burned", 
                  "Act Pile Vol Consumed (FT3)", "BurnName", "PileBcst", "AgencyGroup", "AdministrativeUnit", 
                  "Lat", "Lon", "TWN", "TWND", "RNG", "RNGD", "SECTION")))
#remove spaces in column names
names(raw) <- make.names(names(raw), unique=TRUE) 


### PROCESS ----

process <- raw %>%
  # burn type
  mutate(BURNTYPE_REPORTED = case_when(PileBcst == "b" ~ "Broadcast",
                                       PileBcst == "p" ~ "Pile",
                                       .default = PileBcst)) %>%
  # completed acres  
  # broadcast burns with acres burned = NA are given 0 completed acres.
  # if acres burned are not reported, it is safe to assume no burn occurred b/c burners verify these reports.
  # piles are not always reported in acres, so if acres burned = NA for piles, completed acres = NA.
  mutate(ACRES_COMPLETED = case_when(BURNTYPE_REPORTED == "Broadcast" & is.na(Actual.Acres.Burned) ~ 0, 
                                     .default = Actual.Acres.Burned)) %>%
  # pile volume
  mutate(PILE_VOLUME = case_when(BURNTYPE_REPORTED == "Pile" & is.na(Act.Pile.Vol.Consumed..FT3.) ~ 0, 
                                 .default = Act.Pile.Vol.Consumed..FT3.)) %>%
  # entity requesting 
  mutate(ENTITY_REQUESTING = paste(AgencyGroup, AdministrativeUnit, sep = ", ")) %>%
  # burn status
  # the pile volume column represents actual pile volume consumed so it can be used to find status of pile burns
  mutate(BURN_STATUS = case_when(ACRES_COMPLETED > 0 | PILE_VOLUME > 0 ~ "Complete",
                                 .default = "Incomplete")) %>%
  # legal description
  mutate(township = case_when(is.na(TWN) ~ NA, # concatenate township
                              .default = paste(TWN, TWND, sep = ""))) %>% 
  mutate(range = case_when(is.na(RNG) ~ NA,  # concatenate range
                           .default = paste(RNG, RNGD, sep = ""))) %>%
  mutate(LEGAL_DESCRIP = case_when(is.na(township) | is.na(range) ~ NA, # concatenate township, range, section
                                   .default = paste(township, range, SECTION, sep = " ")))

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
co_ready <- process %>%
  rename("SOURCE_ID" = "ShortPermitNum") %>% 
  #rename("DATE" = "") %>%
  #rename("ACRES_REQUESTED = "") %>%
  rename("ACRES_PERMITTED" = "AcresAnnMaxPERMITTED") %>%
  #rename("ACRES_COMPLETED" = "") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "BurnName") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  #rename("ENTITY_REQUESTING" = "") %>%
  rename("LAT_PERMIT" = "Lat") %>%
  rename("LON_PERMIT" = "Lon") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  #rename("TONS" = "") %>%
  #rename("BURN_STATUS" = "") %>%
  select(any_of(c("SOURCE_ID", "DATE", "ACRES_REQUESTED", "ACRES_PERMITTED", "ACRES_COMPLETED", "PILE_VOLUME", 
                  "BURN_NAME", "BURNTYPE_REPORTED", "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", 
                  "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()


###EXPORT

write_csv(co_ready, "out/co_ready.csv")
