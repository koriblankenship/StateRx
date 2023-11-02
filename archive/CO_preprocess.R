#This script takes CO Rx permit data and processes it so it is ready to be used in the Rx database
#Currently data for 2020-2022 are processed. Data can be added going back to 2010.

library(tidyverse)


### BRING IN DATA

co2022 <- read_csv("in/CO_Copy of Annual Activity Summary 2022 for Public.csv") %>%
  select(-c(PilesAnnMaxProject, "Actual Piles", PileActVolume, PilePctCons)) %>%
  rename(ShortPermitNum = "Full Permit Number") %>%
  rename("Act Pile Vol Consumed" = "Act Pile Vol Consumed (FT3)") %>%
  rename(BurnName = "Burn Name") %>%
  rename(PileBcst = PileBcast) %>%  
  rename(Lon = Long)

co2021 <- read_csv("in/CO_Copy of Annual Activity Summary 2021 for Public.csv") %>%
  select(-c(AgencyID, PilesAnnMaxPERMITTED, "Actual Piles", PileActVolume, PilePctCons)) %>%
  rename(AcresAnnMaxProject = AcresAnnMaxPERMITTED) %>% 
  rename("Act Pile Vol Consumed" = "Act Pile Vol Consumed (FT3)") %>%
  mutate("Permit Issue Date" = "1/1/2021") #I only have the year
co2021$ShortPermitNum <- as.character(co2021$ShortPermitNum) #change the ShortPermitNum to character

co2020 <- read_csv("in/CO_Copy of Annual Activity Summary 2020 for Public.csv") %>%
  select(-c(AgencyID, PilesAnnMaxProjectPermitted, ActPiles, PileActVolume, PilePctCons)) %>%
  rename(AcresAnnMaxProject = AcresAnnMaxProjectPermitted) %>% 
  mutate("Permit Issue Date" = "1/1/2020") #I only have the year
co2020$ShortPermitNum <- as.character(co2020$ShortPermitNum) #change the ShortPermitNum to character


### PROCESS [starting w/ most recent 3 years]

#Bind annual data 
processed <- bind_rows(co2022, co2021, co2020)

# update burn type
processed <- processed %>%
  mutate(BURNTYPE_REPORTED = case_when(PileBcst == "b" ~ "Broadcast",
                              PileBcst == "p" ~ "Pile",
                              .default = PileBcst)) 

#xwalk to the rx database column names
processed <- processed %>%
  rename(DATE = "Permit Issue Date") %>%
  rename(BURN_NAME = BurnName) %>% 
  rename(SOURCE_ID = ShortPermitNum) %>%
  rename(PERMITTED_ACRES = AcresAnnMaxProject) %>%
  rename(LAT_PERMIT = Lat) %>%
  rename(LON_PERMIT = Lon) %>%
  rename("AcresBurned" = "Actual Acres Burned") %>%
  rename("ActPileVol" = "Act Pile Vol Consumed")

#calc complete acres and pile volume 
processed <- processed %>%
  #for broadcast, complete acres = AcresBurned or assign 0 for NAs
  mutate(COMPLETED_ACRES = case_when(BURNTYPE_REPORTED == "Broadcast" & is.na(AcresBurned) ~ 0, 
                                     .default = AcresBurned)) %>%
  #for piles, pile_volume = ActPileVol or assign 0 for NAs
  mutate(PILE_VOLUME = case_when(BURNTYPE_REPORTED == "Pile" & is.na(ActPileVol) ~ 0, 
                                 .default = ActPileVol))

#calculate entity requesting and burn status
processed <- processed %>%
  mutate(ENTITY_REQUESTING = paste(AgencyGroup, AdministrativeUnit, sep = ", ")) %>%
  #if completed acres or pile volume is > 0, status = Complete, else Incomplete
  mutate(BURN_STATUS = case_when(COMPLETED_ACRES > 0 | PILE_VOLUME > 0 ~ "Complete",
                                 .default = "Incomplete")) 

#parse the date
processed$DATE <- mdy(processed$DATE) 

#remove unneeded columns
co_ready <- processed %>%
  select(-c(AgencyGroup, AdministrativeUnit, PileBcst, AcresBurned, ActPileVol))


###EXPORT

write_csv(co_ready, "out/co_ready.csv")
