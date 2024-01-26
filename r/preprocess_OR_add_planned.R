# OR tracks "Planned" in one spreadsheet and "Accomplished" burns in many. This script takes in both
# and processes them to be ready to be used in the Rx database.

library(tidyverse)


### BRING IN THE DATA ----

# planned data
raw_plan <- read_csv("in/OR_PlannedUnitsLatLong 2010 to present.csv") %>%
  rename(Reg = "Registration #") 

# accomplished data (many data sheets)
#make a list of all accomplished data sheets
file_list <- list.files(path="in/", pattern = "OR_sheet", full.names=TRUE ) 
#read all into 1 df
raw_accomplish <- read_csv(file_list, id = "file_name") %>%
  rename(Reg = "Registration #") %>%
  rename(BurnDate = "Burn Date")


### MAKE DFs FROM THE DATA, remove weird formatting from the original files ---- 

# remove header rows from the data
# first, remove x characters from the registration number, this makes all registrations numbers numeric 
#and allows me to split them out from headers which are character
planned <- raw_plan %>% 
  mutate(Reg2 = gsub('xx', '', Reg)) %>% #remove 'xx'
  mutate(Reg3 = gsub('XX', '', Reg2)) %>% #remove 'XX'
  mutate(SOURCE_ID = gsub('x', '', Reg3)) %>% #this takes care of records that had 'xxxxx' in the original Reg column, some of the x's were removed in the previous mutates
  select(-c(Reg, Reg2, Reg3)) #remove the original Reg column and the intermediary Reg3 and Reg3 columns
accomplished <- raw_accomplish %>% 
  mutate(Reg2 = gsub('xx', '', Reg)) %>% #remove 'xx'
  mutate(Reg3 = gsub('XX', '', Reg2)) %>% #remove 'XX'
  mutate(SOURCE_ID = gsub('x', '', Reg3)) %>% #this takes care of records that had 'xxxxx' in the original Reg column, some of the x's were removed in the previous mutates
  select(-c(Reg, Reg2, Reg3)) #remove the original Reg column and the intermediary Reg3 and Reg3 columns
#delete all rows where the registration contains non-numeric values (these are county heading rows)
planned <- planned[!is.na(as.numeric(planned$SOURCE_ID)), ]
accomplished <- accomplished[!is.na(as.numeric(accomplished$SOURCE_ID)), ]
#remove empty columns
#there were lots of hidden, empty columns in original file
planned <- planned %>% discard(~all(is.na(.) | . =="")) 
accomplished <- accomplished %>% discard(~all(is.na(.) | . =="")) 
#remove unneeded columns
planned <- planned %>% select(-c(Legal, Time)) 
accomplished <- accomplished %>% select(-c(file_name, M10, M1000, Rain, Time))
# remove duplicates
planned <- unique(planned)
accomplished <- unique(accomplished)

### PLANNED DATA ----

# date 
planned$p_date <- mdy(planned$Date) 

# tidy
planned <- planned %>%
  # year
  mutate(p_year = year(p_date)) %>%
  # column names
  rename(p_SaleName = "Sale Name") %>%
  rename(p_Latitude = Latitude) %>%
  rename(p_Longitude = Longitude) %>%
  rename(p_BurnType = "Burn Type") %>%
  rename(p_Acres = Acres) %>%
  rename(p_Tons = Tons) %>%
  # remove unneeded column
  select(-(Date))

#write_csv(process_plan2, "out/TEST_OR_process_plan.csv")


### ACCOMPLISHED ----

# source id - remove scientific notation from the source id
accomplished$SOURCE_ID <- format(accomplished$SOURCE_ID, scientific = F)

#lat, lon
accomplished <- accomplished %>%
  separate_wider_delim("Legal / LatLong", "/", names = c("Legal", "latlon")) %>%
  separate_wider_delim(latlon, " -", names = c("LAT_PERMIT", "LON_PERMIT")) %>% #use the negative in longitude as a separator
  mutate(LON_PERMIT = as.numeric(LON_PERMIT)) %>%
  mutate(LON_PERMIT = LON_PERMIT * -1) %>% #add the negative sign back in for long
  mutate(LAT_PERMIT = as.numeric(LAT_PERMIT))

#burn type
accomplished <- accomplished %>%
  mutate(BURNTYPE_REPORTED = case_when(BT == "B" ~ "Broadcast Activity",
                                       BT == "F" ~ "Broadcast Natural",
                                       BT == "G" ~ "Grapple Pile",
                                       BT == "H" ~ "Handpile",
                                       BT == "L" ~ "Landing Only",
                                       BT == "N" ~ "Underburn Natural",
                                       BT == "R" ~ "Right of Way only",
                                       BT == "S" ~ "Rangeland",
                                       BT == "T" ~ "Tractor Pile",
                                       BT == "U" ~ "Underburn Activity")) %>%
#entity requesting
  mutate(ENTITY_REQUESTING = case_when(is.na(Owner) & OT == "S" ~ "State",
                                       is.na(Owner) & OT == "P" ~ "Private",
                                       is.na(Owner) & OT == "F" ~ "Federal",
                                       .default = Owner)) %>%
# burn status
# data are for completed burns, but I classify status as unknown when there were no completed acres
  mutate(BURN_STATUS = case_when(is.na(Acres) ~ "Unknown",
                                 Acres <= 0 ~ "Unknown",
                                 Acres > 0 ~ "Complete")) 

#date 
accomplished$BurnDate <- mdy(accomplished$BurnDate) 

# tidy
accomplished <- accomplished %>%
  # year
  mutate(year = year(BurnDate)) %>%
  #remove unneeded columns
  select(-c(Legal, BT, OT, BurnDate))

## JOIN PLANNED + ACCOMPLISHED
# join_all <- full_join(planned, accomplished, by = c("SOURCE_ID" = "SOURCE_ID"))
# join_all2 <- full_join(planned, accomplished, by = c("SOURCE_ID" = "SOURCE_ID",
#                                                    "p_year" = "year"))
join_all3 <- full_join(planned, accomplished, by = c("SOURCE_ID" = "SOURCE_ID",
                                                     "p_date" = "DATE"))
#look at source ID 077406121921 - 3 rows ????????????????

# planned only
join_planned_only <- join_all %>%
  # all accomplished burns have a LAT_PERMIT so if LAT_PERMIT is na, the burn was planned, not accomplished 
  filter(is.na(LAT_PERMIT))
  
# accomplished only
join_accomplished_only <- join_all %>%
  # all planned burns have a p_Latitude so if p_latitude is na the burn was accomplished, not planned  
  filter(is.na(p_Latitude))
  
# planned and accomplished
join_plan_accomplish <- join_all %>%
  # all burns that are planned and accomplished will have a LAT_PERMIT and a p_Latitude
  filter(!is.na(LAT_PERMIT) & !is.na(p_Latitude))


#### ---- FIGURE OUT PLANNED ACRES FOR PLANNED + ACCOMPLISHED BURNS


process <- join_plan_accomplish %>%
  # group by unique burn
  group_by(SOURCE_ID, LAT_PERMIT, LON_PERMIT, BURNTYPE_REPORTED) %>%
  # arrange planned date ascending
  arrange(p_date, .by_group = TRUE) 

# mutate field - order days 1...n day
# mutate field max requested acres, sum requested acres, sum completed
# ungroup
# summarize. e.g. if sum requested > sum completed ...
  
  













### ---- BELOW HERE IS STUFF FROM ORIGIONAL SCRIPT THAT JUST PROCESSED ACCOMPLISHED DATA
#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
or_ready <- process2 %>%
  #rename("SOURCE_ID" = "") %>% 
  #rename("DATE" = "") %>%
  #rename("ACRES_REQUESTED" = "") %>%
  #rename("ACRES_PERMITTED" = "") %>%
  rename("ACRES_COMPLETED" = "Acres") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "Sale Name") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  #rename("ENTITY_REQUESTING" = "") %>%
  #rename("LAT_PERMIT" = "") %>%
  #rename("LON_PERMIT" = "") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  rename("TONS" = "Tons") %>%
  #rename("BURN_STATUS" = "") %>%
  select(any_of(c("SOURCE_ID", "DATE", "ACRES_REQUESTED", "ACRES_PERMITTED", "ACRES_COMPLETED", "PILE_VOLUME", 
                  "BURN_NAME", "BURNTYPE_REPORTED", "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", 
                  "LEGAL_DESCRIP", "TONS", "BURN_STATUS")))  %>%
  distinct()


### EXPORT ----

write_csv(or_ready, "out/or_ready.csv")


### NOTE ----
# the output csv contains a burn name formatted as a date in excel. 
# the burn name is "March 22", entity requesting was "Petersen Forests LLC"
# check that the name comes back into the RX database as "March 22"