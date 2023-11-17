# This script takes OR "accomplished" data and processes it so it is ready to be used in the Rx database.
# OR also has "Planned" data, which could be added here to understand what is planned but not accomplished.

library(tidyverse)


### BRING IN THE DATA ----

#make a list of all OR data sheets
file_list <- list.files(path="in/", pattern = "OR_sheet", full.names=TRUE ) 
#read all OR sheets into 1 df
raw <- read_csv(file_list, id = "file_name") %>%
  rename(Reg = "Registration #") %>%
  rename(BurnDate = "Burn Date")


### PROCESS: MAKE MASTER TABLE ----

#remove x characters from the registration number, this makes all registrations numbers numeric so I can separate out headers in next step
process <- raw %>% 
  mutate(Reg2 = gsub('xx', '', Reg)) %>% #remove 'xx'
  mutate(Reg3 = gsub('XX', '', Reg2)) %>% #remove 'XX'
  mutate(SOURCE_ID = gsub('x', '', Reg3)) %>% #this takes care of records that had 'xxxxx' in the original Reg column, some of the x's were removed in the previous mutates
  select(-c(Reg, Reg2, Reg3)) #remove the original Reg column and the intermediary Reg3 and Reg3 columns
  
#delete all rows where the registration contains non-numeric values (these are county heading rows)
process <- process[!is.na(as.numeric(process$SOURCE_ID)), ]

#remove empty and unneeded columns
process <- process %>% discard(~all(is.na(.) | . =="")) #there were lots of hidden, empty columns in original file
process <- process %>% select(-c(file_name, M10, M1000, Rain, Time))


### PROCESS: FORMAT FOR RX DATABASE ----

#lat, lon
process2 <- process %>%
  separate_wider_delim("Legal / LatLong", "/", names = c("Legal", "latlon")) %>%
  separate_wider_delim(latlon, " -", names = c("LAT_PERMIT", "LON_PERMIT")) %>% #use the negative in longitude as a separator
  mutate(LON_PERMIT = as.numeric(LON_PERMIT)) %>%
  mutate(LON_PERMIT = LON_PERMIT * -1) %>% #add the negative sign back in for long
  mutate(LAT_PERMIT = as.numeric(LAT_PERMIT))

#burn type
process2 <- process2 %>%
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
mutate(BURN_STATUS = case_when(Acres <= 0 ~ "Unknown", 
                               Acres > 0 ~ "Complete"))

#date 
process2$DATE <- mdy(process2$BurnDate) 

#xwalk to the rx database column names and formats 
#(all rx database attributes are listed, commented out attributes are not present in the state data or do not need to be renamed)
or_ready <- process2 %>%
  #rename("SOURCE_ID" = "") %>% 
  #rename("DATE" = "") %>%
  #rename("PERMITTED_ACRES" = "") %>%
  rename("COMPLETED_ACRES" = "Acres") %>%
  #rename("PILE_VOLUME" = "") %>%
  rename("BURN_NAME" = "Sale Name") %>%
  #rename("BURNTYPE_REPORTED" = "") %>%
  #rename("ENTITY_REQUESTING" = "") %>%
  #rename("LAT_PERMIT" = "") %>%
  #rename("LON_PERMIT" = "") %>%
  #rename("LEGAL_DESCRIP" = "") %>%
  rename("TONS" = "Tons") %>%
  #rename("BURN_STATUS" = "") %>%
  select(any_of(c("SOURCE_ID", "DATE", "PERMITTED_ACRES", "COMPLETED_ACRES", "PILE_VOLUME", "BURN_NAME", "BURNTYPE_REPORTED", 
                  "ENTITY_REQUESTING", "LAT_PERMIT", "LON_PERMIT", "LEGAL_DESCRIP", "TONS", "BURN_STATUS"))) %>%
  distinct()


### EXPORT ----

write_csv(or_ready, "out/or_ready.csv")


### NOTE ----
# the output csv contains a burn name formatted as a date in excel. 
# the burn name is "March 22", entity requesting was "Petersen Forests LLC"
# check that the name comes back into the RX database as "March 22"