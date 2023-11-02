library(tidyverse)

### LOAD DATA

# permit input files
state_file_list <- list.files(path="state_data", pattern = ".csv", full.names=TRUE ) 

###TC: SHOULD THIS BE MAPPED IN? OR JUST READ THE FILES IN A FOR LOOP?
state_data <- map(state_file_list, read_csv) #I use map here to bring in the permit data for each state (I have AZ and GA as test data; I made up GA)

# attribute xwalk
attribute_xwalk <- read_csv("in/attributexwalk_tidy_na.csv") %>%
  drop_na(STATE_NAME)
  
#nest the xwalk by state
# attribute_xwalk_nested <- attribute_xwalk %>% 
#   group_by(STATE) %>% 
#   nest() 
### or split by state
attribute_xwalk_split <- attribute_xwalk %>% 
  split(.$STATE)

#make the database df
database_template <- data.frame(ID = integer(),
                                SOURCE_ID = character(),
                                SOURCE = character(), 
                                STATE = character(),
                                YEAR = integer(),
                                PERMIT_DATE = character(), #read in as character and then format
                                BURN_DOY = integer(),
                                PERMITTED_ACRES = double(),
                                COMPLETED_ACRES = double(),
                                PILE_VOLUME = double(),
                                BURN_NAME = character(),
                                BURNTYPE_REPORTED = character(),
                                ENTITY_REQUESTING = character(),
                                LAT_PERMIT = double(),
                                LON_PERMIT = double(),
                                LEGAL_DESCRIP = character(),
                                TONS = double()
)


### TC MAKE THIS A FOR LOOP ! :)

###EXAMPLE USING AZ, this shows the process for a single state############
xwalk_az <- attribute_xwalk_split$AZ #pull at the AZ xwalk
state_az <- state_data[[1]] #pull out state data for AZ
state_select <- state_az %>% 
#  pick(xwalk_az$STATE_NAME) %>%
  select(all_of(xwalk_az$STATE_NAME)) %>% #select the columns listed in the xwalk from the state data
  rename_with(~ recode(.x, !!!setNames(xwalk_az$DATABASE_NAME, xwalk_az$STATE_NAME))) #rename the columns
database_state <- database_template %>% 
  bind_rows(database_template, state_select) %>% #bind state rows to the database structure 
  mutate(STATE = "AZ") #add the state name
##############################

#I read online that: Mapping on a dataframe means that you are trying to apply your function on each column
#this could mean that map is the wrong function since I'm selecting columns not applying a function
#everything below here is just ideas that I tried; nothing is working at this point
select_cols <- map(state_az, select(all_of(xwalk_az$STATE_NAME)))
select_cols <- map(state_az, select(xwalk_az$STATE_NAME))


#make AZ attributes as list - DO I NEED THIS??????
# az <- attribute_xwalk %>%
#   filter(STATE == "AZ") %>%
#   select(STATE_NAME)
### or
#make a vector of the state column names that xwalk to the database #repeats what is in the split - need this?
state_cols <- attribute_xwalk_split #change AZ to the state variable

state_subset <- map(state_data, keep(state_cols)) #pluck or keep
      
      state_subset <- state_data %>%
        select(all_of(state_cols)) %>%
        rename_with(~ recode(.x, !!!setNames(state_xwalk$DATABASE_NAME, state_xwalk$AZ)))

#select the first element of state_data/which is AZ
state_data %>% 
  map(1) #this gives the first column in each of 2 lists; I want the first list
map_dfr(got_chars, extract, c("name", "culture", "gender", "id", "born", "alive"))

#map(attribute_xwalk_nested, "data$STATE_NAME")
#y <- map(attribute_xwalk_nested, "AZ$data")
#x <- map(attribute_xwalk_nested, `[`, c("STATE", "data"))
#x
#unnesttest <- unnest(attribute_xwalk_nested)

#iterate over two lists at a time? state and match? or match and state data
##map2(.x, .y, .f, ...)
##map(INPUT_ONE, INPUT_TWO, FUNCTION_TO_APPLY, OPTIONAL_OTHER_STUFF)
###my_fun <- function(x, y) paste(x, "was born", y)
###map2_chr(nms, birth, my_fun) %>% head()

#make a function and then map?
#function example
##my_fun <- function(x) paste(x, collapse = " | ")
##map(aliases, my_fun)







