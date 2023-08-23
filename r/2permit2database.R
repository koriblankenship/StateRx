#take state permit data and move it into the state rx database format

library(tidyverse)


############################   INPUT REQUIRED

state <- "AZ" #ENTER: 2 digit state abbreviation
#year <- "2021" #ENTER: 4 digit year (e.g. "2020") or year range (e.g. "2010-20")
#source <- "CO Dept. of Public Health and Environment" #ENTER: name of agency or system from which record was obtained (e.g. "WA Dept. Natural Resources")

#############################


### SETUP 
statepath <- paste0("state_data/", state, "/") #path to state data
attpath <- paste0("state_data", "/", "attributexwalk.csv")

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


### LOAD DATA

# permit input file
state_data <- read_csv(paste0(statepath, state, "_ready", ".csv"))


# attribute xwalk
attxwalk <- read_csv(attpath)


### XWALK
#for a state, make a df of matching rows from the xwalk 
state_xwalk <- attxwalk %>%
  select(DATABASE_NAME, state) %>%
  drop_na(state)

#make a vector of the state column names that xwalk to the database
(state_cols <- state_xwalk$AZ) #change AZ to the state variable

#select the matching columns from the state data and rename them to the database name
state_subset <- state_data %>%
  select(all_of(state_cols)) %>%
  rename_with(~ recode(.x, !!!setNames(state_xwalk$DATABASE_NAME, state_xwalk$AZ)))

#join the state subset to the database structure and add the state name
database_state <- database_template %>%
  bind_rows(database, state_subset) %>%
  mutate(STATE = state)



### POSTPROCESS
#calculate year from PERMIT_DATE
#calculate DOY from PERMIT_DATE
#calculate BURNTYPE_CLASSIFIED, build x-walk as we go, build routine to classify based on burn name field
#calculate BURN_STATUS
#calculate source from xwalk that includes source for all states?


#AZ: get the burn type from the burn number

### EXPORT











#********************************************ARCHIVE
### COMPARE COLUMN NAMES FOR ALL YEARS

#Define a function to get variable names and types
fun_compare_cols <- function(data_frame){
  x <- tibble(`var_name` = colnames(data_frame),
              `var_type` = sapply(data_frame, class))
  return(x)
}

#Make a df that compares column names and types
compare_cols <- lapply(1:length(files),function(i)fun_compare_cols(files[[i]]) %>% 
                   mutate(element =i)) %>%
  bind_rows() %>%
  spread(element, var_type)


###   MAKE COL NAMES IDENTICAL
#use this to list the new columnames
#https://www.geeksforgeeks.org/change-column-name-of-a-given-dataframe-in-r/
colnames(df) <- c('C1','C2','C3')

############# OR XWALK ALL POSSIBLE COL NAMES TO OUR DATBASE NAMES
#create a xwalk csv for every state w/ all possible col names from the source data and our dbase name

# could have datbase template as a dataframe
#use a series of mutates to populate fields like state and year
#use x-walk to populate other fields


#file_list <- list.files(statepath, pattern = "csv", full.names = TRUE)
#files <- lapply(file_list, read_csv)
