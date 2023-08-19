#take state permit data and move it into the state rx database format

library(tidyverse)


############################   INPUT REQUIRED
#state
state <- "co"

#xwalk state variables to rx database variables
#

#############################


### LOAD STATE DATA

#path to state data
statepath <- paste0("in/", state, "/")

#load state data
file_list <- list.files(statepath, pattern = "csv", full.names = TRUE)
files <- lapply(file_list, read_csv)


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

