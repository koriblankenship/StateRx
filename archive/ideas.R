#read in all state files?
#myfiles <- list.files(pattern = ".csv")
#mydata <- map_df(myfiles, read.csv, strin)gsAsFactors = FALSE)

library(tidyverse)
statepath <- "state_data/" #path to state data
file_list <- list.files(statepath, pattern = "csv", full.names = TRUE)
file_list

#mydata <- map_df(file_list, read_csv)
mydata_justmap <- map(file_list, read_csv)


# attribute xwalk
attxwalk <- read_csv(paste0(statepath, "attributexwalk.csv"))


### XWALK
#for the state, select columns from attxwalk that have a database match
col_match <- attxwalk %>%
  select(AZ) %>%
  drop_na(AZ) 