# run all the pre processing scripts 

source("r/preprocess_AZ.r")
rm(list = ls()) #clear the environment before running the next script
source("r/preprocess_CA.r")
rm(list = ls())
source("r/preprocess_CO.r")
rm(list = ls())
#source("r/preprocess_ID.r") #ISSUE HERE; WAIT FOR NEW ID DATA
rm(list = ls())
source("r/preprocess_MT.r")
rm(list = ls())
source("r/preprocess_NM.r")
rm(list = ls())
source("r/preprocess_NV.r")
rm(list = ls())
source("r/preprocess_OR.r")
rm(list = ls())
source("r/preprocess_UT.r")
rm(list = ls())
source("r/preprocess_WA.r")
rm(list = ls())
source("r/preprocess_WY.r")
rm(list = ls())