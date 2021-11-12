###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(tidyverse)
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here( "files","raw_data","SympAct_Any_Pos.Rda")

#load data. 

rawdata <- readRDS(data_location)

View(rawdata)
#735 entries, 63 total columns

d1 <- rawdata %>% select(-contains(c("Score", "Total", "FluA", "FluB", "DxName", "Activity"))) 
#735 entries, 33 total columns
View(d1)

d2 <- d1%>% select(-Unique.Visit)
#735 entries, 32 total columns
View(d2)

processeddata <- d2 %>% drop_na()
# 730 entries, 32 total columns
View(processeddata)



# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("files","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


