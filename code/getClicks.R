library(tidyverse)
library(PAMpal)


myStudy <- readRDS('data/myStudy.rds')

#Only data with known species ID
myStudy <- filter(myStudy, species != "NBHF")
#Only detections on Channel 1
myStudy <- filter(myStudy, Channel == 1)
clicks <- as_tibble(getClickData(myStudy)) %>%
  #remove repeat UIDs, don't know why these exist but they do
  distinct(UID, .keep_all = TRUE) %>% 
  #drop dets with NA values, with some exceptions for variables like Lat/Lon
  drop_na(-c(Latitude, Longitude, gpsUncertainty, angle, angleError))