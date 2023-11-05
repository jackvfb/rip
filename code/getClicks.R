library(tidyverse)
library(PAMpal)


# READ DATA ----------------------------------------------------------------
myStudy <- readRDS('data/myStudy.rds')

# FILTER/CLEAN ------------------------------------------------------------
#Only data with known species ID
myStudy <- filter(myStudy, species != "NBHF")

#Only detections on Channel 1
myStudy <- filter(myStudy, Channel == 1)

#Further cleaning to be performed on data frame
dets <- as_tibble(getClickData(myStudy))

clicks <- dets %>%
  #remove repeat UIDs, don't know why these exist but they do
  distinct(UID, .keep_all = TRUE) %>% 
  #drop dets with NA values, with some exceptions for variables like Lat/Lon
  drop_na(-c(Latitude, Longitude, gpsUncertainty, angle, angleError))

metadata <- clicks %>%
  #select event metadata
  select(UID, eventId, species) %>% 
  #group by event to calculate n
  group_by(eventId) %>%
  #summarize
  summarize(n = n(), species = unique(species)) %>%
  arrange(desc(n))

# SAVE --------------------------------------------------------------------
saveRDS(myStudy, "processed_data/ripStudy.rds")
saveRDS(clicks, "processed_data/clicks.rds")
saveRDS(metadata, "processed_data/metadata.rds")