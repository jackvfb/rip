library(randomForest)
library(rfPermute)

# TRAIN RF CLICK CLASSIFIER -----------------------------------------------
rf_data <- clicks %>%
  #will use these predictors
  select(species, peak, centerkHz_3dB, fmin_3dB, fmax_3dB, BW_3dB, duration) %>%
  mutate(species = as.factor(species)) %>% 
  as.data.frame()
ss <- balancedSampsize(rf_data$species)
rf <- randomForest(formula = species ~ .,
                   data = rf_data,
                   sampsize = ss,
                   proximity = TRUE,
                   importance = TRUE)