library(randomForest)
library(rfPermute)

# TRAIN RF CLICK CLASSIFIER -----------------------------------------------
#data frame needed to train model
clicks <- readRDS("processed_data/clicks.rds")

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

# NEEDED PLOTS ------------------------------------------------------------
impPreds <- plotImpPreds(rf, rf_data, "species")
confMatr <- plotConfMat(rf)

# SAVE --------------------------------------------------------------------
saveRDS(rf, file = "processed_data/click_rf.rds")
saveRDS(impPreds, file = "processed_data/importance_predictors.rds")
saveRDS(confMatr, file = "processed_data/confusion_matrix.rds")
