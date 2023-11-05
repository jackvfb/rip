library(PAMpal)
library(tidyverse)

# MAKE STUDY FOR EACH SPECIES ---------------------------------------------
ripStudy <- readRDS('processed_data/ripStudy.rds')
Kosp <-  filter(ripStudy, species == "Kosp")
Phda <-  filter(ripStudy, species == "Phda")
Phph <-  filter(ripStudy, species == "Phph")
myStudies <- list("Kosp" = Kosp, "Phda" = Phda, "Phph" = Phph)

# MAKE GENERALIZED SPECTRA ------------------------------------------------
allSpec <- lapply(myStudies, getAllSpectra)
sumSpec <- lapply(allSpectra, summarizeSpectra)
spectra <- list_rbind(sumSpec, names_to = "species")

# SAVE --------------------------------------------------------------------
saveRDS(spectra, "processed_data/spectra.rds")