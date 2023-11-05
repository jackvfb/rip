library(PAMpal)
library(tidyverse)

# MAKE STUDY FOR EACH SPECIES ---------------------------------------------
Kosp <-  filter(myStudy, species == "Kosp")
Phda <-  filter(myStudy, species == "Phda")
Phph <-  filter(myStudy, species == "Phph")
myStudies <- list("Kosp" = Kosp, "Phda" = Phda, "Phph" = Phph)

# MAKE GENERALIZED SPECTRA ------------------------------------------------
allSpec <- lapply(myStudies, getAllSpectra)
sumSpec <- lapply(allSpec, summarizeSpectra)
spectra <- list_rbind(sumSpec, names_to = "species")