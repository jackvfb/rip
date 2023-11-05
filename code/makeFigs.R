library(tidyverse)
library(ggtext)

#Define color palette and labels common to all figs
spNames <-  c("Kosp"="*Kogia* spp.", "Phda"="Dall's porpoise", "Phph"="Harbor porpoise")
spCols <-  c("Phda"='#F8766D', "Kosp"='#00BA38',"Phph"='#619CFF')

# FIGURE 1. EVENTS --------------------------------------------------------
#bar graph showing depth of sampling for all events

#data objects needed
clicks <- readRDS("processed_data/clicks.rds")
metadata <- readRDS("processed_data/metadata.rds")

#click counts to be used in the legend labels for each species
counts <- clicks %>% count(species)

#define legend labels
legLabs = c(
  "Phph" = paste("**Harbor porpoise**<br>", counts[3,2], "clicks", sep = " "),
  "Phda" = paste("**Dall's porpoise**<br>", counts[2,2], "clicks", sep = " "),
  "Kosp" = paste("***Kogia*** **spp**.<br>", counts[1,2], "clicks", sep = " "))

metadata %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(size)) %>%
  ggplot(aes(x = size, y = n, fill = species)) +
  geom_bar(stat = "identity") +
  scale_y_log10(expand = c(0,0)) +
  scale_fill_manual(values = spCols,
                    labels = legLabs) +
  labs(x="Event Number",
       y="Clicks in Event",
       title = "Ground-truth NBHF Events by Species",
       subtitle = "Recordings acquired 2016-2018") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        legend.text = element_markdown(hjust = 0.5, size = 12),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "inches"),
        axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

ggsave("figs/events.png", height = unit(7, "inches"), width = unit(12, "inches"))

# FIGURE 2. GENERALIZED SPECTRA -------------------------------------------
spectra <- readRDS("processed_data/spectra.rds")

spectra %>%
  ggplot() +
  geom_line(aes(freq/1000, mn, color = species), size = 1) +
  geom_ribbon(aes(freq/1000, ymin = lci, ymax = uci, fill = species), alpha = 0.3) +
  labs(title = "NBHF Click Spectra by Species",
       fill = "Interquartile range",
       color = "Median") +
  xlab("Frequency (kHz)") +
  ylab("Magnitude (dB)") +
  scale_fill_manual(values = spCols, labels = spNames) +
  scale_color_manual(values = spCols, labels = spNames) +
  theme(legend.text = element_markdown(),
        plot.title = element_text(hjust = 0.5, size = 18))

ggsave("figs/spectra.png", height = unit(7, "inches"), width = unit(12, "inches"))

# FIGURE 3. IMPORTANCE OF PREDICTORS --------------------------------------
#load saved ggplot object
impred <- readRDS("processed_data/importance_predictors.rds")

#make labeller for facet plot
facets <- as_labeller(c(BW_3dB = "Bandwidth (kHz)",
                        fmax_3dB = "Frequency~max~ (kHz)",
                        centerkHz_3dB = "Frequency~center~ (kHz)",
                        peak = "Frequency~peak~(kHz)",
                        fmin_3dB = "Frequency~min~ (kHz)",
                        duration = "Duration(\u03bcs)"))

impred +
  aes(color = `.class.`) +
  scale_colour_manual(values = spCols) +
  scale_x_discrete(labels = spNames) +
  facet_wrap(~ var, strip.position = "left", scales = "free_y", nrow = 2,
             labeller = facets) +
  labs(title = "Click Parameters of NBHF Species") +
  theme(axis.text.x = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text.y.left = element_markdown(),
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_markdown(hjust = 0.5, size = 12, color = 'grey30'),
        legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(1, "cm"))

ggsave("figs/parameters.jpg", scale = 3)

# FIG 4. CONFUSION MATRIX ------------------------------------------------
confmatr <- readRDS("processed_data/confusion_matrix.rds")

confmatr +
  scale_x_discrete(labels = spNames, position = "top") +
  scale_y_discrete()
  labs(title = "Confusion Matrix of NBHF Click Classification Model") +
  theme(axis.text.y = element_markdown(), axis.text.x.top = element_markdown(),
        plot.title = element_text(hjust = 0.5, size = 18))

ggsave("figs/confusion.jpg", scale = 3)
