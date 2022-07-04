# Load libraries
library(tidyverse)
library(colorspace)

# Read rds
pc4.sf.seroincidence <- readRDS(".../Rds files/pc4.sf.seroincidence.Rds")

# Read shapefile
pienter_municipalities <- st_read(
  dsn = paste0(".../Gemeentegrenzen_pienterII_20062007.shp")) %>%
  # Set CRS to 29882 (ignore warning)
  st_set_crs(28992)

# Create pienter municipalities and seroincidence dataset without NAs in municipalities
pienter_municipalities <- pienter_municipalities[!is.na(pienter_municipalities$GM_NAAM),]
pc4.sf.seroincidence2 <- pc4.sf.seroincidence[!is.na(pc4.sf.seroincidence$seroincidence),]

# Colour palette
my_grey <- c("0.0" = "#FFFFFF", "0.0-1.0" ="#BDBDBD", "1.0-3.0" = "#737373", "3.0-8.0" = "#525252"
             , ">8.0" = "#000000")

# Create categories pienter seroincidence data
pc4.sf.seroincidence2 <- pc4.sf.seroincidence2 %>%
  mutate(
    srncdnc_cat = ifelse(seroincidence == 0, "0.0", NA),
    srncdnc_cat = ifelse(seroincidence > 0 & seroincidence <= 1.0, "0.0-1.0", srncdnc_cat),
    srncdnc_cat = ifelse(seroincidence > 1.0 & seroincidence <= 3.0, "1.0-3.0", srncdnc_cat),
    srncdnc_cat = ifelse(seroincidence > 3.0 & seroincidence <= 8.0, "3.0-8.0", srncdnc_cat),
    srncdnc_cat = ifelse(seroincidence > 8.0, "> 8.0", srncdnc_cat),
    srncdnc_cat = as.factor(srncdnc_cat)
      )

# Figure S13
figure_si_pienter <- ggplot() +
  geom_sf(data = pc4.sf.seroincidence2,
          mapping = aes(fill = srncdnc_cat),
          size  = 0.1) +
  scale_fill_manual(values = my_grey) +
  labs(
    fill  = "") +
  geom_sf(data = pc4.sf.seroincidence,
          alpha = 0,
          size = 0.2,
          colour = "lightgrey") +
  geom_sf(
    data = pienter_municipalities,
    mapping = aes(shape = "Pienter-II municipalities"),
    size = 0.2,
    colour = "red",
    fill = NA) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank()) +
  coord_sf(datum = NA)
  # guides(
  #   # zet de vulkleur als eerste legende
  #   fill = guide_coloursteps(order = 1))

figure_si_pienter

# Save figure as PDF
ggsave(
  filename = ".../Figure S13.pdf",
  plot = figure_si_pienter,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S13.png",
  plot = figure_si_pienter,
  width = 18, height = 18, units = "cm",
  dpi = 1000)