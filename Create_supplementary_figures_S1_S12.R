# Load libraries
library(tidyverse)
library(colorspace)
# Use this package for hexagon in legend
library(ggplot.multistats)

# Read Rds
animal.2012 <- readRDS(".../Rds files/animal.hex.sf.2012.rds")
animal.2015 <- readRDS(".../Rds files/animal.hex.sf.2015.rds")
animal.2018 <- readRDS(".../Rds files/animal.hex.sf.2018.rds")
covered_hex_salmonella <- readRDS(".../Rds files/covered_hex_salmonella.rds")
covered_hex_campylobacter <- readRDS(".../Rds files/covered_hex_campylobacter.rds")

# Add years to animal data
animal.2012$year <- 2012
animal.2015$year <- 2015
animal.2018$year <- 2018

# Bind animal data into 1 tidy dataframe
animal_data <- rbind(animal.2012,animal.2015,animal.2018)

#---------------------------------------
# Figure S1: Salmonella - small ruminants

figure_s1 <- ggplot(
  data = animal_data,
  mapping = aes(fill = Kleine_Herkauwers_totaal)) +
  geom_sf(size  = 0.1) +
    scale_fill_continuous_sequential(
      palette = "Grays",
      trans = "log1p",
      breaks = c(0, 10, 100, 1000, 10000, 100000),
      labels = scales::comma_format()) +
    labs(
      fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_salmonella,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "red",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s1

# Save figure as PDF
ggsave(
  filename = ".../Figure S1.pdf",
  plot = figure_s1,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S1.png",
  plot = figure_s1,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S2: Campylobacter - small ruminants

figure_s2 <- ggplot(
  data = animal_data %>% 
    # only years 2015 & 2018
    filter(year %in% c(2015, 2018)),
  mapping = aes(fill = Kleine_Herkauwers_totaal)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_campylobacter,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "blue",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
    ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))
  
figure_s2

# Save figure as PDF
ggsave(
  filename = ".../Figure S2.pdf",
  plot = figure_s2,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S2.png",
  plot = figure_s2,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S3: Salmonella - dairy cows

figure_s3 <- ggplot(
  data = animal_data,
  mapping = aes(fill = Rund_Melk_totaal)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(
    fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_salmonella,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "red",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s3

# Save figure as PDF
ggsave(
  filename = ".../Figure S3.pdf",
  plot = figure_s3,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S3.png",
  plot = figure_s3,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S4: Campylobacter - dairy cows

figure_s4 <- ggplot(
  data = animal_data %>% 
    # only years 2015 & 2018
    filter(year %in% c(2015, 2018)),
  mapping = aes(fill = Rund_Melk_totaal)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_campylobacter,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "blue",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s4

# Save figure as PDF
ggsave(
  filename = ".../Figure S4.pdf",
  plot = figure_s4,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S4.png",
  plot = figure_s4,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S5: Salmonella - veal calves

figure_s5 <- ggplot(
  data = animal_data,
  mapping = aes(fill = Rund_Vlees_totaal)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(
    fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_salmonella,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "red",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s5

# Save figure as PDF
ggsave(
  filename = ".../Figure S5.pdf",
  plot = figure_s5,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S5.png",
  plot = figure_s5,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S6: Campylobacter - veal calves

figure_s6 <- ggplot(
  data = animal_data %>% 
    # only years 2015 & 2018
    filter(year %in% c(2015, 2018)),
  mapping = aes(fill = Rund_Vlees_totaal)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_campylobacter,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "blue",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s6

# Save figure as PDF
ggsave(
  filename = ".../Figure S6.pdf",
  plot = figure_s6,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S6.png",
  plot = figure_s6,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S7: Salmonella - laying hens

figure_s7 <- ggplot(
  data = animal_data,
  mapping = aes(fill = Kip_Leghen)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(
    fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_salmonella,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "red",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s7

# Save figure as PDF
ggsave(
  filename = ".../Figure S7.pdf",
  plot = figure_s7,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S7.png",
  plot = figure_s7,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S8: Campylobacter - laying hens

figure_s8 <- ggplot(
  data = animal_data %>% 
    # only years 2015 & 2018
    filter(year %in% c(2015, 2018)),
  mapping = aes(fill = Kip_Leghen)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_campylobacter,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "blue",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s8

# Save figure as PDF
ggsave(
  filename = ".../Figure S8.pdf",
  plot = figure_s8,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S8.png",
  plot = figure_s8,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S9: Salmonella - broilers

figure_s9 <- ggplot(
  data = animal_data,
  mapping = aes(fill = Kip_Kuiken)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(
    fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_salmonella,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "red",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s9

# Save figure as PDF
ggsave(
  filename = ".../Figure S9.pdf",
  plot = figure_s9,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S9.png",
  plot = figure_s9,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S10: Campylobacter - broilers

figure_s10 <- ggplot(
  data = animal_data %>% 
    # only years 2015 & 2018
    filter(year %in% c(2015, 2018)),
  mapping = aes(fill = Kip_Kuiken)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_campylobacter,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "blue",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s10

# Save figure as PDF
ggsave(
  filename = ".../Figure S10.pdf",
  plot = figure_s10,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S10.png",
  plot = figure_s10,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S11: Salmonella - pigs

figure_s11 <- ggplot(
  data = animal_data,
  mapping = aes(fill = Varkens_totaal)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(
    fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_salmonella,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "red",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s11

# Save figure as PDF
ggsave(
  filename = ".../Figure S11.pdf",
  plot = figure_s11,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S11.png",
  plot = figure_s11,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

#---------------------------------------
# Figure S12: Campylobacter - pigs

figure_s12 <- ggplot(
  data = animal_data %>% 
    # only years 2015 & 2018
    filter(year %in% c(2015, 2018)),
  mapping = aes(fill = Varkens_totaal)) +
  geom_sf(size  = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Grays",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma_format()) +
  labs(fill  = "") +
  facet_grid(cols = vars(year)) +
  geom_sf(
    data = covered_hex_campylobacter,
    mapping = aes(shape = "Covered"),
    size = 0.2,
    colour = "blue",
    fill = NA, 
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    # adapt strips of panels 
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12)) +
  coord_sf(datum = NA) +
  guides(
    # use fill colour as first legend item
    fill = guide_colourbar(order = 1))

figure_s12

# Save figure as PDF
ggsave(
  filename = ".../Figure S12.pdf",
  plot = figure_s12,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save figure as png
ggsave(
  filename = ".../Figure S12.png",
  plot = figure_s12,
  width = 18, height = 18, units = "cm",
  dpi = 1000)