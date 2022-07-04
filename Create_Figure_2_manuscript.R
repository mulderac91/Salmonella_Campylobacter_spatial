# Load libraries
library(base)
library(gridExtra)
library(patchwork)
# Use this package for hexagon in legend
library(ggplot.multistats)

# Read Rds
incidence_jejuni <- readRDS(".../Rds files/incidence_jejuni.rds")
incidence_coli <- readRDS(".../Rds files/incidence_coli.rds")
covered_hex_campylobacter <- readRDS(".../Rds files/covered_hex_campylobacter.rds")

# Create and relevel factors for proper legend
incidence_jejuni$incidence_cat <- as.factor(incidence_jejuni$incidence_cat)
incidence_jejuni$incidence_cat <- factor(incidence_jejuni$incidence_cat, levels = c("0.0-3.0","3.0-10.0","10.0-20.0","20.0-30.0","30.0-40.0","40.0-60.0",">60.0"))
incidence_coli$incidence_cat <- as.factor(incidence_coli$incidence_cat)
incidence_coli$incidence_cat <- factor(incidence_coli$incidence_cat, levels = c("0.0-3.0","3.0-10.0","10.0-20.0","20.0-30.0","30.0-40.0","40.0-60.0",">60.0"))

# Create color palette
my_grey <- c("0.0-3.0" = "#FFFFFF", "3.0-10.0" = "#F0F0F0", "10.0-20.0" ="#BDBDBD", "20.0-30.0" = "#969696", "30.0-40.0" = "#737373", "40.0-60.0" = "#525252"
             , ">60.0" = "#000000")

# Create plots
jejuni <- ggplot(
          data = incidence_jejuni,
          mapping = aes(fill = incidence_cat)) +
          geom_sf(size = 0.1) +
          scale_fill_manual(values = my_grey) +
          labs(
            title = expression(paste("A) ", italic("Campylobacter jejuni")))) +
          theme(
            panel.background = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(size = 10),
            legend.position = "none") +
          coord_sf(datum = NA) +
          geom_sf(
            data = covered_hex_campylobacter,
            size = 0.2,
            aes(colour = "Covered"), fill = NA,
            # key_glyph is functionality for hexagons from ggplot.multistats package
            key_glyph = 'hexagon'
          ) +
          theme(
            panel.background = element_blank(),
            legend.title = element_blank(),
            legend.key =  element_blank(),
            legend.position = "none") +
          coord_sf(datum = NA)

coli <- ggplot(
  data = incidence_coli,
  mapping = aes(fill = incidence_cat)) +
  geom_sf(size = 0.1) +
  scale_fill_manual(values = my_grey) +
  labs(
    title = expression(paste("B) ", italic("Campylobacter coli")))) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 10),
    legend.position = "none") +
  coord_sf(datum = NA) +
  geom_sf(
    data = covered_hex_campylobacter,
    size = 0.2,
    aes(colour = "Covered"), fill = NA,
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank(),
    legend.position = "none") +
  coord_sf(datum = NA)

# Arrange figures and create figure 2 of manuscript using patchwork package
fig2 <- jejuni + coli & theme(legend.position = "right")
fig2 <- fig2 + plot_layout(guides = "collect")

# Save as pdf
ggsave(
  filename = ".../Figure 2.pdf",
  plot = fig2,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save as png
ggsave(
  filename = ".../Figure 2.png",
  plot = fig2,
  width = 18, height = 18, units = "cm",
  dpi = 1000)
