# Load libraries
library(base)
library(gridExtra)
library(grid)
# Use this package for hexagon in legend
library(ggplot.multistats)

# Read Rds
incidence_SE <- readRDS(".../Rds files/incidence_SE.rds")
incidence_ST <- readRDS(".../Rds files/incidence_ST.rds")
incidence_cattle <- readRDS(".../Rds files/incidence_cattle.rds")
incidence_layers <- readRDS(".../Rds files/incidence_layers.rds")
incidence_broilers <- readRDS(".../Rds files/incidence_broilers.rds")
incidence_pigs <- readRDS(".../Rds files/incidence_pigs.rds")
covered_hex_salmonella <- readRDS(".../Rds files/covered_hex_salmonella.rds")

# Create and relevel factors for proper legend
incidence_SE$incidence_cat <- as.factor(incidence_SE$incidence_cat)
incidence_SE$incidence_cat <- factor(incidence_SE$incidence_cat, levels = c("0.0-0.5","0.5-1.5","1.5-2.5","2.5-4.0","4.0-6.0","6.0-10.0",">10.0"))
incidence_ST$incidence_cat <- as.factor(incidence_ST$incidence_cat)
incidence_ST$incidence_cat <- factor(incidence_ST$incidence_cat, levels = c("0.0-0.5","0.5-1.5","1.5-2.5","2.5-4.0","4.0-6.0","6.0-10.0",">10.0"))
incidence_cattle$incidence_cat <- as.factor(incidence_cattle$incidence_cat)
incidence_cattle$incidence_cat <- factor(incidence_cattle$incidence_cat, levels = c("0.0-0.5","0.5-1.5","1.5-2.5","2.5-4.0","4.0-6.0","6.0-10.0",">10.0"))
incidence_layers$incidence_cat <- as.factor(incidence_layers$incidence_cat)
incidence_layers$incidence_cat <- factor(incidence_layers$incidence_cat, levels = c("0.0-0.5","0.5-1.5","1.5-2.5","2.5-4.0","4.0-6.0","6.0-10.0",">10.0"))
incidence_broilers$incidence_cat <- as.factor(incidence_broilers$incidence_cat)
incidence_broilers$incidence_cat <- factor(incidence_broilers$incidence_cat, levels = c("0.0-0.5","0.5-1.5","1.5-2.5","2.5-4.0","4.0-6.0","6.0-10.0",">10.0"))
incidence_pigs$incidence_cat <- as.factor(incidence_pigs$incidence_cat)
incidence_pigs$incidence_cat <- factor(incidence_pigs$incidence_cat, levels = c("0.0-0.5","0.5-1.5","1.5-2.5","2.5-4.0","4.0-6.0","6.0-10.0",">10.0"))

# Create color palette
my_grey <- c("0.0-0.5" = "#FFFFFF", "0.5-1.5" = "#F0F0F0", "1.5-2.5" ="#BDBDBD", "2.5-4.0" = "#969696", "4.0-6.0" = "#737373", "6.0-10.0" = "#525252"
             , ">10.0" = "#000000")

# Create plots
SE <- ggplot(
          data = incidence_SE,
          mapping = aes(fill = incidence_cat)) +
          geom_sf(size = 0.1) +
          scale_fill_manual(values = my_grey) +
          labs(
            title = expression(paste("A) ", italic("Salmonella"), " Enteritidis"))) +
          theme(
            panel.background = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(size = 10),
            legend.position = "none") +
          coord_sf(datum = NA) +
          geom_sf(
            data = covered_hex_salmonella,
            size = 0.2,
            aes(colour = "Covered"), fill = NA
          ) +
          theme(
            panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "none") +
          coord_sf(datum = NA)

ST <- ggplot(
  data = incidence_ST,
  mapping = aes(fill = incidence_cat)) +
  geom_sf(size = 0.1) +
  scale_fill_manual(values = my_grey) +
  labs(
    title = expression(paste("B) ", italic("Salmonella"), " Typhimurium*"))) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 10),
    legend.position = "none") +
  coord_sf(datum = NA) +
  geom_sf(
    data = covered_hex_salmonella,
    size = 0.2,
    aes(colour = "Covered"), fill = NA
  ) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none") +
  coord_sf(datum = NA)

SCattle <- ggplot(
  data = incidence_cattle,
  mapping = aes(fill = incidence_cat)) +
  geom_sf(size = 0.1) +
  scale_fill_manual(values = my_grey) +
  labs(
    title = expression(paste("C) ", italic("Salmonella"), " source - cattle"))) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 10),
    legend.position = "none") +
  coord_sf(datum = NA) +
  geom_sf(
    data = covered_hex_salmonella,
    size = 0.2,
    aes(colour = "Covered"), fill = NA
  ) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none") +
  coord_sf(datum = NA)

SLayers <- ggplot(
  data = incidence_layers,
  mapping = aes(fill = incidence_cat)) +
  geom_sf(size = 0.1) +
  scale_fill_manual(values = my_grey) +
  labs(
    title = expression(paste("D) ", italic("Salmonella"), " source - layers"))) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 10),
    legend.position = "none") +
  coord_sf(datum = NA) +
  geom_sf(
    data = covered_hex_salmonella,
    size = 0.2,
    aes(colour = "Covered"), fill = NA
  ) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none") +
  coord_sf(datum = NA)

SBroilers <- ggplot(
  data = incidence_broilers,
  mapping = aes(fill = incidence_cat)) +
  geom_sf(size = 0.1) +
  scale_fill_manual(values = my_grey) +
  labs(
    title = expression(paste("E) ", italic("Salmonella"), " source - broilers"))) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 10),
    legend.position = "none") +
  coord_sf(datum = NA) +
  geom_sf(
    data = covered_hex_salmonella,
    size = 0.2,
    aes(colour = "Covered"), fill = NA
  ) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none") +
  coord_sf(datum = NA)

SPigs <- ggplot(
  data = incidence_pigs,
  mapping = aes(fill = incidence_cat)) +
  geom_sf(size = 0.1) +
  scale_fill_manual(values = my_grey) +
  labs(
    title = expression(paste("F) ", italic("Salmonella"), " source - pigs"))) +
    #caption = "*including its monophasic variants") +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 10),
    legend.position = "none",
    plot.caption= element_text(size=8,
                               color="darkgrey")) +
                               #face="bold",
                               #vjust = 5)) +
  coord_sf(datum = NA) +
  geom_sf(
    data = covered_hex_salmonella,
    size = 0.2,
    aes(colour = "Covered"), fill = NA
  ) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none") +
  coord_sf(datum = NA)

# create figure pigs including legend to get legend out
SPigs_legend <- ggplot(
  data = incidence_pigs,
  mapping = aes(fill = incidence_cat)) +
  geom_sf(size = 0.1) +
  scale_fill_manual(values = my_grey) +
  labs(
    title = expression(paste("F) ", italic("Salmonella"), " source - pigs"))) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 10)) +
    #legend.position = "none") +
  coord_sf(datum = NA) +
  geom_sf(
    data = covered_hex_salmonella,
    size = 0.2,
    aes(colour = "Covered"), fill = NA,
    # key_glyph is functionality for hexagons from ggplot.multistats package
    key_glyph = 'hexagon'
  ) +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.key =  element_blank()) +
    #legend.position = "none") +
  coord_sf(datum = NA)

# Get legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(SPigs_legend)

# Arrange figures in a matrix including legend
fig1 <- grid.arrange(SE, ST, SCattle, SLayers, SBroilers, SPigs, legend,
             # nrow = 3, ncol = 2)
             layout_matrix=rbind(c(1,1,2,2,7), c(3,3,4,4,7), c(5,5,6,6,7)))

fthbar <- grobTree(rectGrob(gp=gpar(fill=NA,col=NA)),
                   textGrob("*including its monophasic variants ", x=1, hjust=1, 
                            gp = gpar(col = "black", fontsize = 9)), 
                   cl="ann")
heightDetails.ann <- function(x) unit(1,"line")

fig1_foot <- grid.arrange(fig1, bottom = fthbar)

# Save as pdf
ggsave(
  filename = ".../Figure 1.pdf",
  plot = fig1_foot,
  width = 18, height = 18, units = "cm",
  dpi = 1000)

# Save as png
ggsave(
  filename = ".../Figure 1.png",
  plot = fig1_foot,
  width = 18, height = 18, units = "cm",
  dpi = 1000)
