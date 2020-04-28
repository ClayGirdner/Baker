library(magrittr)
library(dplyr)
library(ggplot2)
library(nflscrapR)
library(tidyverse)
library(ggrepel)
library(ggalt)
library(readxl)
library(ggimage)
library(viridis)

# Theme for plots
source("Code/theme_pff.R")

# Load in SIS data
SIS_2019 <- read_xlsx("SIS_data.xlsx", sheet = "2019_agg")

# Join logo URLs
nfl_logos <- read_csv("nfl_team_logos.csv")
SIS_2019 <- left_join(SIS_2019, nfl_logos[,c("mascot", "url")], by = c("team" = "mascot"))


# Run PCA calculations
PCA <- prcomp(SIS_2019[c(25:31)], center = TRUE, scale = TRUE)
PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)

# Append PC values
SIS_2019$PC1 <- PCA$x[,"PC1"]
SIS_2019$PC2 <- PCA$x[,"PC2"]
SIS_2019$PC3 <- PCA$x[,"PC3"]

# Temporary tibble created to append arrow data to qbs_19
temp <- tibble(team = as.character(c("Play_Action%",
                                     "Shotgun%",
                                     "Screen%",
                                     "Bootleg%",
                                     "Avg_Drop_Depth",
                                     "aDOT",
                                     "Avg_#_WR")),
               dropbacks = 0,
               url = "blah",
               PC1 = PCAloadings$PC1 * 5.5,
               PC2 = PCAloadings$PC2 * 5.5,
               PC3 = PCAloadings$PC3 * 5.5)

# Further trim unneeded columns
# bind rows from temp
SIS_2019_trimmed <- SIS_2019 %>%
    select(team, dropbacks, url, PC1, PC2, PC3) %>%
    bind_rows(temp)

# Calculate % of variance for each PC
pov <- PCA$sdev^2/sum(PCA$sdev^2)

# Plot PCA
SIS_2019_trimmed %>%
    ggplot(aes(x=PC1, y=PC2)) +
    # Add variable lines from origin
    geom_segment(data = filter(SIS_2019_trimmed,
                               dropbacks == 0),
                 aes(x = 0, y = 0,
                     xend = (PC1),
                     yend = (PC2)),
                 color="#787878",
                 linetype = "dashed",
                 alpha = 0.5,
                 arrow = arrow(length = unit(0.2,"cm"))) +
    # Add points
    geom_point(data = filter(SIS_2019_trimmed, dropbacks > 0),
               aes(color = PC3),
               size = 3) +
    scale_color_viridis(option = "plasma") +
    # Add team labels
    geom_text_repel(aes(label = team),
              data = filter(SIS_2019_trimmed, dropbacks > 0)) +
    # Add labels for variable names
    geom_text(aes(label = team),
              data = filter(SIS_2019_trimmed, dropbacks == 0 & team != "Avg_#_WR"),
              color = "#787878",
              vjust = "outward", hjust = "outward") +
    # Special label for "Avg_#_WR" since it was going off frame
    geom_text(aes(label = team),
              data = filter(SIS_2019_trimmed, team == "Avg_#_WR"),
              color = "#787878",
              vjust = "outward") +
    # PFF theme
    theme_pff +
    # Cosmetics
    theme(panel.grid.major = element_blank(),
          legend.position = c(0.15,0.55)) +
    labs(title = "Cards Playing 3D Chess",
         subtitle = "principal component analysis, 2019 passing schemes",
         caption = "data from nflscrapR and Sports Info Solutions")

ggsave("Images/Off_PCA_2019_3D.png", height=6, width=9)
