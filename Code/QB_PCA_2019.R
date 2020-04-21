library(magrittr)
library(dplyr)
library(ggplot2)
library(nflscrapR)
library(tidyverse)
library(ggrepel)
library(ggalt)

# Theme for plots
source("Code/theme_pff.R")

# Import functions that load/process data
source("Code/Baker_functions.R")

# Load in 2019 passing data (nflscrapR)
pass_19 <- load.transform.trim(2019)

# Aggregate the full season data for nfl qb's (including baker)
# Filter out guys with fewer than x dropbacks
qbs_19 <- aggup(pass_19)
qbs_19 <- filter(qbs_19, dropbacks >= 100)

# Run PCA calculations
PCA <- prcomp(qbs_19[c(6:13)], center = TRUE, scale = TRUE)
PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)

# Append PC1 and PC2 values for each QB
qbs_19$PC1 <- PCA$x[,"PC1"]
qbs_19$PC2 <- PCA$x[,"PC2"]

# Temporary tibble created to append arrow data to qbs_19
temp <- tibble(name = as.character(c("Y/A",
                                     "Cmp%",
                                     "Y/G",
                                     "aDOT",
                                     "EPA/\ndropback",
                                     "TD%",
                                     "Int%",
                                     "Sack%")),
               dropbacks = 0,
               primary = "#787878",
               PC1 = PCAloadings$PC1 * 5.5,
               PC2 = PCAloadings$PC2 * 5.5)

# Further trim qbs_19 to make plot look better, drop unneeded columns,
# bind rows from temp to qbs_19 and create new tibble
qbs_19_trimmed <- qbs_19 %>%
    filter(dropbacks >= 350) %>%
    select(name, dropbacks, primary, PC1, PC2) %>%
    bind_rows(temp)


# Plot PCA
qbs_19_trimmed %>%
    ggplot(aes(x=PC1, y=PC2)) +
    # Add variable lines from origin
    geom_segment(data = filter(qbs_19_trimmed,
                               dropbacks == 0),
                 aes(x = 0, y = 0,
                     xend = (PC1),
                     yend = (PC2)),
                 color="#787878",
                 linetype = "dashed",
                 alpha = 0.5,
                 arrow = arrow(length = unit(0.2,"cm"))) +
    # Add points
    geom_point(aes(color=primary),
               data = filter(qbs_19_trimmed, dropbacks > 0)) +
    # Add labels for everything except Baker (including variable names)
    geom_text_repel(data = filter(qbs_19_trimmed, name != "B.Mayfield"),
                    aes(color=primary,
                        label=name)) +
    # Add label for Baker, need separate line because it's so different
    geom_text_repel(data = filter(qbs_19_trimmed, name == "B.Mayfield"),
                    aes(color=primary,
                        label = name),
                    fontface = "bold",
                    size = 6,
                    segment.color = "Black",
                    nudge_x = -0.75,
                    nudge_y = -0.50) +
    # Add shape around Baker's cohort
    geom_encircle(aes(x=PC1,y=PC2),
                  data = filter(qbs_19_trimmed, name %in% c("B.Mayfield",
                                                            "M.Trubisky",
                                                            "K.Allen",
                                                            "J.Allen",
                                                            "S.Darnold",
                                                            "D.Jones",
                                                            "A.Dalton",
                                                            "K.Murray"))) +
    
    # needed to automatically adjust the color of scatter points
    scale_color_identity() +
    # PFF theme
    theme_pff +
    # Cosmetics
    theme(panel.grid.major = element_blank()) +
    labs(title = "Clustered Among the Young (and Andy Dalton)",
         subtitle = "principal component analysis, 2019 passing stats",
         caption = "data from nflscrapR\nminimum 350 dropbacks")

ggsave("Images/QB_PCA_2019.png", dpi=2000, height=7, width=7)