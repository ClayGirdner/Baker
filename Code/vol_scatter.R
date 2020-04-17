library(magrittr)
library(dplyr)
library(ggplot2)
library(nflscrapR)
library(tidyverse)
library(ggrepel)

# Theme for plots
source("Code/theme_pff.R")

# Import functions that load/process data
source("Code/Baker_functions.R")

# Load in passing data for 2018/2019 and combine
pass_18 <- load.transform.trim(season=2018)
pass_19 <- load.transform.trim(season=2019)
pass_tot <- rbind(pass_18, pass_19)

sd_pbp <- pass_tot %>%
    group_by(name) %>%
    summarize(pbp = sd(epa))

sd_gbg <- pass_tot %>%
    group_by(name, season, week) %>%
    summarize(dropbacks = sum(pass),
              epa = sum(epa),
              epa_per_db = epa/dropbacks) %>%
    filter(dropbacks >= 10) 

sd_both <- sd_gbg %>%
    group_by(name) %>%
    summarize(games = sum(!is.na(name)),
              gbg = sd(epa_per_db)) %>%
    filter(games > 16) %>%
    left_join(sd_pbp, by = "name") %>%
    left_join(unique(pass_19[, c("name", "posteam")]), by = "name") %>%
    left_join(nflteams, by = c("posteam" = "abbr"))

# Plot scatter
sd_both %>%
    ggplot(aes(x = gbg, y = pbp, label=name, color=primary)) +
    geom_point(data = filter(sd_both, !grepl("B.Mayfield", name)),
               alpha = 0.5) +
    geom_point(data = filter(sd_both, grepl("B.Mayfield", name))) +
    # we need this to assign colors
    scale_color_identity() +
    # Baker's label
    geom_text_repel(data = filter(sd_both, grepl("B.Mayfield", name)),
                    size = 6, hjust = 1, nudge_x = 0.04, fontface="bold") +
    # add labels for set of other players
    geom_text_repel(data = subset(sd_both, !grepl("Mayfield", name))) +
    labs(x = "Per Game (EPA/Dropback)",
         y = "Per Play (EPA)",
         caption = "data from nflscrapR\nminimum 17 games with at least 10 dropbacks",
         title = "Quarterback Volatility Around the NFL",
         subtitle = "standard deviation, 2018-2019") +
    # Pff theme
    theme_pff +
    # cosmetic adjustments
    theme(panel.grid.major = element_blank())

ggsave("Images/vol_scatter.png", dpi=2000, height=7, width=7)