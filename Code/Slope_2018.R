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

# Load in 2018 passing data
pass_18 <- load.transform.trim(season=2018)

# Split out Baker's early season and late season data
bake_early <- pass_18 %>%
    filter(name=="B.Mayfield" & week < 9)
bake_late <- pass_18 %>%
    filter(name=="B.Mayfield" & week > 8)

# Aggregate baker's stats for these two periods
bake_early_agg <- aggup(bake_early)
bake_late_agg <- aggup(bake_late)

# Aggregate the full season data for nfl qb's (including baker)
# Filter out guys with fewer than x dropbacks
qbs_18 <- aggup(pass_18)
qbs_18 <- filter(qbs_18, dropbacks>=100)

# Function to calculate percentiles
perc.rank <- function(x, xo)  length(x[x <= xo])/length(x)*100

# Stat lists
pos_stats <- names(qbs_18)[9:11]
neg_stats <- names(qbs_18)[12:13]

# Empty dataframe that will house baker's split data
bake_splits <- data.frame(period=factor(),
                          stat=factor(),
                          perc=numeric())

# Calculate Baker's percentiles by season split (for both pos and neg stats)
for (i in pos_stats){
    bake_splits <- rbind(bake_splits, 
                         data.frame(period = "Weeks 1-8",
                                    stat = i,
                                    perc = perc.rank(qbs_18[[i]],
                                                     bake_early_agg[[i]])))
}
for (i in neg_stats){
    bake_splits <- rbind(bake_splits, 
                         data.frame(period = "Weeks 1-8",
                                    stat = i,
                                    perc = 100 - perc.rank(qbs_18[[i]],
                                                           bake_early_agg[[i]])))
}
for (i in pos_stats){
    bake_splits <- rbind(bake_splits, 
                         data.frame(period = "Weeks 9-17",
                                    stat = i,
                                    perc = perc.rank(qbs_18[[i]],
                                                     bake_late_agg[[i]])))
}
for (i in neg_stats){
    bake_splits <- rbind(bake_splits, 
                         data.frame(period = "Weeks 9-17",
                                    stat = i,
                                    perc = 100 - perc.rank(qbs_18[[i]],
                                                           bake_late_agg[[i]])))
}

# Create slope plot
bake_splits %>%
    ggplot(aes(x=period, y=perc, group = stat, label = stat)) + 
    geom_line(size=5.5, color = "#000000") +
    geom_line(data=filter(bake_splits, stat == "INT_rate"),
              size=4, color="#5c3920") +
    geom_line(data=filter(bake_splits, stat != "INT_rate"),
              size=4, color="#fb4f14") +
    geom_point(size=6, color="#000000") +
    geom_text_repel(data=filter(bake_splits,
                                period == "Weeks 1-8" &
                                    stat %in% c("aDOT",
                                                "TD_rate",
                                                "epa_per_dropback")
    ),
    label = c("aDOT",
              "EPA",
              "Touchdowns"),
    size = 4,
    nudge_x = -0.3) + 
    geom_text_repel(data=filter(bake_splits,
                                period == "Weeks 9-17" &
                                    stat %in% c("sack_rate",
                                                "INT_rate")
                                ),
                    label = c("Interceptions", "Sacks"),
                    size = 4,
                    nudge_x = 0.3) + 
    labs(title = "Upward Momentum: Mayfield's Ascendant Rookie Season",
         x = NULL,
         y = "2018 Percentile",
         caption = "data from nflscrapR\nadjusted for total dropbacks (except aDOT)") +
    scale_x_discrete(position = "top") +
    ylim(0,100) +
    theme_pff +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_text(size = 13, color = "black"),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 13))

ggsave("Images/slope_2018.png", dpi=2000, height=7, width=7)