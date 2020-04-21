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
# Rename
bake_early_agg$name <- "Mayfield\nWeeks 1-8"
bake_late_agg$name <- "Mayfield\nWeeks 9-17"

# Aggregate the full season data for nfl qb's (including baker)
# Filter out guys with fewer than x dropbacks
qbs_18 <- aggup(pass_18)
qbs_18 <- filter(qbs_18, dropbacks>=100)

# Rename Baker
qbs_18$name <- if_else(qbs_18$name == "B.Mayfield", 
                   "Mayfield\nFull Season", qbs_18$name)

# Caclulate means
mean_comp <- mean(qbs_18$comp_percentage)
mean_YPA <- mean(qbs_18$YPA)

# Filter out even more to make for easier viewing
qbs_18 <- filter(qbs_18, dropbacks>=350)

# Append other Baker aggregates to qbs
qbs_18 <- rbind(qbs_18, bake_early_agg, bake_late_agg)

# Plot scatter
qbs_18 %>%
    ggplot(aes(x = YPA, y = comp_percentage, label=name, color=primary)) +
    # Notice that color/size inside aes()
    geom_point(data = filter(qbs_18, !grepl("Weeks | Season", name))) +
    geom_point(data = filter(qbs_18, grepl("Weeks | Season", name))) +
    # we need this to assign colors and sizes
    scale_color_identity() +
    scale_size_identity() + 
    geom_text_repel(data = filter(qbs_18, grepl("Full Season", name)),
                    size=5,
                    fontface="bold",
                    nudge_x = 0.7,
                    nudge_y = -0.03,
                    segment.color = "black") +
    geom_text_repel(data = filter(qbs_18, grepl("Weeks 1-8", name)),
                    size=5,
                    fontface="bold",
                    nudge_x = 0.0,
                    nudge_y = -0.025,
                    segment.color = "black") +
    geom_text_repel(data = filter(qbs_18, grepl("Weeks 9-17", name)),
                    size=5,
                    fontface="bold",
                    nudge_x = -0.7,
                    nudge_y = 0.01,
                    segment.color = "black") +
    # add labels for set of other players
    geom_text_repel(data = subset(qbs_18, !grepl("Mayfield", name))) +
    # reference lines with mean for each variable
    geom_hline(aes(yintercept = mean_comp), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(aes(xintercept =  mean_YPA), color = "red", linetype = "dashed", alpha=0.5) +
    # plot labels
    labs(x = "Yards per Attempt",
         y = "Completion Percentage",
         caption = "data from nflscrapR",
         title = "2018: Tale of Two Halves",
         subtitle = "Baker vs The Field (350+ dropbacks)") +
    # Pff theme
    theme_pff +
    # cosmetic adjustments
    theme(panel.grid.major = element_blank())

ggsave("Images/QB_scatter_2018.png", dpi=2000, height=7, width=8)