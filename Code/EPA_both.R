library(dplyr)
library(ggplot2)
library(nflscrapR)
library(tidyverse)
library(ggimage)
library(ggrepel)

# Theme for plots
source("Code/theme_pff.R")

# Import functions that load/process data
source("Code/Baker_functions.R")

# Load in passing data for 2018/2019 and combine
pass_18 <- load.transform.trim(season=2018)
pass_19 <- load.transform.trim(season=2019)
pass_both <- rbind(pass_18, pass_19)

# Filter out Baker
bake_both <- filter(pass_both, name=="B.Mayfield")

# Convert to factors
bake_both$season = factor(bake_both$season)
bake_both$week = factor(bake_both$week)

# League mean epa per dropback
mean_epa_both <- mean(pass_both$epa)

# Aggregate EPA by week
bake_both_agg <- bake_both %>%
    group_by(season, week, defteam) %>%
    summarize(dropbacks = sum(pass),
              epa = sum(epa),
              epa_per_db = epa/dropbacks)

nfl_logos_df <- read_csv("https://raw.githubusercontent.com/ClayGirdner/Baker/master/nfl_team_logos.csv")
bake_both_agg <- left_join(bake_both_agg,
                         nfl_logos_df, by = c("defteam" = "team_code"))

# Plot bar chart
bake_both_agg %>%
    ggplot(aes(x = week, y = epa_per_db)) +
    geom_hline(yintercept = mean_epa_both, color = "black", linetype = "dashed", alpha = 0.5) +
    geom_text(data = filter(bake_both_agg, season == 2018),
              label = "NFL Average", x = 6.0, y = 0.6, size = 3) +
    geom_segment(data = filter(bake_both_agg, season == 2018),
                 x = 6.0, y = 0.55, xend = 6.0, yend = mean_epa_both + 0.03,
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_col(aes(fill = if_else(epa_per_db > 0 , "green", "red"),
                 color = "black")) +
    geom_image(data = filter(bake_both_agg, epa_per_db > 0),
               aes(image=url, y = epa_per_db + 0.075),
               size = 0.05) +
    geom_image(data = filter(bake_both_agg, epa_per_db < 0),
               aes(image=url, y = epa_per_db - 0.075),
               size = 0.05) +
    scale_fill_identity() + 
    scale_color_identity() +
    facet_grid(. ~ season, switch="x") +
    labs(title = "Mr. Mayfield's Wild Ride",
         subtitle = "EPA per dropback",
         caption = "data from nflscrapR",
         x = "Week",
         y = NULL) + 
    theme_pff +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave("Images/EPA_both.png", dpi=2000, height=6, width=9)
