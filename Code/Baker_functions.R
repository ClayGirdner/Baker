library(dplyr)
library(nflscrapR)
library(tidyverse)

# Load in data, apply Ben Baldwin transformation, trim to passes/non spikes
load.transform.trim <- function(season){
    
    # Read in pbp and game data and join them on game_id
    pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", season,".csv")))
    games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", season,".csv")))
    pbp <- left_join(pbp, games[,c("game_id", "type", "season", "week")],
                     by="game_id")
    
    # Ben Baldwin transformation
    rp <- pbp %>%
        # grab only penalties, pass, and run plays
        filter(!is.na(epa), play_type == "no_play" | play_type == "pass" | play_type == "run") %>%
        # create pass, rush and success columns
        # use information from desc column to determine if penalty plays were run or pass
        mutate(
            pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
            rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
            success = ifelse(epa > 0, 1, 0)
        ) %>%
        # filter to only pass or rush plays
        filter(pass == 1 | rush == 1) %>%
        # Again, use desc to pull out player names on penalty plays
        mutate(
            passer_player_name = ifelse(play_type == "no_play" & pass == 1,
                                        str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                        passer_player_name
            ),
            receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"),
                                          str_extract(
                                              desc,
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"
                                          ),
                                          receiver_player_name
            ),
            rusher_player_name = ifelse(play_type == "no_play" & rush == 1,
                                        str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|      (up the middle)|(right guard)|(right tackle)|(right end)))"),
                                        rusher_player_name
            )
        ) %>%
        mutate(
            name = if_else(!is.na(passer_player_name), passer_player_name, rusher_player_name),
            rusher = rusher_player_name,
            receiver = receiver_player_name,
            play = 1
        )
    
    
    # trim to passing data
    pass_data <- filter(rp, pass==1 & qb_spike==0)
    
    return(pass_data)
}

# Function to aggregate a given set of QB data
aggup <- function(data) {
    grouped <- group_by(data, name, posteam)
    summed <- summarize(grouped,
                        dropbacks = sum(pass),
                        attempts = sum(pass_attempt),
                        games = n_distinct(game_id),
                        YPA = sum(yards_gained)/attempts,
                        comp_percentage = sum(complete_pass)/attempts,
                        YPG = sum(yards_gained)/games,
                        AYPA = sum(air_yards, na.rm = TRUE)/attempts,
                        epa_per_dropback = sum(epa)/dropbacks,
                        TD_rate = sum(pass_touchdown)/dropbacks,
                        INT_rate = sum(interception)/dropbacks,
                        sack_rate = sum(sack)/dropbacks)
    # Join team colors
    summed <- left_join(summed, nflteams, by = c("posteam" = "abbr"))
    
    return(summed)
}
