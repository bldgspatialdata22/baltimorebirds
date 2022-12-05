
  
# This document contains all of the data work to create the files that will be used to make 
# the tables for the site. This document will not be included on the site but will be in the repo. 
# Almost all of this code has been adapted or copied from the baltimorebirds_exploratory document.


library(tidyverse)
library(lubridate)
library(here)
library(dplyr)
library(ggplot2)
library(tidygeocoder)
library(mapview)
library(sf)
library(rnaturalearth)
library(xlsx)
library(retrosheet)
library(gt)


# Bringing in the eBird data. This is a reduced file that has already been cleaned and only 
# contains years 2012-2022. I don't need 2022 so removing that as well.


bird_all <-
  read.csv(here("data", "eBird_US_Canada_2012-2022.csv"))

bird_all <-
  bird_all |>
  filter(Year != 2022)



# Making a giant function/macro to bring in the Orioles game logs and clean them up. 
# There are certainly better ways to do this but I'm so fucking proud of this behemoth.



game_logs2 = function(yr) {
  return(getRetrosheet("game", yr) |>
           filter(HmTm == "BAL" | VisTm == "BAL") |>
           mutate(opponent = case_when(
             HmTm =="BAL" ~ VisTm,
             VisTm == "BAL" ~ HmTm)) |>
           mutate(home_away = case_when(
             HmTm == "BAL" ~ "Home",
             VisTm == "BAL" ~ "Away")) |>
           # Stats for Baltimore  
           mutate(bal_runs = case_when(
             HmTm == "BAL" ~ HmRuns,
             VisTm == "BAL" ~ VisRuns)) |>
           mutate(bal_AB = case_when(
             HmTm == "BAL" ~ HmAB,
             VisTm == "BAL" ~ VisAB)) |>
           mutate(bal_H = case_when(
             HmTm == "BAL" ~ HmH,
             VisTm == "BAL" ~ VisH)) |>
           mutate(bal_doubles = case_when(
             HmTm == "BAL" ~ HmD,
             VisTm == "BAL" ~ VisD)) |>
           mutate(bal_triples = case_when(
             HmTm == "BAL" ~ HmT,
             VisTm == "BAL" ~ VisT)) |>
           mutate(bal_HR = case_when(
             HmTm == "BAL" ~ HmHR,
             VisTm == "BAL" ~ VisHR)) |>
           mutate(bal_RBI = case_when(
             HmTm == "BAL" ~ HmRBI,
             VisTm == "BAL" ~ VisRBI)) |>
           mutate(bal_sac_hits = case_when(
             HmTm == "BAL" ~ HmSH,
             VisTm == "BAL" ~ VisSH)) |>
           mutate(bal_sac_fly = case_when(
             HmTm == "BAL" ~ HmSF,
             VisTm == "BAL" ~ VisSF)) |>
           mutate(bal_HBP = case_when(
             HmTm == "BAL" ~ HmHBP,
             VisTm == "BAL" ~ VisHBP)) |>
           mutate(bal_BB = case_when(
             HmTm == "BAL" ~ HmBB,
             VisTm == "BAL" ~ VisBB)) |>
           mutate(bal_IBB = case_when(
             HmTm == "BAL" ~ HmIBB,
             VisTm == "BAL" ~ VisIBB)) |>
           mutate(bal_hitters_K = case_when(
             HmTm == "BAL" ~ HmK,
             VisTm == "BAL" ~ VisK)) |>
           mutate(bal_SB = case_when(
             HmTm == "BAL" ~ HmSB,
             VisTm == "BAL" ~ VisSB)) |>
           mutate(bal_CS = case_when(
             HmTm == "BAL" ~ HmCS,
             VisTm == "BAL" ~ VisCS)) |>
           mutate(bal_GDP = case_when(
             HmTm == "BAL" ~ HmGDP,
             VisTm == "BAL" ~ VisGDP)) |>
           mutate(bal_LOB = case_when(
             HmTm == "BAL" ~ HmLOB,
             VisTm == "BAL" ~ VisLOB)) |>
           mutate(bal_num_pitchers = case_when(
             HmTm == "BAL" ~ HmPs,
             VisTm == "BAL" ~ VisPs)) |>
           mutate(bal_team_ER = case_when(
             HmTm == "BAL" ~ HmTER,
             VisTm == "BAL" ~ VisTER)) |>
           mutate(bal_WP = case_when(
             HmTm == "BAL" ~ HmWP,
             VisTm == "BAL" ~ VisWP)) |>
           mutate(bal_balks = case_when(
             HmTm == "BAL" ~ HmBalks,
             VisTm == "BAL" ~ VisBalks)) |>
           mutate(bal_assists = case_when(
             HmTm == "BAL" ~ HmA,
             VisTm == "BAL" ~ VisA)) |>
           mutate(bal_errors = case_when(
             HmTm == "BAL" ~ HmE,
             VisTm == "BAL" ~ VisE)) |>
           mutate(bal_pass_balls = case_when(
             HmTm == "BAL" ~ HmPass,
             VisTm == "BAL" ~ VisPassed)) |>
           mutate(bal_double_play = case_when(
             HmTm == "BAL" ~ HmDB,
             VisTm == "BAL" ~ VisDB)) |>
           mutate(bal_triple_play = case_when(
             HmTm == "BAL" ~ HmTP,
             VisTm == "BAL" ~ VisTP)) |>
           # Stats for opponent
           mutate(opp_runs = case_when(
             HmTm != "BAL" ~ HmRuns,
             VisTm != "BAL" ~ VisRuns)) |>
           mutate(opp_AB = case_when(
             HmTm != "BAL" ~ HmAB,
             VisTm != "BAL" ~ VisAB)) |>
           mutate(opp_H = case_when(
             HmTm != "BAL" ~ HmH,
             VisTm != "BAL" ~ VisH)) |>
           mutate(opp_doubles = case_when(
             HmTm != "BAL" ~ HmD,
             VisTm != "BAL" ~ VisD)) |>
           mutate(opp_triples = case_when(
             HmTm != "BAL" ~ HmT,
             VisTm != "BAL" ~ VisT)) |>
           mutate(opp_HR = case_when(
             HmTm != "BAL" ~ HmHR,
             VisTm != "BAL" ~ VisHR)) |>
           mutate(opp_RBI = case_when(
             HmTm != "BAL" ~ HmRBI,
             VisTm != "BAL" ~ VisRBI)) |>
           mutate(opp_sac_hits = case_when(
             HmTm != "BAL" ~ HmSH,
             VisTm != "BAL" ~ VisSH)) |>
           mutate(opp_sac_fly = case_when(
             HmTm != "BAL" ~ HmSF,
             VisTm != "BAL" ~ VisSF)) |>
           mutate(opp_HBP = case_when(
             HmTm != "BAL" ~ HmHBP,
             VisTm != "BAL" ~ VisHBP)) |>
           mutate(opp_BB = case_when(
             HmTm != "BAL" ~ HmBB,
             VisTm != "BAL" ~ VisBB)) |>
           mutate(opp_IBB = case_when(
             HmTm != "BAL" ~ HmIBB,
             VisTm != "BAL" ~ VisIBB)) |>
           mutate(opp_hitters_K = case_when(
             HmTm != "BAL" ~ HmK,
             VisTm != "BAL" ~ VisK)) |>
           mutate(opp_SB = case_when(
             HmTm != "BAL" ~ HmSB,
             VisTm != "BAL" ~ VisSB)) |>
           mutate(opp_CS = case_when(
             HmTm != "BAL" ~ HmCS,
             VisTm != "BAL" ~ VisCS)) |>
           mutate(opp_GDP = case_when(
             HmTm != "BAL" ~ HmGDP,
             VisTm != "BAL" ~ VisGDP)) |>
           mutate(opp_LOB = case_when(
             HmTm != "BAL" ~ HmLOB,
             VisTm != "BAL" ~ VisLOB)) |>
           mutate(opp_num_pitchers = case_when(
             HmTm != "BAL" ~ HmPs,
             VisTm != "BAL" ~ VisPs)) |>
           mutate(opp_team_ER = case_when(
             HmTm != "BAL" ~ HmTER,
             VisTm != "BAL" ~ VisTER)) |>
           mutate(opp_WP = case_when(
             HmTm != "BAL" ~ HmWP,
             VisTm != "BAL" ~ VisWP)) |>
           mutate(opp_balks = case_when(
             HmTm != "BAL" ~ HmBalks,
             VisTm != "BAL" ~ VisBalks)) |>
           mutate(opp_assists = case_when(
             HmTm != "BAL" ~ HmA,
             VisTm != "BAL" ~ VisA)) |>
           mutate(opp_errors = case_when(
             HmTm != "BAL" ~ HmE,
             VisTm != "BAL" ~ VisE)) |>
           mutate(opp_pass_balls = case_when(
             HmTm != "BAL" ~ HmPass,
             VisTm != "BAL" ~ VisPassed)) |>
           mutate(opp_douple_play = case_when(
             HmTm != "BAL" ~ HmDB,
             VisTm != "BAL" ~ VisDB)) |>
           mutate(opp_triple_play = case_when(
             HmTm != "BAL" ~ HmTP,
             VisTm != "BAL" ~ VisTP)) |>
           
           mutate(game_result = case_when(
             bal_runs > opp_runs ~ "Win",
             bal_runs < opp_runs ~ "Loss",
             TRUE ~ "Tie")) |>
           
           mutate(Year = substr(Date, 1, 4),
                  Month = substr(Date, 5, 6),
                  Day_num = substr(Date, 7, 8))|>
           
           mutate(bal_BA = bal_H / bal_AB,
                  opp_BA = opp_H / opp_AB) |>
           mutate(across(c(bal_BA, opp_BA), round, 3)) |>
           # Keep only new vars and a few old vars.
           select(Date, Year, Month, Day_num, Day, DayNight, ParkID, game_result,
                  opponent:bal_runs,
                  bal_BA,
                  bal_AB:opp_runs,
                  opp_BA,
                  opp_AB:opp_triple_play) |>
           
           # Adding the city variable.
           
           mutate(city = case_when(
             home_away == "Home" ~ "Baltimore",
             
             home_away == "Away" & opponent == "CHA" ~ "Chicago",
             home_away == "Away" & opponent == "CLE" ~ "Cleveland",
             home_away == "Away" & opponent == "DET" ~ "Detroit",
             home_away == "Away" & opponent == "KCA" ~ "Kansas City",
             home_away == "Away" & opponent == "MIN" ~ "Minneapolis",
             
             home_away == "Away" & opponent == "BOS" ~ "Boston",
             home_away == "Away" & opponent == "NYA" ~ "Buffalo",
             home_away == "Away" & opponent == "TBA" ~ "Tampa",
             home_away == "Away" & opponent == "TOR" ~ "Toronto",
             
             home_away == "Away" & opponent == "HOU" ~ "Houston",
             home_away == "Away" & opponent == "ANA" ~ "Anaheim",
             home_away == "Away" & opponent == "OAK" ~ "Oakland",
             home_away == "Away" & opponent == "SEA" ~ "Seattle",
             home_away == "Away" & opponent == "TEX" ~ "Dallas",
             
             home_away == "Away" & opponent == "CHN" ~ "Chicago",
             home_away == "Away" & opponent == "CIN" ~ "Cincinnati",
             home_away == "Away" & opponent == "MIL" ~ "Milwaukee",
             home_away == "Away" & opponent == "PIT" ~ "Pittsburgh",
             home_away == "Away" & opponent == "SLN" ~ "St. Louis",
             
             home_away == "Away" & opponent == "ATL" ~ "Atlanta",
             home_away == "Away" & opponent == "MIA" ~ "Miami",
             home_away == "Away" & opponent == "NYN" ~ "Buffalo",
             home_away == "Away" & opponent == "PHI" ~ "Philadelphia",
             home_away == "Away" & opponent == "WAS" ~ "Washington, DC",
             
             home_away == "Away" & opponent == "ARI" ~ "Phoenix",
             home_away == "Away" & opponent == "COL" ~ "Denver",
             home_away == "Away" & opponent == "LAN" ~ "Los Angeles",
             home_away == "Away" & opponent == "SDN" ~ "San Diego",
             home_away == "Away" & opponent == "SFN" ~ "San Francisco")) |>
           
           # Adding the bird season.
           
           mutate(bird_season = case_when(
             (Month == "06" | Month == "07") ~ "Breeding",
             (Month == "04" | Month == "05") ~ "Pre-breeding Migratory",
             (Month == "08" | Month == "09" | Month == "10" | Month == "11") ~ "Post-breeding Migratory",
             TRUE ~ "Non-breeding/Wintering"))
         
         
  ) # This parentheses MUST be at the end!!!
}




# Now using the macro to get game logs for each year 2012-2021.


orioles_log_2012 <-
  game_logs2(yr = 2012)

orioles_log_2013 <-
  game_logs2(yr = 2013)

orioles_log_2014 <-
  game_logs2(yr = 2014)

orioles_log_2015 <-
  game_logs2(yr = 2015)

orioles_log_2016 <-
  game_logs2(yr = 2016)

orioles_log_2017 <-
  game_logs2(yr = 2017)

orioles_log_2018 <-
  game_logs2(yr = 2018)

orioles_log_2019 <-
  game_logs2(yr = 2019)

orioles_log_2020 <-
  game_logs2(yr = 2020)

orioles_log_2021 <-
  game_logs2(yr = 2021)



# Appending the game logs together to make the combined summary table on the main results page.

orioles_log_all <-
  bind_rows(orioles_log_2012,
            orioles_log_2013,
            orioles_log_2014,
            orioles_log_2015,
            orioles_log_2016,
            orioles_log_2017,
            orioles_log_2018,
            orioles_log_2019,
            orioles_log_2020,
            orioles_log_2021)
