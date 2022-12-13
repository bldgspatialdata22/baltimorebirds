
  
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
  read.csv(here("data", "birds", "eBird_US_Canada_2012-2022.csv"))

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

# A short macro to filter the bird data by year and prep it.
# remove = FALSE keeps the lat/long variables, which makes it
# easier to save the file as a csv and then bring it back in

bird_data = function(yr) {
  return(bird_all |>
           filter(Year == yr) |>
           add_count(STATE, name = "state_count") |>
           mutate(state_perc = percent_rank(state_count)) |>
           filter(state_perc > 0.0001) |>
           st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE))
}


# Making the final analytic data sets. I macrotized the initial prep 
# steps but don't know how to do that for the rest of what I need. I'm doing
# each year separately and describing the steps as I go. Detailed descriptions
# of each step in 2012 only.

# Before any of that, making the US/Canada map that will be used to
# crop each of the breeding season files in Step 2.

us_canada <-
  ne_countries(scale = "medium", returnclass = "sf") |>
  filter(type == "Country" | type == "Sovereign country") |>
  filter(sovereignt == "United States of America" | sovereignt == "Canada")


################
################
##### 2012 #####
################
################


# Step 1: Getting game log and bird sightings for single year using the 
# above macros. Then geocoding the game log and splitting it into separate 
# files by breeding season before making each an sf object.

orioles_log_2012 <- game_logs2(yr = 2012)

birds_2012_sf <- bird_data(yr = 2012)

orioles_log_2012 <-
  geocode(orioles_log_2012, 
          city = city,
          method = "osm")

orioles_breeding_2012 <-
  orioles_log_2012 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2012 <-
  orioles_log_2012 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2012 <-
  orioles_log_2012 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE )

# Step 2: Splitting the eBird file by breeding season, then cropping the 
# US/Canada map made above to basically convert the point sightings to
# a polygon that has borders aligning with the outermost sightings.

birds_2012_sf_breeding <-
  birds_2012_sf |>
  filter(season == "Breeding")

birds_2012_sf_pre_breed <-
  birds_2012_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2012_sf_post_breed <-
  birds_2012_sf |>
  filter(season == "Post-breeding Migratory")

breed_range_2012 <-
  st_crop(us_canada, birds_2012_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2012 <-
  st_crop(us_canada, birds_2012_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2012 <-
  st_crop(us_canada, birds_2012_sf_post_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2012)
mapview(pre_breed_range_2012)
mapview(post_breed_range_2012)

# Step 3: Spatial join of each breeding range polygon map to the game log
# with the city each game was played in geocoded.

orioles_breeding_2012 <-
  st_join(orioles_breeding_2012, breed_range_2012)

orioles_pre_breeding_2012 <-
  st_join(orioles_pre_breeding_2012, pre_breed_range_2012)

orioles_post_breeding_2012 <-
  st_join(orioles_post_breeding_2012, post_breed_range_2012)

# Step 4: Appending each of the sets made in Step 3 back into a single
# set, adding a flag to indicate whether each game was played in the breeding
# range corresponding to the breeding season it was played during, and finally
# dropping a bunch of variables.

orioles_log_final_2012 <-
  orioles_breeding_2012 |>
  bind_rows(orioles_pre_breeding_2012, orioles_post_breeding_2012) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2012_sf_breeding, birds_2012_sf_post_breed, birds_2012_sf_pre_breed,
       orioles_breeding_2012, orioles_post_breeding_2012, orioles_pre_breeding_2012,
       breed_range_2012, post_breed_range_2012, pre_breed_range_2012,
       orioles_log_2012)

# Saving as csv.

orioles_log_final_2012 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2012.csv"))

birds_2012_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2012.csv"))

################
################
##### 2013 #####
################
################


# Step 1.

orioles_log_2013 <- game_logs2(yr = 2013)

birds_2013_sf <- bird_data(yr = 2013)

orioles_log_2013 <-
  geocode(orioles_log_2013, 
          city = city,
          method = "osm")

orioles_breeding_2013 <-
  orioles_log_2013 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2013 <-
  orioles_log_2013 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2013 <-
  orioles_log_2013 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE )

# Step 2.

birds_2013_sf_breeding <-
  birds_2013_sf |>
  filter(season == "Breeding")

birds_2013_sf_pre_breed <-
  birds_2013_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2013_sf_post_breed <-
  birds_2013_sf |>
  filter(season == "Post-breeding Migratory")

breed_range_2013 <-
  st_crop(us_canada, birds_2013_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2013 <-
  st_crop(us_canada, birds_2013_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2013 <-
  st_crop(us_canada, birds_2013_sf_post_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2013)
mapview(pre_breed_range_2013)
mapview(post_breed_range_2013)

# Step 3.

orioles_breeding_2013 <-
  st_join(orioles_breeding_2013, breed_range_2013)

orioles_pre_breeding_2013 <-
  st_join(orioles_pre_breeding_2013, pre_breed_range_2013)

orioles_post_breeding_2013 <-
  st_join(orioles_post_breeding_2013, post_breed_range_2013)

# Step 4.

orioles_log_final_2013 <-
  orioles_breeding_2013 |>
  bind_rows(orioles_pre_breeding_2013, orioles_post_breeding_2013) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2013_sf_breeding, birds_2013_sf_post_breed, birds_2013_sf_pre_breed,
       orioles_breeding_2013, orioles_post_breeding_2013, orioles_pre_breeding_2013,
       breed_range_2013, post_breed_range_2013, pre_breed_range_2013,
       orioles_log_2013)

# Saving as csv.

orioles_log_final_2013 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2013.csv"))

birds_2013_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2013.csv"))

################
################
##### 2014 #####
################
################

# Note there was one game played in the non-breeding season in 2014 (Mar 31).
# I added things accordingly and kept them for subsequent years. There were
# no non-breeding season games in 2012 or 2013.

# Step 1.

orioles_log_2014 <- game_logs2(yr = 2014)

birds_2014_sf <- bird_data(yr = 2014)

orioles_log_2014 <-
  geocode(orioles_log_2014, 
          city = city,
          method = "osm")

orioles_breeding_2014 <-
  orioles_log_2014 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2014 <-
  orioles_log_2014 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2014 <-
  orioles_log_2014 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_non_breeding_2014 <-
  orioles_log_2014 |>
  filter(bird_season == "Non-breeding/Wintering")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Step 2.

birds_2014_sf_breeding <-
  birds_2014_sf |>
  filter(season == "Breeding")

birds_2014_sf_pre_breed <-
  birds_2014_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2014_sf_post_breed <-
  birds_2014_sf |>
  filter(season == "Post-breeding Migratory")

birds_2014_sf_non_breed <-
  birds_2014_sf |>
  filter(season == "Non-breeding/Wintering")

breed_range_2014 <-
  st_crop(us_canada, birds_2014_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2014 <-
  st_crop(us_canada, birds_2014_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2014 <-
  st_crop(us_canada, birds_2014_sf_post_breed)|>
  st_transform(crs = 4326)

non_breed_range_2014 <-
  st_crop(us_canada, birds_2014_sf_non_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2014)
mapview(pre_breed_range_2014)
mapview(post_breed_range_2014)
mapview(non_breed_range_2014)

# Step 3.

orioles_breeding_2014 <-
  st_join(orioles_breeding_2014, breed_range_2014)

orioles_pre_breeding_2014 <-
  st_join(orioles_pre_breeding_2014, pre_breed_range_2014)

orioles_post_breeding_2014 <-
  st_join(orioles_post_breeding_2014, post_breed_range_2014)

orioles_non_breeding_2014 <-
  st_join(orioles_non_breeding_2014, non_breed_range_2014)

# Step 4.

orioles_log_final_2014 <-
  orioles_breeding_2014 |>
  bind_rows(orioles_pre_breeding_2014, orioles_post_breeding_2014, orioles_non_breeding_2014) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2014_sf_breeding, birds_2014_sf_post_breed, birds_2014_sf_pre_breed, birds_2014_sf_non_breed,
       orioles_breeding_2014, orioles_post_breeding_2014, orioles_pre_breeding_2014, orioles_non_breeding_2014,
       breed_range_2014, post_breed_range_2014, pre_breed_range_2014, non_breed_range_2014,
       orioles_log_2014)

# Saving as csv.

orioles_log_final_2014 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2014.csv"))

birds_2014_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2014.csv"))

################
################
##### 2015 #####
################
################


# Step 1.

orioles_log_2015 <- game_logs2(yr = 2015)

birds_2015_sf <- bird_data(yr = 2015)

orioles_log_2015 <-
  geocode(orioles_log_2015, 
          city = city,
          method = "osm")

orioles_breeding_2015 <-
  orioles_log_2015 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2015 <-
  orioles_log_2015 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2015 <-
  orioles_log_2015 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_non_breeding_2015 <-
  orioles_log_2015 |>
  filter(bird_season == "Non-breeding/Wintering")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Step 2.

birds_2015_sf_breeding <-
  birds_2015_sf |>
  filter(season == "Breeding")

birds_2015_sf_pre_breed <-
  birds_2015_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2015_sf_post_breed <-
  birds_2015_sf |>
  filter(season == "Post-breeding Migratory")

birds_2015_sf_non_breed <-
  birds_2015_sf |>
  filter(season == "Non-breeding/Wintering")

breed_range_2015 <-
  st_crop(us_canada, birds_2015_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2015 <-
  st_crop(us_canada, birds_2015_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2015 <-
  st_crop(us_canada, birds_2015_sf_post_breed)|>
  st_transform(crs = 4326)

non_breed_range_2015 <-
  st_crop(us_canada, birds_2015_sf_non_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2015)
mapview(pre_breed_range_2015)
mapview(post_breed_range_2015)
mapview(non_breed_range_2015)

# Step 3.

orioles_breeding_2015 <-
  st_join(orioles_breeding_2015, breed_range_2015)

orioles_pre_breeding_2015 <-
  st_join(orioles_pre_breeding_2015, pre_breed_range_2015)

orioles_post_breeding_2015 <-
  st_join(orioles_post_breeding_2015, post_breed_range_2015)

orioles_non_breeding_2015 <-
  st_join(orioles_non_breeding_2015, non_breed_range_2015)

# Step 4.

orioles_log_final_2015 <-
  orioles_breeding_2015 |>
  bind_rows(orioles_pre_breeding_2015, orioles_post_breeding_2015, orioles_non_breeding_2015) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2015_sf_breeding, birds_2015_sf_post_breed, birds_2015_sf_pre_breed, birds_2015_sf_non_breed,
       orioles_breeding_2015, orioles_post_breeding_2015, orioles_pre_breeding_2015, orioles_non_breeding_2015,
       breed_range_2015, post_breed_range_2015, pre_breed_range_2015, non_breed_range_2015,
       orioles_log_2015)

# Saving as csv.

orioles_log_final_2015 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2015.csv"))

birds_2015_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2015.csv"))

################
################
##### 2016 #####
################
################


# Step 1.

orioles_log_2016 <- game_logs2(yr = 2016)

birds_2016_sf <- bird_data(yr = 2016)

orioles_log_2016 <-
  geocode(orioles_log_2016, 
          city = city,
          method = "osm")

orioles_breeding_2016 <-
  orioles_log_2016 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2016 <-
  orioles_log_2016 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2016 <-
  orioles_log_2016 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_non_breeding_2016 <-
  orioles_log_2016 |>
  filter(bird_season == "Non-breeding/Wintering")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Step 2.

birds_2016_sf_breeding <-
  birds_2016_sf |>
  filter(season == "Breeding")

birds_2016_sf_pre_breed <-
  birds_2016_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2016_sf_post_breed <-
  birds_2016_sf |>
  filter(season == "Post-breeding Migratory")

birds_2016_sf_non_breed <-
  birds_2016_sf |>
  filter(season == "Non-breeding/Wintering")

breed_range_2016 <-
  st_crop(us_canada, birds_2016_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2016 <-
  st_crop(us_canada, birds_2016_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2016 <-
  st_crop(us_canada, birds_2016_sf_post_breed)|>
  st_transform(crs = 4326)

non_breed_range_2016 <-
  st_crop(us_canada, birds_2016_sf_non_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2016)
mapview(pre_breed_range_2016)
mapview(post_breed_range_2016)
mapview(non_breed_range_2016)

# Step 3.

orioles_breeding_2016 <-
  st_join(orioles_breeding_2016, breed_range_2016)

orioles_pre_breeding_2016 <-
  st_join(orioles_pre_breeding_2016, pre_breed_range_2016)

orioles_post_breeding_2016 <-
  st_join(orioles_post_breeding_2016, post_breed_range_2016)

orioles_non_breeding_2016 <-
  st_join(orioles_non_breeding_2016, non_breed_range_2016)

# Step 4.

orioles_log_final_2016 <-
  orioles_breeding_2016 |>
  bind_rows(orioles_pre_breeding_2016, orioles_post_breeding_2016, orioles_non_breeding_2016) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2016_sf_breeding, birds_2016_sf_post_breed, birds_2016_sf_pre_breed, birds_2016_sf_non_breed,
       orioles_breeding_2016, orioles_post_breeding_2016, orioles_pre_breeding_2016, orioles_non_breeding_2016,
       breed_range_2016, post_breed_range_2016, pre_breed_range_2016, non_breed_range_2016,
       orioles_log_2016)

# Saving as csv.

orioles_log_final_2016 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2016.csv"))

birds_2016_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2016.csv"))

################
################
##### 2017 #####
################
################


# Step 1.

orioles_log_2017 <- game_logs2(yr = 2017)

birds_2017_sf <- bird_data(yr = 2017)

orioles_log_2017 <-
  geocode(orioles_log_2017, 
          city = city,
          method = "osm")

orioles_breeding_2017 <-
  orioles_log_2017 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2017 <-
  orioles_log_2017 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2017 <-
  orioles_log_2017 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_non_breeding_2017 <-
  orioles_log_2017 |>
  filter(bird_season == "Non-breeding/Wintering")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Step 2.

birds_2017_sf_breeding <-
  birds_2017_sf |>
  filter(season == "Breeding")

birds_2017_sf_pre_breed <-
  birds_2017_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2017_sf_post_breed <-
  birds_2017_sf |>
  filter(season == "Post-breeding Migratory")

birds_2017_sf_non_breed <-
  birds_2017_sf |>
  filter(season == "Non-breeding/Wintering")

breed_range_2017 <-
  st_crop(us_canada, birds_2017_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2017 <-
  st_crop(us_canada, birds_2017_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2017 <-
  st_crop(us_canada, birds_2017_sf_post_breed)|>
  st_transform(crs = 4326)

non_breed_range_2017 <-
  st_crop(us_canada, birds_2017_sf_non_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2017)
mapview(pre_breed_range_2017)
mapview(post_breed_range_2017)
mapview(non_breed_range_2017)

# Step 3.

orioles_breeding_2017 <-
  st_join(orioles_breeding_2017, breed_range_2017)

orioles_pre_breeding_2017 <-
  st_join(orioles_pre_breeding_2017, pre_breed_range_2017)

orioles_post_breeding_2017 <-
  st_join(orioles_post_breeding_2017, post_breed_range_2017)

orioles_non_breeding_2017 <-
  st_join(orioles_non_breeding_2017, non_breed_range_2017)

# Step 4.

orioles_log_final_2017 <-
  orioles_breeding_2017 |>
  bind_rows(orioles_pre_breeding_2017, orioles_post_breeding_2017, orioles_non_breeding_2017) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2017_sf_breeding, birds_2017_sf_post_breed, birds_2017_sf_pre_breed, birds_2017_sf_non_breed,
       orioles_breeding_2017, orioles_post_breeding_2017, orioles_pre_breeding_2017, orioles_non_breeding_2017,
       breed_range_2017, post_breed_range_2017, pre_breed_range_2017, non_breed_range_2017,
       orioles_log_2017)

# Saving as csv.

orioles_log_final_2017 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2017.csv"))

birds_2017_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2017.csv"))

################
################
##### 2018 #####
################
################


# Step 1.

orioles_log_2018 <- game_logs2(yr = 2018)

birds_2018_sf <- bird_data(yr = 2018)

orioles_log_2018 <-
  geocode(orioles_log_2018, 
          city = city,
          method = "osm")

orioles_breeding_2018 <-
  orioles_log_2018 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2018 <-
  orioles_log_2018 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2018 <-
  orioles_log_2018 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_non_breeding_2018 <-
  orioles_log_2018 |>
  filter(bird_season == "Non-breeding/Wintering")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Step 2.

birds_2018_sf_breeding <-
  birds_2018_sf |>
  filter(season == "Breeding")

birds_2018_sf_pre_breed <-
  birds_2018_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2018_sf_post_breed <-
  birds_2018_sf |>
  filter(season == "Post-breeding Migratory")

birds_2018_sf_non_breed <-
  birds_2018_sf |>
  filter(season == "Non-breeding/Wintering")

breed_range_2018 <-
  st_crop(us_canada, birds_2018_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2018 <-
  st_crop(us_canada, birds_2018_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2018 <-
  st_crop(us_canada, birds_2018_sf_post_breed)|>
  st_transform(crs = 4326)

non_breed_range_2018 <-
  st_crop(us_canada, birds_2018_sf_non_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2018)
mapview(pre_breed_range_2018)
mapview(post_breed_range_2018)
mapview(non_breed_range_2018)

# Step 3.

orioles_breeding_2018 <-
  st_join(orioles_breeding_2018, breed_range_2018)

orioles_pre_breeding_2018 <-
  st_join(orioles_pre_breeding_2018, pre_breed_range_2018)

orioles_post_breeding_2018 <-
  st_join(orioles_post_breeding_2018, post_breed_range_2018)

orioles_non_breeding_2018 <-
  st_join(orioles_non_breeding_2018, non_breed_range_2018)

# Step 4.

orioles_log_final_2018 <-
  orioles_breeding_2018 |>
  bind_rows(orioles_pre_breeding_2018, orioles_post_breeding_2018, orioles_non_breeding_2018) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2018_sf_breeding, birds_2018_sf_post_breed, birds_2018_sf_pre_breed, birds_2018_sf_non_breed,
       orioles_breeding_2018, orioles_post_breeding_2018, orioles_pre_breeding_2018, orioles_non_breeding_2018,
       breed_range_2018, post_breed_range_2018, pre_breed_range_2018, non_breed_range_2018,
       orioles_log_2018)

# Saving as csv.

orioles_log_final_2018 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2018.csv"))

birds_2018_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2018.csv"))

################
################
##### 2019 #####
################
################


# Step 1.

orioles_log_2019 <- game_logs2(yr = 2019)

birds_2019_sf <- bird_data(yr = 2019)

orioles_log_2019 <-
  geocode(orioles_log_2019, 
          city = city,
          method = "osm")

orioles_breeding_2019 <-
  orioles_log_2019 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2019 <-
  orioles_log_2019 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2019 <-
  orioles_log_2019 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_non_breeding_2019 <-
  orioles_log_2019 |>
  filter(bird_season == "Non-breeding/Wintering")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Step 2.

birds_2019_sf_breeding <-
  birds_2019_sf |>
  filter(season == "Breeding")

birds_2019_sf_pre_breed <-
  birds_2019_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2019_sf_post_breed <-
  birds_2019_sf |>
  filter(season == "Post-breeding Migratory")

birds_2019_sf_non_breed <-
  birds_2019_sf |>
  filter(season == "Non-breeding/Wintering")

breed_range_2019 <-
  st_crop(us_canada, birds_2019_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2019 <-
  st_crop(us_canada, birds_2019_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2019 <-
  st_crop(us_canada, birds_2019_sf_post_breed)|>
  st_transform(crs = 4326)

non_breed_range_2019 <-
  st_crop(us_canada, birds_2019_sf_non_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2019)
mapview(pre_breed_range_2019)
mapview(post_breed_range_2019)
mapview(non_breed_range_2019)

# Step 3.

orioles_breeding_2019 <-
  st_join(orioles_breeding_2019, breed_range_2019)

orioles_pre_breeding_2019 <-
  st_join(orioles_pre_breeding_2019, pre_breed_range_2019)

orioles_post_breeding_2019 <-
  st_join(orioles_post_breeding_2019, post_breed_range_2019)

orioles_non_breeding_2019 <-
  st_join(orioles_non_breeding_2019, non_breed_range_2019)

# Step 4.

orioles_log_final_2019 <-
  orioles_breeding_2019 |>
  bind_rows(orioles_pre_breeding_2019, orioles_post_breeding_2019, orioles_non_breeding_2019) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2019_sf_breeding, birds_2019_sf_post_breed, birds_2019_sf_pre_breed, birds_2019_sf_non_breed,
       orioles_breeding_2019, orioles_post_breeding_2019, orioles_pre_breeding_2019, orioles_non_breeding_2019,
       breed_range_2019, post_breed_range_2019, pre_breed_range_2019, non_breed_range_2019,
       orioles_log_2019)

# Saving as csv.

orioles_log_final_2019 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2019.csv"))

birds_2019_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2019.csv"))

################
################
##### 2020 #####
################
################


# Step 1.

orioles_log_2020 <- game_logs2(yr = 2020)

birds_2020_sf <- bird_data(yr = 2020)

orioles_log_2020 <-
  geocode(orioles_log_2020, 
          city = city,
          method = "osm")

orioles_breeding_2020 <-
  orioles_log_2020 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2020 <-
  orioles_log_2020 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2020 <-
  orioles_log_2020 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_non_breeding_2020 <-
  orioles_log_2020 |>
  filter(bird_season == "Non-breeding/Wintering")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Step 2.

birds_2020_sf_breeding <-
  birds_2020_sf |>
  filter(season == "Breeding")

birds_2020_sf_pre_breed <-
  birds_2020_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2020_sf_post_breed <-
  birds_2020_sf |>
  filter(season == "Post-breeding Migratory")

birds_2020_sf_non_breed <-
  birds_2020_sf |>
  filter(season == "Non-breeding/Wintering")

breed_range_2020 <-
  st_crop(us_canada, birds_2020_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2020 <-
  st_crop(us_canada, birds_2020_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2020 <-
  st_crop(us_canada, birds_2020_sf_post_breed)|>
  st_transform(crs = 4326)

non_breed_range_2020 <-
  st_crop(us_canada, birds_2020_sf_non_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2020)
mapview(pre_breed_range_2020)
mapview(post_breed_range_2020)
mapview(non_breed_range_2020)

# Step 3.

orioles_breeding_2020 <-
  st_join(orioles_breeding_2020, breed_range_2020)

orioles_pre_breeding_2020 <-
  st_join(orioles_pre_breeding_2020, pre_breed_range_2020)

orioles_post_breeding_2020 <-
  st_join(orioles_post_breeding_2020, post_breed_range_2020)

orioles_non_breeding_2020 <-
  st_join(orioles_non_breeding_2020, non_breed_range_2020)

# Step 4.

orioles_log_final_2020 <-
  orioles_breeding_2020 |>
  bind_rows(orioles_pre_breeding_2020, orioles_post_breeding_2020, orioles_non_breeding_2020) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2020_sf_breeding, birds_2020_sf_post_breed, birds_2020_sf_pre_breed, birds_2020_sf_non_breed,
       orioles_breeding_2020, orioles_post_breeding_2020, orioles_pre_breeding_2020, orioles_non_breeding_2020,
       breed_range_2020, post_breed_range_2020, pre_breed_range_2020, non_breed_range_2020,
       orioles_log_2020)

# Saving as csv.

orioles_log_final_2020 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2020.csv"))

birds_2020_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2020.csv"))


################
################
##### 2021 #####
################
################


# Step 1.

orioles_log_2021 <- game_logs2(yr = 2021)

birds_2021_sf <- bird_data(yr = 2021)

orioles_log_2021 <-
  geocode(orioles_log_2021, 
          city = city,
          method = "osm")

orioles_breeding_2021 <-
  orioles_log_2021 |>
  filter(bird_season == "Breeding")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_pre_breeding_2021 <-
  orioles_log_2021 |>
  filter(bird_season == "Pre-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_post_breeding_2021 <-
  orioles_log_2021 |>
  filter(bird_season == "Post-breeding Migratory")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

orioles_non_breeding_2021 <-
  orioles_log_2021 |>
  filter(bird_season == "Non-breeding/Wintering")|>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Step 2.

birds_2021_sf_breeding <-
  birds_2021_sf |>
  filter(season == "Breeding")

birds_2021_sf_pre_breed <-
  birds_2021_sf |>
  filter(season == "Pre-breeding Migratory")

birds_2021_sf_post_breed <-
  birds_2021_sf |>
  filter(season == "Post-breeding Migratory")

birds_2021_sf_non_breed <-
  birds_2021_sf |>
  filter(season == "Non-breeding/Wintering")

breed_range_2021 <-
  st_crop(us_canada, birds_2021_sf_breeding)|>
  st_transform(crs = 4326)

pre_breed_range_2021 <-
  st_crop(us_canada, birds_2021_sf_pre_breed)|>
  st_transform(crs = 4326)

post_breed_range_2021 <-
  st_crop(us_canada, birds_2021_sf_post_breed)|>
  st_transform(crs = 4326)

non_breed_range_2021 <-
  st_crop(us_canada, birds_2021_sf_non_breed)|>
  st_transform(crs = 4326)

# Quick check that they worked. Disappointingly little variation, but they look fine.

mapview(breed_range_2021)
mapview(pre_breed_range_2021)
mapview(post_breed_range_2021)
mapview(non_breed_range_2021)

# Step 3.

orioles_breeding_2021 <-
  st_join(orioles_breeding_2021, breed_range_2021)

orioles_pre_breeding_2021 <-
  st_join(orioles_pre_breeding_2021, pre_breed_range_2021)

orioles_post_breeding_2021 <-
  st_join(orioles_post_breeding_2021, post_breed_range_2021)

orioles_non_breeding_2021 <-
  st_join(orioles_non_breeding_2021, non_breed_range_2021)

# Step 4.

orioles_log_final_2021 <-
  orioles_breeding_2021 |>
  bind_rows(orioles_pre_breeding_2021, orioles_post_breeding_2021, orioles_non_breeding_2021) |>
  mutate(range_flag = case_when(
    !is.na(scalerank) ~ "In Range", # any var from range map will work
    TRUE ~ "Out of Range")) |>
  select(Date:long, range_flag)

# Removing all of the intermediate files to free up space.

remove(birds_2021_sf_breeding, birds_2021_sf_post_breed, birds_2021_sf_pre_breed, birds_2021_sf_non_breed,
       orioles_breeding_2021, orioles_post_breeding_2021, orioles_pre_breeding_2021, orioles_non_breeding_2021,
       breed_range_2021, post_breed_range_2021, pre_breed_range_2021, non_breed_range_2021,
       orioles_log_2021)


# Saving as csv.

orioles_log_final_2021 |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_2021.csv"))

birds_2021_sf |>
  st_drop_geometry() |>
  write.csv(here("data", "birds", "birds_2021.csv"))


# Now I'm combining all of the game log years into a single file so I can 
# put overall summaries on the main Results page.

orioles_log_final_all_years <-
  bind_rows(orioles_log_final_2012,
            orioles_log_final_2013,
            orioles_log_final_2014,
            orioles_log_final_2015,
            orioles_log_final_2016,
            orioles_log_final_2017,
            orioles_log_final_2018,
            orioles_log_final_2019,
            orioles_log_final_2020,
            orioles_log_final_2021)

# Saving as csv.

orioles_log_final_all_years |>
  st_drop_geometry() |>
  write.csv(here("data", "baseball", "orioles_log_final_all_years.csv"))



















project:
  type: website

website:
  title: "The Baltimore Orioles and Baltimore orioles"
navbar:
  left:
  - href: index.qmd
text: Home
- overview.qmd
- data_methods.qmd
- section: results/results_main.qmd
- references.qmd

format:
  html:
  theme: cosmo
css: styles.css
toc: true

editor: visual







