# ------------------------------------------------------------------------------
# TFRRS cleaning data
# May 23, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load Libraries
library(tidyverse)

# Load data
df <- readRDS(here::here("NCAA/data/tfrrs_NCAA_2010_2021_df.rds"))

# Cleean up times
df <- df %>% 
  mutate(Time = gsub("[^0-9:0-9\\.]", "", TIME)) %>%
  separate(Time,
           into = c("mins", "secs"), 
           fill = "left", 
           convert = TRUE,
           sep = ":") %>%
  mutate(mins = replace_na(mins, 0)) %>%
  mutate(time_secs = mins*60 + secs,
         time_mins = mins + secs/60)

# Clean up marks and points
df <- df %>%
  mutate(Mark = as.numeric(gsub("[^0-9\\.]", "", MARK)),
         POINTS = as.numeric(POINTS))

# Merge athlete columns
df <- df %>%
  mutate(ATHLETE = coalesce(ATHLETE, ATHLETES))

# Split event and sex
df <- df %>%
  separate(Event, into = c("Event", "Sex"), sep = "\\s\\(") %>%
  mutate(Sex = str_remove_all(Sex, "\\)")) %>%
  mutate(Event = str_replace_all(Event, "Meters", "m"))

# Organize cols and colnames
df <- df %>%
  select(RANK, ATHLETE, YEAR, TEAM, Event, Sex, TIME, 
         time_secs, time_mins, MARK, Mark, CONV, POINTS, MEET, `MEET DATE`, Year) %>%
  rename(EVENT = Event,
         SEX = Sex,
         TIME_S = time_secs,
         TIME_M = time_mins,
         MARK_METERS = Mark,
         MEET_DATE = `MEET DATE`,
         YEAR_QUAL = Year)

# Save data as .tsv
write_tsv(df, here::here("NCAA/data/tfrrs_2010_2021_data.tsv"))


# Load data
df <- read_tsv(here::here("NCAA/data/tfrrs_2010_2021_data.tsv"),
               col_types = cols(MARK = "c", 
                                MARK_METERS = "n", 
                                CONV = "c", 
                                POINTS = "n", 
                                YEAR_QUAL = "f"))

# Save as .rds
saveRDS(df, here::here("NCAA/data/tfrrs_NCAA_2010_2021_final_data.rds"))

# Load data
df <- readRDS(here::here("NCAA/data/tfrrs_NCAA_2010_2021_final_data.rds"))
  
df <- df %>%
  mutate(EVENT = ifelse(EVENT == "3000 m", 
                        "3000 Steeplechase", 
                        EVENT)) %>%
  mutate(EVENT = ifelse(EVENT == "3000 Steeplechase", 
                        "3,000 m Steeplechase", 
                        EVENT)) %>%
  mutate(EVENT = ifelse(EVENT == "110 Hurdles", 
                        "110 m Hurdles", 
                        EVENT)) %>%
  mutate(EVENT = ifelse(EVENT == "100 Hurdles", 
                        "100 m Hurdles", 
                        EVENT)) %>%
  mutate(EVENT = ifelse(EVENT == "400 Hurdles", 
                        "400 m Hurdles", 
                        EVENT)) %>%
  mutate(EVENT = ifelse(EVENT == "4 x 100 Relay", 
                        "4 x 100 m Relay", 
                        EVENT)) %>%
  mutate(EVENT = ifelse(EVENT == "4 x 400 Relay", 
                        "4 x 400 m Relay", 
                        EVENT))  %>%
  mutate(EVENT = ifelse(EVENT == "5000 m", 
                        "5,000 m", 
                        EVENT)) %>%
  mutate(EVENT = str_replace_all(EVENT, "\\sm", "m"))

# Save as .rds
saveRDS(df, here::here("NCAA/data/tfrrs_NCAA_2010_2021_final_data.rds"))

# Clean team names -------------------------------------------------------------

# Load data
df <- readRDS(url("https://tsoleary.github.io/track/NCAA/data/tfrrs_NCAA_2010_2021_final_data.rds")) %>%
  mutate(EVENT_TYPE = case_when(!is.na(TIME_S) ~ "TRACK",
                                !is.na(MARK_METERS) ~ "FIELD",
                                !is.na(POINTS) ~ "MULTI"))

# Define list of unique NCAA team names for ncaahoopsR
ncaa_colors <- ncaahoopR::ncaa_colors %>%
  add_row(ncaa_name = "non")

# With the help of fuzzy matching 
# Load library
require("fuzzyjoin")
z <- stringdist_join(ncaa_colors,
                     track,
                     by = "ncaa_name",
                     ignore_case = FALSE, 
                     method = "jw", 
                     max_dist = 99, 
                     distance_col = "dist") %>%
  group_by(ncaa_name.y) %>%
  slice_min(order_by = dist, 
            n = 1) %>%
  rename(ncaa_name = ncaa_name.x,
         TRACK_clean = ncaa_name.y) %>%
  select(TRACK_clean,
         ncaa_name,
         espn_name, 
         dist, everything()) %>%
  arrange(TRACK_clean)


# Clean relay team names
df <- df %>%
  mutate(TEAM_clean = str_replace(TEAM, " \\([[:alpha:]]\\)", ""))

# With help of ncaahoopsR packages 
df_colors <- read_csv("NCAA/data/ncaa_colors.csv")

# Join the two data frames 
df <- df %>% 
  full_join(
    df_colors %>%
      select(TRACK_clean, ncaa_name),
  by = c("TEAM_clean" = "TRACK_clean"))
  
# Take out redundant variable 
df <- df %>%
  select(-TEAM_clean)

# Save as new rds
saveRDS(df, here::here("NCAA/data/tfrrs_NCAA_2010_2021_final_data_team.rds"))
