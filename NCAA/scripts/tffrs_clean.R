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
