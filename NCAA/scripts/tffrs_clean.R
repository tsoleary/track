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



# '^-?[0-9.]+$'

y <- df %>% 
  filter(!str_detect(TIME, "@"))
