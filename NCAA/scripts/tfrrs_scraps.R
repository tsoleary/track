# ------------------------------------------------------------------------------
# NCAA Improvement figures and scraps of code
# May 23, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load Libraries
library(tidyverse)

# Load data
df <- readRDS(here::here("NCAA/data/tfrrs_NCAA_2010_2021_final_data.rds"))

# Denote Track, Field, & Multi Events
df <- df %>%
  mutate(EVENT_TYPE = case_when(!is.na(TIME_S) ~ "TRACK",
                                !is.na(MARK_METERS) ~ "FIELD",
                                !is.na(POINTS) ~ "MULTI"))


x <- df %>%
  filter(EVENT_TYPE == "TRACK") %>%
  group_by(EVENT, SEX) %>%
  group_split()

for (i in 1:length(x)) {
  p <- x[[i]] %>%
    ggplot(aes(x = YEAR_QUAL, y = TIME_M)) +
    geom_violin(fill = "azure2", color = "grey40") +
    geom_boxplot(fill = "azure2", color = "grey40", width = 0.2) +
    labs(title = paste0(unique(x[[i]]$SEX), "'s ", unique(x[[i]]$EVENT))) +
    theme_classic()
  
  print(p)
}


df %>%
  filter(EVENT == "Long Jump" & SEX == "Men") %>%
  ggplot(aes(x = YEAR_QUAL, y = MARK_METERS)) +
  geom_violin(fill = "azure2", color = "grey40") +
  geom_boxplot(fill = "azure2", color = "grey40", width = 0.2) +
  theme_classic()

