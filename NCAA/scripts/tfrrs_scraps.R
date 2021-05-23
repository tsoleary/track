# ------------------------------------------------------------------------------
# NCAA Improvement figures and scraps of code
# May 23, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load Libraries
library(tidyverse)

# Load data
df <- readRDS(here::here("NCAA/data/tfrrs_NCAA_2010_2021_df.rds"))

# Fastest in each event each year 
df_tops <- df %>%
  group_by(Event, Year) %>%
  slice_max(1)


df %>%
  filter(Event == "200 Meters (Men)") %>%
  mutate(time = as.numeric(str_remove_all(TIME, "@"))) %>%
  ggplot() +
  geom_histogram(aes(x = time), 
                 bins = 30,
                 color = "grey50", 
                 fill = "grey70") +
  labs(y = "Count",
       x = "Time") +
  theme_classic() + 
  facet_wrap( ~ Year)


df %>%
  filter(Event == "100 Meters (Men)") %>%
  mutate(time = as.numeric(str_remove_all(TIME, "@"))) %>%
  ggplot(aes(y = time, x = Year)) +
  geom_violin(fill = "azure2") +
  geom_boxplot(color = "grey50", fill = "azure", width = 0.2) +
  #geom_hline(yintercept = 19.5, color = "grey10", linetype = "dashed") + 
  #geom_smooth(aes(y = time, x = as.numeric(Year)), method = lm) +
  labs(y = "Time",
       x = "Year",
       title = "100 Meters (Men)") +
  theme_classic()

y <- df %>%
  filter(Event == "1500 Meters (Men)") %>%
  mutate(time = str_remove_all(TIME, "@")) %>%
  separate(time, into = c("mins", "secs"), sep = ":") %>%
  mutate(mins = as.numeric(mins),
         secs = as.numeric(secs)) %>%
  mutate(time_s = ((mins * 60) + secs)) %>%
  ggplot(aes(y = time_s, x = Year)) +
  geom_violin(fill = "azure2") +
  geom_boxplot(color = "grey50", fill = "azure", width = 0.2) +
  # geom_hline(yintercept = 19.5, color = "grey10", linetype = "dashed") +
  # geom_smooth(aes(y = time, x = as.numeric(Year)), method = lm) +
  labs(y = "Time",
       x = "Year") +
  theme_classic()