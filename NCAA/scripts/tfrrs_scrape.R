# ------------------------------------------------------------------------------
# Scraping TFRRS data
# May 17, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load Libraries
library(rvest)
library(tidyverse)

# Define pages to scrape
tfrrs_page_n <- c("2010" = "528", 
                  "2011" = "673", 
                  "2012" = "840", 
                  "2013" = "1029", 
                  "2014" = "1228", 
                  "2015" = "1439", 
                  "2016" = "1688", 
                  "2017" = "1912",
                  "2018" = "2279", 
                  "2019" = "2568", 
                  "2021" = "3191")

# Define common beginning and end of URL
url1 <- "https://www.tfrrs.org/lists/"
url2 <- ".html?limit=%3C%3D500&event_type=all&year=&gender=x"

# Create an empty list to populate with data
df <- vector(mode = "list", length = length(tfrrs_page_n))

# Loop through all years -------------------------------------------------------

for (i in 1:length(tfrrs_page_n)) {
  
  # Set up url and read in html -----
  
    # Paste together with variable number
    url <- paste0(url1, tfrrs_page_n[i], url2)
    
    # Read the html
    page <- read_html(url)
  
  # Get data -----
  
    # Get list of all tables on that page
    list_dfs <- page %>%
      html_nodes("table") %>%
      html_table()
    
    # Name of all events 
    events <- page %>%
      html_elements("h3") %>%
      html_text2()
    
    # Don't save the title of the page
    events <- events[2:length(events)]
    
    # Name the events in the list
    names(list_dfs) <- events
  
  # Save data ----
  
    # Save the list of all events 
    df[[i]] <- list_dfs
    
    # Save year in name of list
    names(df)[i] <- names(tfrrs_page_n)[i]
    
    # Sleep for 10 seconds
    print(i)
    Sys.sleep(10)
}


# Save list of data as a .rds file
saveRDS(df, here::here("NCAA/data/tfrrs_NCAA_2010_2021_data.rds"))

# Quickly clean into a single df -----------------------------------------------
df <- readRDS(here::here("NCAA/data/tfrrs_NCAA_2010_2021_data.rds"))

# Loop through each year to bind the dfs
for (i in 1:length(df)) {
  
  for(j in 1:length(df[[i]])){
    
    df[[i]][[j]] <- df[[i]][[j]] %>%
      # Create RANK col for first column
      rename(RANK = "") %>%
      # Save all as numeric so that bind_rows will have compatible col_types
      mutate(across(where(is.numeric), as.character))
  }
  
  # Save Event col
  df[[i]] <- bind_rows(df[[i]], .id = "Event")
}

# Save Year
df <- bind_rows(df, .id = "Year")

# Save object as .rds
saveRDS(df, here::here("NCAA/data/tfrrs_NCAA_2010_2021_df.rds"))