# For now, this is a script I run manually to update the weekly data.
# I may add Scheduler to the Heroku app and have it run once a week.
# It also requires a Census API key, so that has to be setup on Heroku
# before this will run there.

# Update from their website: "As of March 7, 2021 we are no longer collecting new data."

library(tidyverse)
library(lubridate)
library(tidycensus)
library(rvest)

get_data <- function() {
  
  weekly_stats_csv <- "weekly_stats.csv"
  states_raw <- read_csv(url("https://covidtracking.com/api/v1/states/daily.csv"))
  states <- states_raw %>%
    filter(!is.na(lastUpdateEt)) %>%
    mutate(
      date = ymd(date),
      region = state.region[match(state, state.abb)]
    ) %>%
    arrange(state, date)
  
  # Grab census population data by state and add to state table
  api_key <- Sys.getenv("CENSUS_API_KEY")
  acs <- get_acs(
    "state",
    variables = "B01003_001E",
    year = 2018
  )
  states <- states %>%
    left_join(
      acs %>% select(GEOID, population = estimate),
      by = c("fips" = "GEOID")
    ) %>%
    filter(!is.na(population))
  
  # Add governor data to the state table
  state_abb <- setNames(state.abb, state.name)
  wikiurl <- "https://en.wikipedia.org/wiki/List_of_current_United_States_governors_by_age"
  data <- wikiurl %>%
    xml2::read_html()
  gov_tbl <- html_table(data)[[1]] %>%
    mutate(state = as.character(state_abb[State])) %>%
    select(state, state_full = State, Party)
  states <- states %>%
    left_join(gov_tbl, by = "state") %>%
    filter(!is.na(Party))
  
  
  # Add lock down dates
  balloturl <- "https://ballotpedia.org/Status_of_lockdown_and_stay-at-home_orders_in_response_to_the_coronavirus_(COVID-19)_pandemic,_2020"
  data <- balloturl %>%
    xml2::read_html()
  data <- html_table(data, fill = TRUE)[[3]]
  data <- data[2:nrow(data), 1:2]
  colnames(data) <- c("state", "date")
  data <- data %>%
    separate(date, c("start", "stop"), sep = "-") %>%
    separate(stop, c(NA, "month", "day", NA), sep = " ") %>%
    separate(day, c("day", NA), sep = "\\[") %>%
    unite(stop, month, day, sep = " ") %>%
    mutate(
      start = paste(start, "2020"),
      start = mdy(start),
      stop = paste(stop, "2020"),
      stop = ifelse(grepl("TBD", stop), "June 16 2020", stop),
      stop = mdy(stop),
      days = stop - start,
      days = ifelse(is.na(days), 0, days)
    ) %>%
    select(state, lockdown_days = days)
  states <- states %>%
    left_join(data, by = c("state_full" = "state"))
  
  # Aggregate state data into weekly stats
  weekly_stats_df <- states %>%
    arrange(state, date) %>%
    mutate(
      week = floor_date(date, "weeks"),
      week_id = week(date)
    ) %>%
    group_by(state, week) %>%
    summarize(
      days = n(),
      week_id = first(week_id),
      positiveIncrease = sum(positiveIncrease),
      totalTestResultsIncrease = sum(totalTestResultsIncrease),
      deathIncrease = sum(deathIncrease),
      hospitalizedCurrently = mean(hospitalizedCurrently, na.rm = TRUE),
      hospitalizedCurrently = ifelse(
        is.nan(hospitalizedCurrently), NA, hospitalizedCurrently),
      Party = first(Party),
      population = first(population),
      lockdown_days = first(lockdown_days),
      state_full = first(state_full),
      region = first(region)
    ) %>%
    # filter out any week with <7 days
    filter(days == 7) %>%
    mutate(
      positive_rate = round(positiveIncrease / totalTestResultsIncrease * 100, 1),
      # some errors in the reported data lead to 1-2 negative numbers
      positive_rate = pmax(positive_rate, 0),
      death_rate = round(deathIncrease / population * 1000000, 4),
      hosp_rate = round(hospitalizedCurrently / population * 1000000, 4),
      lockdown_class = case_when(
        lockdown_days > 60 ~ "1.Long",
        lockdown_days > 30 ~ "2.Medium",
        lockdown_days == 0 ~ "4.None",
        TRUE ~ "3.Short"
      ),
      lockdown_class = factor(
        lockdown_class,
        levels = c("1.Long", "2.Medium", "3.Short", "4.None"),
        ordered = TRUE
      )
    ) %>%
    ungroup()
  
  write_csv(weekly_stats_df, weekly_stats_csv)
}

get_data()