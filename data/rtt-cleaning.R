##### RTT Data Cleaning for Shiny app #####

# clear workspace
rm(list = ls()) 

# load libraries
library(readxl)
library(janitor)
library(tidyverse)
library(zoo)

# load CSV of data
# this has been *slightly* cleaned in excel (made top merged row into part of the column headers and got rid of blank rows)--
# could be done in R to make it completely reproducible, maybe using existing pipeline 

rtt_data_raw <- read_excel("data/rtt-data.xlsx", na = "-") %>% clean_names()

latest_data <- ymd("2023-06-01")

##### Clean data #####

rtt_data <- rtt_data_raw %>% 
  
  # select only cols of interest- total waiting, new referrals, non admitted, admitted
  select(month_year
         , incomplete_total_waiting_mil_with_estimates_for_missing_data
         , new_referrals_no_of_new_rtt_periods_with_estimates_for_missing_data
         , non_admitted_no_of_pathways_all_with_estimates_for_missing_data
         , admitted_unadj_no_of_pathways_all_with_estimates_for_missing_data
  ) %>%
  
  # make names nicer
  rename(
    waiting_list = incomplete_total_waiting_mil_with_estimates_for_missing_data
    , new_referrals = new_referrals_no_of_new_rtt_periods_with_estimates_for_missing_data
    , completed_non_admitted = non_admitted_no_of_pathways_all_with_estimates_for_missing_data
    , completed_admitted = admitted_unadj_no_of_pathways_all_with_estimates_for_missing_data
  ) %>% 
  
  # convert month year to lubridate and get total completed
  mutate(
    month_year = ymd(month_year)
    , completed_total = completed_admitted + completed_non_admitted
  ) %>% 
  
  # # NO LONGER NEEDED
  # # get 3-month moving averages for completed and new referrals (using previous two months + current month, i.e. right aligned)
  # mutate(
  #   completed_total_ma = rollmean(completed_total, k = 3, align = "right", fill = NA)
  #   , new_referrals_ma = rollmean(new_referrals, k = 3, align = "right", fill = NA)
  # ) %>% 
  
  # get total outflow
  mutate(
    waiting_list_last_val = lag(waiting_list) # find the last value-- doing explicitly for QA purposes
  , total_activity = waiting_list_last_val - waiting_list + new_referrals
  , other_reasons = total_activity - completed_total
  ) %>% 
  
  # just include data from FY 2016 to June 2023
  filter(month_year >= ymd("2016-04-01") & month_year <= latest_data)
  
  
 
##### get trendlines for pre and post-pandemic ahead of time #####

# fit a line to pre and post pandemic referrals and completed
pre_pandemic_referrals_line <- predict(lm(new_referrals ~ month_year, data = rtt_data[rtt_data$month_year < ymd("2020-03-01"),]))
post_pandemic_referrals_line <- predict(lm(new_referrals ~ month_year, data = rtt_data[rtt_data$month_year > ymd("2021-04-01"),]))
pre_pandemic_activity_line <- predict(lm(total_activity ~ month_year, data = rtt_data[rtt_data$month_year < ymd("2020-03-01"),]))
post_pandemic_activity_line <- predict(lm(total_activity ~ month_year, data = rtt_data[rtt_data$month_year > ymd("2021-04-01"),]))

# get vector to plot lines and assign it to a new var in rtt_data
# rep NA 14 times for no line during COVID months
rtt_data$referrals_trend <- c(pre_pandemic_referrals_line, rep(NA_real_, 14), post_pandemic_referrals_line)
rtt_data$activity_trend <- c(pre_pandemic_activity_line, rep(NA_real_, 14), post_pandemic_activity_line)


###### daily seasonality #####

# read in CSV with number of working days each month 
workdays <- read.csv("working-days-table.csv") %>% 
  mutate(month_year = ymd(month_year))

rtt_data <- rtt_data %>% 
  left_join(workdays, by = "month_year") 

# for seasonality: limit to up to FY18/19, calculate a daily rate, get yearly average, residual between month and year, and get avg deviance from yearly average 
seasonality <- rtt_data %>% 
  filter(month_year < ymd("2019-04-01")) %>% 
  # get day rate for each month
  mutate(new_referrals_day_rate = new_referrals / workdays
         , total_activity_day_rate = total_activity / workdays) %>%
  # get financial year 
  mutate(fin_year = floor(quarter(month_year, with_year = TRUE, fiscal_start = 4))) %>% 
  # group by year to get a yearly average
  group_by(fin_year) %>% 
  mutate(avg_yearly_referral_day_rate = mean(new_referrals_day_rate)
         , avg_yearly_activity_day_rate = mean(total_activity_day_rate)) %>% 
  ungroup() %>% 
  # get proportional difference between month and year
  mutate(referrals_diff = new_referrals_day_rate/avg_yearly_referral_day_rate
         , activity_diff = total_activity_day_rate/avg_yearly_activity_day_rate) %>% 
  # group by month and get average monthly seasonality
  group_by(month = month(month_year)) %>% 
  summarise(referrals_seasonality = mean(referrals_diff)
            , activity_seasonality = mean(activity_diff))

##### Save data to use in app #####
saveRDS(rtt_data, "data/rtt_data.RDS")

saveRDS(seasonality, "data/seasonality.RDS")

