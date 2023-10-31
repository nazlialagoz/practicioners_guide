# Simulate the data for the DiD blog


# Simulation study for the Medium article: 


# This code simulates a panel dataset and then runs event studies 
# Different scenarios are created to demonstrate pitfalls of event studies
# The simulaiton part of the code is adapted from Andrew Baker's awesome blog: 
# https://andrewcbaker.netlify.app/2020/06/27/how-to-create-relative-time-indicators/
# Also see a relevant package and blog by Sant'Anna & Callaway: 
# https://bcallaway11.github.io/did/articles/pre-testing.html

rm(list = ls())
library(data.table)
library(fastDummies)
library(tidyverse)
library(ggthemes)
library(fixest)
library(kableExtra)
library(bacondecomp)

theme_set(theme_clean() + theme(plot.background = element_blank()))
select <- dplyr::select
set.seed(123)

# Simulate data -----------------------------------------------------------
# Create a function that takes in the number of units, the number of periods,
# a treatment effect that will accumulate overtime (tau), and
# cohort periods (in which periods a cohort is treated). 

# If a cohort period is greater than the end observation period this means that
# this cohort is untreated during the observation period. 

calc_dep_var <- function(constant, unit_fe, period_fe, tau_cum, error){
  dep_var = constant + unit_fe + period_fe + tau_cum + error
}

constant = 80 # set the constant so that the outcome variable makes sense for the example

make_data <- function(num_unit, num_period,tau, cohort_periods){
  end_period = num_period-1 # periods start from 0
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:num_unit, 
    unit_fe = rnorm(num_unit, 0, .1),
    # Assign units to different treatment cohorts
    cohort_period = sample(cohort_periods, num_unit, replace = TRUE), 
    # generate treatment effect
    mu = rnorm(num_unit, tau, 0.2))
  
  # period fixed effects 
  period <- tibble(
    period = 0:end_period,
    period_fe = rnorm(num_period, 0, .1))
  
  # Trend Break -------------------------------------------------------------
  
  # make main dataset
  # full interaction of unit X period 
  tot_num_obs = num_unit*num_period 
  
  expand_grid(unit = 1:num_unit, period = 0:end_period) %>% 
    left_join(., unit) %>% 
    left_join(., period) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(tot_num_obs, 0, 10),
           treat = ifelse(period >= cohort_period, 1, 0),
           tau = ifelse(treat == 1, mu, 0)) %>% 
    # calculate cumulative treatment effects
    group_by(unit) %>% 
    mutate(tau_cum = cumsum(tau)) %>% 
    ungroup() %>% 
    # calculate the dep variable
    mutate(dep_var = calc_dep_var(constant, unit_fe, period_fe, tau_cum,error))
}

simulate_final_data <- function(...){

  # Make data
  num_unit <- 10000
  num_period <- 5
  cohort_periods <- c(2,3) 
  tau <- 1.00 # For each treated period the treatment effect accumulates by this number
  data <- make_data(num_unit, num_period, tau, cohort_periods)
  data <- as.data.table(data)
  
  setnames(data, 'dep_var', 'hrs_listened')
  
  setkeyv(data, c('cohort_period', 'unit', 'period'))
  select_cols <- c('unit', 'period', 'cohort_period','treat','hrs_listened')
  
  kable(head(data[, ..select_cols]), 'simple')
  
  kable(summary(data[, ..select_cols]), 'simple')
  nrow(data)
  # We have 10000 units and 5 periods (incl. 0). This makes a total of 50000 observations.
  
  data[, .N, by = cohort_period]
  data[, .(mean_hrs_listened = mean(hrs_listened)), by = cohort_period]
  avg_dv_period <- data[, .(mean_hrs_listened = mean(hrs_listened)), by = c('cohort_period','period')] 
  data[, .(mean_hrs_listened = mean(hrs_listened)), by = c('cohort_period','treat')] 
  
  # The true treatment effect
  data[, .(mean_treat_effect = mean(tau_cum)), by = c('cohort_period','treat')]
  # The treatment effect changes overtime
  avg_treat_period <- data[treat == 1, .(mean_treat_effect = mean(tau_cum)), by = c('cohort_period','period')]
  
  # Create relative time dummies to use in the regression
  data <- data %>% 
    # make relative year indicator
    mutate(post_period = ifelse(treat == 1,period - cohort_period, -1L))
  summary(data$post_period)
  
  data <- data %>% 
    dummy_cols(select_columns = "post_period")
  
  rel_per_dummies <- colnames(data)[grepl('post_period_', colnames(data))]
  # Change name w/ minuses to handle them more easily
  rel_per_dummies_new<-gsub('-','min', rel_per_dummies)
  setnames(data, rel_per_dummies, rel_per_dummies_new)
  
  # Heterogeneity -------------------------------------------------------
  # Until now, the dynamics of the TE between the treatment groups are the same
  # They are affected by the treatment in the same fashion overtime
  # However, what if the first treated group is affected much more positively?
  data_hetero <- copy(data)
  
  # calculate tau
  data_hetero[cohort_period==2, tau := 4*tau]
  # calculate tau_cum
  setkeyv(data_hetero, c('unit', 'period')) # order
  
  return(data_hetero)
}
