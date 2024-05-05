# Simulate the data 


# The simulaiton part of the code is adapted from Andrew Baker's awesome blog: 
# https://andrewcbaker.netlify.app/2020/06/27/how-to-create-relative-time-indicators/
# Also see a relevant package and blog by Sant'Anna & Callaway: 
# https://bcallaway11.github.io/did/articles/pre-testing.html

rm(list = ls())
library(data.table)
library(fastDummies)
library(tidyverse)
library(ggthemes)

select <- dplyr::select
set.seed(123)

# Utilities: set theme and choose a palette for the graphs
theme_set(theme_clean() + theme(plot.background = element_blank()))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Initiate the data -----------------------------------------------------------
# Create a function that takes in the number of units, the number of periods,
# a treatment effect that will accumulate overtime (tau), and
# cohort periods (in which periods a cohort is treated). 

calc_dep_var <- function(constant, unit_fe, period_fe, tau_cum, error){
  dep_var = constant + unit_fe + period_fe + tau_cum + error
}

init_data <- function(num_unit, num_period, tau, cohort_periods, constant){
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
    # calculate the dependent variable
    mutate(dep_var = calc_dep_var(constant, unit_fe, period_fe, tau_cum,error))
}

# Initiate the data and add heterogeneity in treatment effects (TE) ------------
# Until now, the dynamics of the TE between the treatment groups are the same
# They are affected by the treatment in the same fashion overtime
# However, what if the first treated group is affected much more positively?

sim_data <- function(...){
  constant = 80
  data <- as.data.table(init_data(num_unit = 1000, 
                                  num_period = 10, 
                                  tau = -1.00, 
                                  cohort_periods = c(2,3,4,99,100,101), # c(2,3,4,5,6,7,8)
                                  constant = constant))
  
  # setnames(data, 'dep_var', 'hrs_listened')
  setkeyv(data, c('cohort_period', 'unit', 'period'))
  
  # Introduce heterogeneity in treatment effects 
  # calculate the new tau
  data[cohort_period==2, tau := 3*tau]
  # calculate tau_cum
  setkeyv(data, c('unit', 'period')) # order
  data[cohort_period==2 & period == 2, tau := 21] # increase initial effect
  data[cohort_period==2, tau_cum := cumsum(tau), by = unit]
  # calculate the dependent variable
  data[cohort_period==2, dep_var := calc_dep_var(constant,unit_fe,period_fe,tau_cum,error)]
  
  # Introduce heterogeneity in treatment effects 
  # calculate the new tau
  data[cohort_period==3, tau := 2*tau]
  # calculate tau_cum
  setkeyv(data, c('unit', 'period')) # order
  data[cohort_period==3 & period == 3, tau := 12] # increase initial effect
  data[cohort_period==3, tau_cum := cumsum(tau), by = unit]
  # calculate the dependent variable
  data[cohort_period==3, dep_var := calc_dep_var(constant,unit_fe,period_fe,tau_cum,error)]
  
  
  data[cohort_period==4 & period == 4, tau := 5] # increase initial effect
  data[cohort_period==4, tau_cum := cumsum(tau), by = unit]
  # calculate the dependent variable
  data[cohort_period==4, dep_var := calc_dep_var(constant,unit_fe,period_fe,tau_cum,error)]
  
  
  setkeyv(data, c('unit', 'period'))
  
  return(data)
}

