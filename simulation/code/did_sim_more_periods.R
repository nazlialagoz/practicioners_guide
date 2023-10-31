
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
      mutate(error = rnorm(tot_num_obs, 0, 0.5),
             treat = ifelse(period >= cohort_period, 1, 0),
             tau = ifelse(treat == 1, mu, 0)) %>% 
      # calculate cumulative treatment effects
      group_by(unit) %>% 
      mutate(tau_cum = cumsum(tau)) %>% 
      ungroup() %>% 
      # calculate the dep variable
      mutate(dep_var = calc_dep_var(constant, unit_fe, period_fe, tau_cum,error))
  }
  
  # Make data
  num_unit <- 10000
  num_period <- 20
  cohort_periods <- c(2,3,99) # Cohort period 99 is the untreated cohort
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
  

  # Final plot showing the outcome over time
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  ggplot(avg_dv_period, aes(color=factor(cohort_period), y=mean_hrs_listened, x=period)) + 
    geom_line() + # geom_bar(position="dodge", stat="identity") +  coord_cartesian(ylim=c(79,85))+
    labs(x = "Period", y = "Hours", title = 'Average music listening (hours)', 
         caption = 'Cohort 2 is the early treated, cohort 3 is the late treated and cohort 99 is the never treated group.') + 
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) + scale_fill_manual(values=cbPalette) +
    geom_vline(xintercept = 1.5, color = '#999999', lty = 5)+
    geom_vline(xintercept = 2.5, color = '#E69F00', lty = 5) + 
    geom_text(label = 'Cohort period 2 is treated',aes(1.4,83), color = '#999999', angle = 90)+
    geom_text(label = 'Cohort period 3 is treated',aes(2.4,83), color = '#E69F00', angle = 90) +
    guides(fill=guide_legend(title="Treatment cohort period"))
  
  # Treatment effect 
  ggplot(avg_treat_period, aes(color=factor(cohort_period), y=mean_treat_effect, x=period)) + 
    geom_line() + geom_point() + # geom_bar(position="dodge", stat="identity") +  #coord_cartesian(ylim=c(79,85))+
    labs(x = "Period", y = "Hours", title = 'True treatment effect (hrs)',
         caption = 'Cohort 2 is the early treated, cohort 3 is the late treated and cohort 99 is the never treated group.') + 
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) + scale_fill_manual(values=cbPalette) + 
    guides(fill=guide_legend(title="Treatment cohort period"))

  
  # Create relative time dummies to use in the regression
  data <- data %>% 
    # make relative year indicator
    mutate(rel_period = ifelse(cohort_period == 99,99,period - cohort_period))
  summary(data$rel_period)
  
  data <- data %>% 
    dummy_cols(select_columns = "rel_period")
  
  rel_per_dummies <- colnames(data)[grepl('rel_period_', colnames(data))]
  # Change name w/ minuses to handle them more easily
  rel_per_dummies_new<-gsub('-','min', rel_per_dummies)
  setnames(data, rel_per_dummies, rel_per_dummies_new)
  
# Event studies in the simple situation -----------------------------------
  # A) Single treatment effect regression to start off with (Single TE)
  formula <- as.formula('hrs_listened ~ treat')
  model <- feols(formula,
               data = data, panel.id = "unit", # [cohort_period != 99]
               fixef = c("unit", "period"))
  summary(model)
  
  # B) Event study
  covs <- setdiff(rel_per_dummies_new, c('rel_period_99','rel_period_min1'))
  covs_collapse <- paste0(covs, collapse='+')
  formula <- as.formula(paste0('hrs_listened ~ ',covs_collapse))
  model <- feols(formula,
               data = data, panel.id = "unit",
               fixef = c("unit", "period"))
  summary(model)
  
  # Create function to report simple event study results
  tbl_res_rel_period <- function(model){
    res <- as.data.table(model$coeftable, keep.rownames = T)
    res[, rn := gsub('rel_period_','', rn)]
    res[, rn := as.numeric(gsub('min','-', rn))]
    res <- res %>% 
      mutate_at(vars(-rn), funs(round(., 2)))
    setnames(res, 'rn','Relative period')
    setorder(res, by = 'Relative period')
    return(res)
  }
  
  kable(tbl_res_rel_period(model), 'simple')
  
  # C) Event study with cohort specific treatment effects
  # Create dummies for the cohort-period 
  data <- data %>% 
    dummy_cols(select_columns = "cohort_period")
  cohort_dummies <- c('cohort_period_2','cohort_period_3')
  # Create interactions between relative period and cohort dummies
  interact <- as.data.table(expand_grid(cohort_dummies, covs))
  # Ps. one can drop rel_period_min3 for cohort 2 and 
  # rel_period_2 for cohort 3 as these observations don't exist 
  # but I'll let the regression drop these
  interact[, interaction := paste0(cohort_dummies,':',covs)]
  interact_covs <- interact$interaction
  interact_covs_collapse <- paste0(interact_covs,collapse = '+')
  
  formula <- as.formula(paste0('hrs_listened ~ ',interact_covs_collapse))
  model <- feols(formula,
               data = data, panel.id = "unit",
               fixef = c("unit", "period"))
  summary(model)
  # Two variables are naturally dropped because the most negative relative time period 
  # for cohort 2 is -2 and for the cohort period 3, 
  # we only have relative period up to 1.
  
  # The results are consistent with the true effects. There are no statistically 
  # significant effects pre-treatment and there are positive treatment effects 
  # after the treatment
  

  # Report period-cohort event study result
  tbl_res_period_cohort <- function(model){
    res <- as.data.table(model$coeftable, keep.rownames = T)
    
    res[, 'Cohort period' := as.numeric(str_extract(rn, "(?i)(?<=cohort_period_)\\d+"))]
    res[, rn := gsub('cohort_period_3','', rn)]
    res[, rn := gsub('cohort_period_2','', rn)]
    res[, rn := gsub(':','', rn)]
    
    res[, rn := gsub('rel_period_','', rn)]
    res[, rn := as.numeric(gsub('min','-', rn))]
    res <- res %>% 
      mutate_at(vars(-rn), funs(round(., 2)))
    setnames(res, 'rn','Relative period')
    setorderv(res, c('Cohort period','Relative period'))
    setcolorder(res, 'Cohort period')
    return(res)
  }
  
  kable(tbl_res_period_cohort(model), format = 'simple')


# 2.  Heterogeneity -------------------------------------------------------
  # Until now, the dynamics of the TE between the treatment groups are the same
  # They are affected by the treatment in the same fashion overtime
  # However, what if the first treated group is affected much more positively?
  data_hetero <- copy(data)

  # calculate tau
  data_hetero[cohort_period==2, tau := 6*tau]
  # calculate tau_cum
  setkeyv(data_hetero, c('unit', 'period')) # order
  data_hetero[cohort_period==2, tau_cum := cumsum(tau), by = unit]
  # calculate the dependent variable
  data_hetero[cohort_period==2, hrs_listened := calc_dep_var(constant,unit_fe,period_fe,tau_cum,error)]
  
  data_hetero[cohort_period==2, mean(tau), by = period]
  data_hetero[cohort_period==2, mean(tau_cum), by = period]
  data_hetero[cohort_period==2, mean(hrs_listened), by = period]
  
  data_hetero[, mean(tau_cum), by = c('period','cohort_period')]
  
  data_hetero[, mean(hrs_listened), by = c('period','cohort_period')]
  
  # Visualize 
  avg_dv_period <- data_hetero[, .(mean_hrs_listened = mean(hrs_listened)), by = c('cohort_period','period')] # Graph this
  
  ggplot(avg_dv_period, aes(color=factor(cohort_period), y=mean_hrs_listened, x=period)) + 
    geom_line() + # geom_bar(position="dodge", stat="identity") +  coord_cartesian(ylim=c(79,85))+
    labs(x = "Period", y = "Hours", title = 'Average music listening (hours)', 
         caption = 'Cohort 2 is the early treated, cohort 3 is the late treated and cohort 99 is the never treated group.',
         subtitle = 'Treatment effect heterogeneity across cohorts') + 
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) + scale_fill_manual(values=cbPalette) +
    geom_vline(xintercept = 1.5, color = '#999999', lty = 5)+
    geom_vline(xintercept = 2.5, color = '#E69F00', lty = 5) + 
    geom_text(label = 'Cohort period 2 is treated',aes(1.4,83), color = '#999999', angle = 90)+
    geom_text(label = 'Cohort period 3 is treated',aes(2.4,83), color = '#E69F00', angle = 90) +
    guides(fill=guide_legend(title="Treatment cohort period"))
  
  # Treatment effect 
  avg_treat_period <- data_hetero[treat == 1, .(mean_treat_effect = mean(tau_cum)), by = c('cohort_period','period')]
  ggplot(avg_treat_period, aes(color=factor(cohort_period), y=mean_treat_effect, x=period)) + 
    geom_line() + # geom_bar(position="dodge", stat="identity") +  #coord_cartesian(ylim=c(79,85))+
    labs(x = "Period", y = "Hours", title = 'True treatment effect (hrs)',
         caption = 'Cohort 2 is the early treated and cohort 3 is the late treated group.',
         subtitle = 'Treatment effect heterogeneity across cohorts') + 
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) + scale_fill_manual(values=cbPalette) + 
    guides(fill=guide_legend(title="Treatment cohort period"))
  
  
  # ???????????????? EXCLUDE UNTREATED   ???????????????????? 
  data_hetero <- data_hetero[cohort_period != 99] 
  
  
  
  
  # I feel like having the nontreated helped figuring out the
  # approx TE. Once it is dropped the est TE is negative. 
  
  # Maybe we can also just increase the number of periods so
  # effects really compund. 
  
  # A) Single TE model
  mean(avg_treat_period$mean_treat_effect)
  formula <- as.formula('hrs_listened ~ treat')
  model <- feols(formula,
               data = data_hetero[cohort_period != 99], panel.id = "unit",
               fixef = c("unit", "period"))
  summary(model)
  
  # B) Event study
  formula <- as.formula(paste0('hrs_listened ~ ',covs_collapse))
  model <- feols(formula,
               data = data_hetero, panel.id = "unit",
               fixef = c("unit", "period"))
  summary(model)
  
  kable(tbl_res_rel_period(model), 'simple')
  
  # C) Event study with cohort specific treatment effects
  formula <- as.formula(paste0('hrs_listened ~ ',interact_covs_collapse))
  model <- feols(formula,
               data = data_hetero, panel.id = "unit",
               fixef = c("unit", "period"))
  summary(model)

  kable(tbl_res_period_cohort(model), 'simple')

