
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
library(survey) # for linear combinations, Ref: https://www.patrickbaylis.com/blog/2018-04-12-svycontrasts/
library(bacondecomp) # Goodman-Bacon Decomposition
library(did)

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
  
  # Make data
  num_unit <- 1000
  num_period <- 5
  cohort_periods <- c(2,3) 
  tau <- 1.00 # For each treated period the treatment effect accumulates by this number
  data <- make_data(num_unit, num_period, tau, cohort_periods)
  data <- as.data.table(data)
  
  setnames(data, 'dep_var', 'hrs_listened')
  
  setkeyv(data, c('cohort_period', 'unit', 'period'))
  select_cols <- c('unit', 'period', 'cohort_period','treat','hrs_listened')
  
  # Final plot showing the outcome over time
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
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
  data_hetero[cohort_period==2, tau_cum := cumsum(tau), by = unit]
  # calculate the dependent variable
  data_hetero[cohort_period==2, hrs_listened := calc_dep_var(constant,unit_fe,period_fe,tau_cum,error)]
  
  setkeyv(data_hetero, c('unit', 'period'))
  
# EDA and Analysis --------------------------------------------------------
  # Check out the data
  kable(head(data_hetero[, ..select_cols]), 'simple')
  
  kable(summary(data_hetero[, ..select_cols]), 'simple')
  nrow(data_hetero)
  # We have 10000 units and 5 periods (incl. 0). This makes a total of 50000 observations.
  
  data_hetero[, .N, by = cohort_period]
  data_hetero[, .(mean_hrs_listened = mean(hrs_listened)), by = cohort_period]
  avg_dv_period <- data_hetero[, .(mean_hrs_listened = mean(hrs_listened)), by = c('cohort_period','period')] 
  data_hetero[, .(mean_hrs_listened = mean(hrs_listened)), by = c('cohort_period','treat')] 
  
  # The true treatment effect
  data_hetero[, .(mean_treat_effect = mean(tau_cum)), by = c('cohort_period','treat')]
  # The treatment effect changes overtime
  avg_treat_period <- data_hetero[treat == 1, .(mean_treat_effect = mean(tau_cum)), by = c('cohort_period','period')]
  
  
  data_hetero[cohort_period==2, mean(tau), by = period]
  data_hetero[cohort_period==2, mean(tau_cum), by = period]
  data_hetero[cohort_period==2, mean(hrs_listened), by = period]
  
  data_hetero[, mean(tau_cum), by = c('period','cohort_period')]
  
  data_hetero[, mean(hrs_listened), by = c('period','cohort_period')]
  
  # Visualize 
  avg_dv_period <- data_hetero[, .(mean_hrs_listened = mean(hrs_listened)), by = c('cohort_period','period')] # Graph this
  
  ggplot(avg_dv_period, aes(fill=factor(cohort_period), y=mean_hrs_listened, x=period)) + 
    geom_bar(position=position_dodge2(), stat="identity") +  coord_cartesian(ylim=c(79,85))+
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
  plot_te <- ggplot(avg_treat_period, aes(fill=factor(cohort_period), y=mean_treat_effect, x=period)) + 
    geom_bar(position=position_dodge2(preserve = "single"), stat="identity") +  #coord_cartesian(ylim=c(79,85))+
    labs(x = "Period", y = "Hours", title = 'True treatment effects (hrs)',
         caption = 'Cohort 2 is the early treated and cohort 3 is the late treated group.',
         subtitle = 'Treatment effect heterogeneity across cohorts') + 
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) + scale_fill_manual(values=cbPalette) + 
    guides(fill=guide_legend(title="Treatment cohort period")) + 
    scale_x_continuous(breaks = unique(data_hetero$period))
  plot_te
  
  
  # Report period-cohort event study result
  # Create function to report simple event study results
  tbl_res_post_period <- function(model){
    res <- as.data.table(model$coeftable, keep.rownames = T)
    res[, rn := gsub('post_period_','', rn)]
    res[, rn := as.numeric(gsub('min','-', rn))]
    res <- res %>% 
      mutate_at(vars(-rn, -coef_name), funs(round(., 2)))
    setnames(res, 'rn','Post-period')
    setorder(res, by = 'Post-period')
    return(res)
  }
  
  tbl_res_period_cohort <- function(model, round){
    res <- as.data.table(model$coeftable, keep.rownames = T)
    
    res[, 'Cohort period' := as.numeric(str_extract(rn, "(?i)(?<=cohort_period_)\\d+"))]
    res[, coef_name := rn]
    res[, rn := gsub('cohort_period_3','', rn)]
    res[, rn := gsub('cohort_period_2','', rn)]
    res[, rn := gsub(':','', rn)]
    
    res[, rn := gsub('post_period_','', rn)]
    res[, rn := as.numeric(gsub('min','-', rn))]
    
    if(round == T){
      res <- res %>% 
        mutate_at(vars(-rn,-coef_name), funs(round(., 2)))
    }
    setnames(res, 'rn','Post-period')
    setorderv(res, c('Cohort period','Post-period'))
    setcolorder(res, 'Cohort period')
    return(res)
  }
  
  # A) Canonical DiD
  formula <- as.formula('hrs_listened ~ treat')
  canonical_did <- feols(formula,
               data = data_hetero, panel.id = "unit",
               fixef = c("unit", "period"), cluster = "unit")
  summary(canonical_did) 

  # True estimates vs 
  plot_te + geom_hline(aes(yintercept = -.2, color = 'red'))
  
  # Bacon Decomposition
  bacon_decomp <- bacon(formula, data_hetero, id_var="unit", time_var='period', quietly = F)
  sum(bacon_decomp$weight * bacon_decomp$estimate)
  
  # C) DiD with post-period-cohort specific TEs
  # Create interaction variables
  data_hetero[, .(mean_te = mean(tau_cum)), by = c('cohort_period', 'post_period')]
  
  data_hetero <- data_hetero %>% 
    dummy_cols(select_columns = "cohort_period")
  cohort_dummies <- colnames(data_hetero)[grepl('cohort_period_',colnames(data_hetero))]
  cohort_dummies <- setdiff(cohort_dummies, 'cohort_period_99')
  # Create interactions between Post-period and cohort dummies
  covs <- setdiff(rel_per_dummies_new, c('post_period_min1'))
  interact <- as.data.table(expand_grid(cohort_dummies, covs))
  interact[, interaction := paste0(cohort_dummies,':',covs)]
  interact_covs <- setdiff(interact$interaction, 'cohort_period_3:post_period_2')
  interact_covs_collapse <- paste0(interact_covs,collapse = '+')
  
  
  # We cannot estimate the treatment effect for the late treated cause there is no
  # feasible control for that group?
  # Cut out the data when eb is treated? --> yes. 
  # We can only have the est for period 2 for earlier treated cohort
  formula <- as.formula(paste0('hrs_listened ~ ',interact_covs_collapse))
  model <- feols(formula,
               data = data_hetero[period < 3], panel.id = "unit",
               fixef = c("unit", "period"), cluster = "unit")
  summary(model) 
  
  # Santanna & Callaway
  out <- att_gt(yname = "hrs_listened",
                gname = "cohort_period",
                idname = "unit",
                tname = "period",
                xformla = ~1,
                data = data_hetero,
                # est_method = "reg",
                control_group = 'notyettreated'
  )
  
  es <- aggte(out, type = "dynamic")
  group_effects <- aggte(out, type = "group")
  ggdid(out)
  
  # Why the estimates are slightly different? 
  # est procedure & randomness?
  # Also did package reports simult. conf band. 
  # using higher critical values to account for simultanous testing
  # Thus the diff in the significance in addition to diff in the SEs
  
  res <- tbl_res_period_cohort(model, round = F)
  res_rounded <- tbl_res_period_cohort(model, round = T)
  kable(res_rounded, 'simple')

  
# Visualization estimation results 
  canonical_did_est = round(canonical_did$coefficients[['treat']], 2)
  
  plot_te <- ggplot(res, aes(fill=factor(`Cohort period`), y=Estimate, x=`Post-period`)) + 
    geom_bar(position=position_dodge(preserve = "single"), stat="identity") +  #coord_cartesian(ylim=c(79,85))+
    labs(x = "Post-period", y = "Hours", title = 'Estimated treatment effects (hrs)',
         caption = paste0('Cohort 2 is the early- and cohort 3 is the late-treated group. \n',
                          'The error bars indicate the 95% CI around the estimated treatment effect. '),
         subtitle = 'The treatment effects are estimated correctly with a more flexible approach. ') + 
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) + scale_fill_manual(values=cbPalette) + 
    guides(fill=guide_legend(title="Treatment cohort period")) + 
    scale_x_continuous(breaks = unique(data_hetero$period)) + 
    geom_errorbar(aes(ymin=Estimate-1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`),
                  position=position_dodge(preserve = "single", width = .9), 
                  width = .3) + 
    geom_hline(yintercept =canonical_did_est, linetype="dashed", color = "red", linewidth=.5) +
    annotate("text", x=1.85, y=-0.4, color="red", size = 3, 
             label=paste0("Canonical DiD est. = ", canonical_did_est))
  plot_te
  
  
# Aggregate the estimated TEs
  res
  
  # Need to add weights by unit and divy the weight across treated periods
  # I have a balanced sample
  # cohort_weight = num_units_group / num_units_group
  # cohort_period_weight = cohort_weight / num_post_period
  
  tot_num_units = length(unique(data_hetero$unit))
  
  weights <- data_hetero[post_period>-1, .(N=.N), by = c('cohort_period', 'post_period')]
  # Calculate weights for each cohort given the number of units in that cohort
  weights[, weight_cohort := N/tot_num_units]
  # Divy up the weight for a cohort across their treated periods
  weights[, num_period_cohort := .N, by = cohort_period]
  weights[, weight_cohort_period := weight_cohort/num_period_cohort, by = cohort_period]
  weights[, weights_cohort_aggr := 1/num_period_cohort]
  # This type of weighting avoids overweighing earlier treated cohorts (you would 
  # have this problem if you calculate the weights simply by the observation numbers)

  est_weights <- merge(res, weights, 
                       by.x = c('Cohort period', 'Post-period'),
                       by.y = c('cohort_period', 'post_period'))
  
  # Aggregate 
  # One can aggregate to either cohort, post period or a total TE
  # Ideally take the multip testing into account, not the topic of this blog
  # To say something overall 
  
  # Aggregate to cohort level
  cohort_2_weights <- est_weights[`Cohort period`==2]
  cohort_3_weights <- est_weights[`Cohort period`==3]
  formula_cohort_2 <- setNames(cohort_2_weights$weights_cohort_aggr, cohort_2_weights$coef_name)
  formula_cohort_3 <- setNames(cohort_3_weights$weights_cohort_aggr, cohort_3_weights$coef_name)
  
  formula_total_te <-   setNames(est_weights$weight_cohort_period, est_weights$coef_name)
  
  aggregate_results <- function(model, formula){
    est = svycontrast(model, formula)[[1]]
    se = SE(svycontrast(model, formula))[[1]]
    return(list('est'= est, 'se'=se))
  }
  
  aggregate_results(model, formula_cohort_2)
  aggregate_results(model, formula_cohort_3)
  overall_te <- aggregate_results(model, formula_total_te)
  
  
  # Graph
  graph_coef_weights <- function(...){
    g <- ggplot(est_weights, aes(x=`Post-period`, y = Estimate, 
                                  color = "Coef.", group = as.factor(`Cohort period`))) +
      geom_point(position = position_dodge2(width = .4), 
                 aes(color = as.factor(`Cohort period`))) # , size = weight_cohort_period/10
    g <- g + #facet_grid(`Cohort period` ~ .) + 
      geom_errorbar(aes(ymin=Estimate-1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`, 
                        color = as.factor(`Cohort period`)), # size = prop_in_total/1000
                    width=.4,size=.5,position = position_dodge2(width = .5)) +
      geom_hline(yintercept =canonical_did_est, linetype="dashed", color = "red", linewidth=.5) +
      labs(x = "Post-treatment period", y = 'Coef. Est.', 
           color = 'Cohort', title = 'Flexible vs. Canonical DiD estimates')  + # size = 'Weight'
      theme_light(base_size = 16) +
      annotate("text", x=1.95, y=0.2, color="red", size = 3, 
               label=paste0("Canonical DiD est. = ", canonical_did_est))+
    geom_hline(yintercept =overall_te[['est']], linetype="dashed", color = "blue", linewidth=.5)+
      annotate("text", x=1.85, y=5, color="blue", size = 3, 
               label=paste0("Aggregated flexible DiD est. = ", round(overall_te[['est']],2)))

    print(g)
  }
  
  graph_coef_weights()
  
  
  