
# Simulation study for the DiD article: 

rm(list = ls())
source('../../simulation/code/sim_data.R') # Import simulation function and some utilities
source('common_def_func.R') # Import libraries and common functions

dt <- sim_data()
unique(dt$cohort_period)
unique(dt$period)
# dt <- dt[] # exclude period where everyone is treated so that we have an untreated group

# EDA and Analysis --------------------------------------------------------
# Check out the data

select_cols <- c('unit', 'period', 'cohort_period','treat','hrs_listened')

kable(head(dt[, ..select_cols]), 'simple')

kable(summary(dt[, ..select_cols]), 'simple')

dt[, .N, by = cohort_period] # group sizes
dt[, .(mean_hrs_listened = mean(hrs_listened)), by = cohort_period]

# Visualize the outcome variable -----------------------------------------------
avg_dv_period <- dt[, .(mean_hrs_listened = mean(hrs_listened)), by = c('cohort_period','period')] 

# Convert 'cohort_period' to a factor to have discrete color scale in the plot
avg_dv_period[, cohort_period := as.factor(cohort_period)]

# Plot
ggplot(avg_dv_period, aes(x = period, y = mean_hrs_listened, group = cohort_period, color = cohort_period)) +
  geom_line() +
  geom_vline(data = avg_dv_period[cohort_period == period], aes(xintercept = period), linetype = "dashed") +
  scale_color_discrete(name = "Cohort Period") +
  labs(title = "Trends in Mean Hours Listened by Cohort Period",
       x = "Period",
       y = "Mean Hours Listened") +
  theme_minimal()
ggsave(paste(out_dir, 'plot_outcome_by_cohort_period.png'))


# Black and white friendly
# Define the linetypes and shapes based on the number of unique cohort_periods
line_types <- c("solid", "dashed", "dotted", "twodash", "longdash")
shapes <- c(16, 17, 18, 19, 15)  # Filled shapes
num_cohorts <- length(unique(avg_dv_period$cohort_period))
line_types <- line_types[1:num_cohorts]
shapes <- shapes[1:num_cohorts]

# Plot
gg <- ggplot(avg_dv_period, aes(x = period, y = mean_hrs_listened, group = cohort_period)) +
  geom_line(aes(linetype = cohort_period)) +
  geom_point(aes(shape = cohort_period)) +
  geom_vline(data = avg_dv_period[cohort_period == period], aes(xintercept = period, linetype = cohort_period), color = "black") +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  labs(title = "Trends in Mean Hours Listened by Cohort Period",
       x = "Period",
       y = "Mean Hours Listened",
       linetype = "Cohort Period",
       shape = "Cohort Period") +
  theme_minimal()

# Set color blindness-friendly palette for digital version and print
gg <- gg + scale_color_brewer(palette = "Dark2", name = "Cohort Period")

print(gg)
ggsave(paste(out_dir, 'plot_outcome_by_cohort_period_bw.png'))


# Plot true treatment effects --------------------------------------------------
avg_treat_period <- dt[treat == 1, .(mean_treat_effect = mean(tau_cum)), by = c('cohort_period','period')]
# Convert 'cohort_period' to a factor for discrete fill patterns
avg_treat_period[, cohort_period := as.factor(cohort_period)]

# Color plot
plot_te <- ggplot(avg_treat_period, aes(fill=factor(cohort_period), y=mean_treat_effect, x=period)) + 
  scale_fill_brewer(palette = "Set1") + # Color palette
  geom_bar(position=position_dodge2(preserve = "single"), stat="identity") +  
  labs(x = "Period", y = "Hours", title = 'True treatment effects (hrs)',
       subtitle = 'Treatment effect heterogeneity across cohorts') + 
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  guides(fill=guide_legend(title="Treatment cohort period")) + 
  scale_x_continuous(breaks = unique(avg_treat_period$period)) + 
  scale_y_continuous(breaks = round(unique(avg_treat_period$mean_treat_effect)))
plot_te

ggsave(paste(out_dir, 'plot_true_te_by_cohort_period.png'))

# Create a grayscale plot
grayscale_plot <- plot_te +
  scale_fill_grey(start = 0.8, end = 0.2) + # Grayscale fill
  labs(title = "True Treatment Effects by Cohort Period and Period (Grayscale)")
grayscale_plot

ggsave(paste(out_dir, 'plot_true_te_by_cohort_period_bw.png'))



# A) Canonical DiD -------------------------------------------------------------
formula <- as.formula('hrs_listened ~ treat')
canonical_did <- feols(formula,
                       data = dt, panel.id = "unit",
                       fixef = c("unit", "period"), cluster = "unit")
summary(canonical_did) 


# Bacon Decomposition
bacon_decomp <- bacon(formula, dt, id_var="unit", time_var='period', quietly = F)
sum(bacon_decomp$weight * bacon_decomp$estimate)

# B) DiD with post-period-cohort specific TEs ----------------------------------

# Drop periods where everyone is treated
dt_did <- dt[period < 3]

# Create dummy variables
dt_did <- dt_did %>% 
  dummy_cols(select_columns = c("cohort_period", "period"))

interact_covs <- 'cohort_period_2:period_2'

# Regression
formula <- as.formula(paste0('hrs_listened ~ ',interact_covs))
model <- feols(formula,
               data = dt_did, panel.id = "unit",
               fixef = c("unit", "period"), cluster = "unit")
summary(model) 

# C) did package by Santanna & Callaway ----------------------------------------
out <- att_gt(yname = "hrs_listened",
              gname = "cohort_period",
              idname = "unit",
              tname = "period",
              xformla = ~1,
              data = dt,
              est_method = "reg",
              control_group = 'notyettreated'
)
out
es <- aggte(out, type = "dynamic")
group_effects <- aggte(out, type = "group")
ggdid(out)

# Why the estimates are slightly different? 
  # est procedure & randomness?
  # Also did package reports simult. conf band. 
  # using higher critical values to account for simultanous testing
  # Thus the diff in the significance in addition to diff in the SEs

# D) ETWFE       ---------------------------------------------------------------
dt
mod_etwfe =
  etwfe(
    fml  = hrs_listened ~ 1, # outcome ~ controls
    tvar = period,        # time variable
    gvar = cohort_period, # group variable
    data = dt,       # dataset
    vcov = ~unit  # vcov adjustment (here: clustered)
  )
summary(mod_etwfe)

# E) Stacked DiD ---------------------------------------------------------------
head(dt)

# Create relative time dummy
dt[, time_since_treat := period-cohort_period]
dt[cohort_period>max(period), time_since_treat:= -999] # nontreated group
dt[, treatment_group:= ifelse(time_since_treat != -999, 1,0)] # nontreated group
sort(unique(dt$time_since_treat))

unique(dt[time_since_treat == -999]$cohort_period)
dt[time_since_treat == -999, cohort_period := 999]


# Simple TWFE --------------------------------------------------------------
# Define reference period: the most negative period
REF_PERIOD = min(unique(dt[cohort_period!=999]$time_since_treat))

# there are no untreated group so not c(REF_PERIOD, -999), ie. ref period the most neg and also untreated
REF_PERIODS = c(-999, REF_PERIOD)

run_twfe <- function(dv){
  formula <- as.formula(paste0(dv,"~ i(time_since_treat, ref = REF_PERIODS)")) 
  twfe_ols <- feols(formula, data = dt, panel.id = "unit",
                    cluster = "unit", fixef = c("unit", "period"))
  print(summary(twfe_ols))
  saveRDS(twfe_ols, paste0(out_dir, dv, '_twfe.rds'))
  
  # Save results as df
  export_reg_as_df(twfe_ols, dv, out_dir,method = 'twfe', ref_periods = REF_PERIODS)
}

run_twfe(dv = 'hrs_listened')


dv = 'hrs_listened'

# Create stacked data -----------------------------------------------------
### for stacking
groups <- unique(dt[cohort_period != 999]$cohort_period)

sort(groups)
sort(unique(dt$period))

MAX_WEEKS = 2 # indicates the time window around treatment

### create stacked data
getdata <- function(i) {
  
  #keep what we need
  dt %>% 
    # keep focal treatment cohort (i) and the correct controls, i.e., cohorts treated after
    # keep treated units and all units not treated within specified time period
    filter(cohort_period == i | cohort_period > (i + MAX_WEEKS)) %>%
    # keep just relevant time periods
    filter(period >= (i - MAX_WEEKS) & period <= (i + MAX_WEEKS)) %>%
    # create an indicator for the dataset
    mutate(df = i) %>% 
    # mutate(time_to_treatment = period - cohort_period) %>% 
    # make dummies
    mutate(time_since_treat = if_else(cohort_period == i, time_since_treat, -999)) # TODO: check this
}
stacked_data <- map_df(groups, getdata) %>% 
  mutate(bracket_df = paste(unit,df))

stacked_data<- as.data.table(stacked_data)
head(stacked_data[df==2,c('unit','period','cohort_period', 'time_since_treat','df','bracket_df')],100)
summary(stacked_data$time_since_treat)
summary(stacked_data$time_since_treat)

summary(stacked_data[cohort_period==999]$time_since_treat) # for the untreated

# Function ----------------------------------------------------------------
run_model <- function(outcome_variable, MAX_WEEKS) {
  
  # fit the model (using REF_PERIOD and untreated people -1000 as reference levels)
  # i() generates a factor variable from time_to_treatment where the reference level is specified by the vector
  # unit^df and period^df are being treated as fixed effects. The ^df notation indicates that each unique combination of unit and df, and period and df, is getting its own fixed effect.
  
  # outcome_variable = 'hrs_listened'
  # ref = REF_PERIODS
  # REF_PERIODS = c(-2,-1, REF_PERIODS)
  
  # Model
  model_formula <- as.formula(paste(outcome_variable, "~ i(time_since_treat, ref = REF_PERIODS) | 
                                     unit^df + period^df"))
  
  model <- feols(model_formula, 
                 data = stacked_data, 
                 cluster = "bracket_df")
  summary(model)
  
  # Process results
  stacked <- broom::tidy(model,conf.int = TRUE) %>% 
    mutate(t =  as.double(str_replace(term, "time_to_treatment::", ""))) %>% 
    filter(t >= -1*MAX_WEEKS & t <= MAX_WEEKS) %>% 
    select(t, estimate, conf.low, conf.high) %>% 
    bind_rows(tibble(t = REF_PERIODS[1], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    bind_rows(tibble(t = REF_PERIODS[2], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    mutate(method = "stacked")
  write.csv(stacked,paste0(out_dir, outcome_variable, '_stacked.csv'))
  
  # Regular avg. 
  stacked <- as.data.table(stacked)
  stacked_treat <- stacked[t>=0]
  stacked_treat[, SE := (conf.high - estimate)/1.96] # given .95 conf level
  stacked_treat[, var := SE^2]
  overall_ATT = sum(stacked_treat$estimate)/length(stacked_treat$estimate)
  overall_ATT_SE = (1/length(stacked_treat$estimate))*sqrt(sum(stacked_treat$var))
  overall_ATT_t_value = overall_ATT/overall_ATT_SE
  overall_ATT_p_value <- round(2 * (1 - pnorm(abs(overall_ATT_t_value))),3)  
  
  sig_stars = ''
  if(overall_ATT_p_value <= .05){
    sig_stars = '*'
  }
  subtitle = paste0('Overall ATT = ', round(overall_ATT,3), sig_stars,
                    ' (p-value = ', overall_ATT_p_value, ')')
  
  # plot 
  coefs <- bind_rows(stacked) 
  
  coef_min = MAX_WEEKS*-1 # the most min is the ref period thus anchored at 0
  coef_max = MAX_WEEKS
  
  plot <- coefs[t>coef_min & t<=coef_max] %>% 
    ggplot(aes(x = t, y = estimate, color = method)) + 
    geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 1) +
    geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), linewidth = 0.75) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
    geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
    scale_color_manual(name="Estimation Method", values= met.brewer("Cross", 8, "discrete")) +
    theme(legend.position= 'bottom') +
    labs(title = 'Event Time Estimates', y="ATT", x = "Relative Time", 
         subtitle = subtitle, 
         caption = '* indicates statistically significant at .05 significance level.') + 
    guides(col = guide_legend(nrow = 3))+ scale_x_continuous(breaks= sort(coefs$t))
  
  # Save plot
  ggsave(paste0(out_dir,outcome_variable,'_stacked_dynamic_max_period',MAX_WEEKS,'.png'))
  
  print(plot)
  
  # Simple model only with the treat step dummy
  model_formula_simple <- as.formula(paste(outcome_variable, "~ treat  | 
                                     unit^df + period^df"))
  
  model_simple <- feols(model_formula_simple, 
                        data = stacked_data, 
                        cluster = "bracket_df")
  
  print(summary(model_simple)) # seems like with sole sync dummy the estimate is the same as regular twfe. ~.02 and very sig. for both outcomes
  
  output <- list("stacked_treat" = stacked_treat, "plot" = plot, 'model' = model)
  saveRDS(model, paste0(out_dir, outcome_variable, '_stacked.rds'))
  return(output)
}

# Usage:
output_stacked <- run_model(dv, MAX_WEEKS)


# Put graphs next to each other
gg <- ggarrange(output_nplaylist$plot, output_sumfollow$plot,
                ncol=2, nrow=1, common.legend = TRUE, legend="bottom") 
gg
ggexport(gg,filename = paste0(out_dir,"reg_main_all_plots.png"))


# Weighted pverall ATT for nplaylist
# (weighted or not the estimates are really close so continue unweighted)
stacked_treat <- output_nplaylist$stacked_treat
obs_num_t <- c()
for(i in 0:(length(stacked_treat$t)-1)){
  N_t = unq_len(stacked_data[cohort_period != 10000 & time_to_treatment==i]$unit)
  obs_num_t <- c(obs_num_t, N_t)
}
obs_num_t<-(as.data.table(obs_num_t))

obs_num_t[, total_N := sum(obs_num_t)]
obs_num_t[, weight := obs_num_t/total_N]
stopifnot(sum(obs_num_t$weight)==1)

stacked_treat <-cbind(stacked_treat,obs_num_t[,c('weight')])
stacked_treat[, weighted_estimate :=estimate*weight]
stacked_treat[, weighted_var := (weight^2)*var]
overall_ATT_weighted = round(sum(stacked_treat$weighted_estimate),3)
# The SE for average of N r.v.'s = 1/N * sqrt(sum of variences of r.v.s)
overall_ATT_weighted_SE = sqrt(sum(stacked_treat$weighted_var))
overall_ATT_weighted_t_value = (overall_ATT_weighted/overall_ATT_weighted_SE)
overall_ATT_weighted_p_value <- round(2 * (1 - pnorm(abs(overall_ATT_weighted_t_value))),3)  # the abs() function takes the absolute value of the z-score

sig_stars = ''
if(overall_ATT_weighted_p_value <= .05){
  sig_stars = '*'
}
subtitle_weighted = paste0('Overall ATT = ', overall_ATT_weighted, sig_stars,
                           ' (p-value = ', overall_ATT_weighted_p_value, ')')


# Heterogeneity -----------------------------------------------------------
run_model_cov <- function(outcome_variable, MAX_WEEKS, cov) {
  model_formula <- as.formula(paste(outcome_variable, "~ sync_dummy +
                                      sync_dummy:",cov," | 
                                     unit^df + period^df"))
  
  model <- feols(model_formula, 
                 data = stacked_data, 
                 cluster = "bracket_df")
  print(summary(model))
  
  return(model)
}
# ps. for title pop. nontreat is NA
models_cov <- list()
dvs <- dv
covs <- c('n_playlist_inclusion_earliest','sumfollowers_earliest', 'song_age_yr','title_numVotes') # shouldn't be song age year and title num votes also be omitted as they are constant for each song.

for(dv in dvs){
  for(cov in covs){
    model <- run_model_cov(dv, MAX_WEEKS, cov)
    models_cov[[paste0(dv,'X',cov)]] <- model
  }
}

# Summarize these results in 1 - 2 tables
names(models_cov)

modelsummary(models_cov[1:3])

### Include all covs at once, except title num votes becuase that one can only be among treated. 
unique(stacked_data[treatment_group==1 & is.na(title_numVotes)]$title)
# We can analyze 
stacked_data[treatment_group==0, title_numVotes := 0]

run_model_cov_all <- function(outcome_variable, MAX_WEEKS,pop_metric) {
  
  # outcome_variable = "sumfollowers_log"
  # outcome_variable = "n_playlist_inclusion_log"
  model_formula <- as.formula(paste(outcome_variable, "~ sync_dummy +
                                      sync_dummy:",pop_metric," +
                                      sync_dummy:song_age_yr +
                                      sync_dummy:title_numVotes| 
                                      unit^df + period^df"))
  
  model <- feols(model_formula, 
                 data = stacked_data, 
                 cluster = "bracket_df")
  print(summary(model))
  
  return(model)
}


models_cov_all <- list()
dvs <- dv
pop_metrics <- c('n_playlist_inclusion_earliest','sumfollowers_earliest')

# Iterate over both dependent variables and population metrics
for(dv in dvs) {
  for(pop_metric in pop_metrics) {
    model <- run_model_cov_all(dv, MAX_WEEKS, pop_metric) # Assuming pop_metric is a single value
    models_cov_all[[paste0(dv,'X',pop_metric)]] <- model
  }
}

# Summarize these results in separate tables for each popularity metric
for(pop_metric in pop_metrics) {
  models_subset <- models_cov_all[names(models_cov_all) %like% (pop_metric)]
  for(fmt in c('txt','tex')) {
    modelsummary(models_subset, 
                 output = paste0(out_dir, 'reg_covs_all_', pop_metric, '.', fmt),
                 stars = T, title = paste0('\n Heterogeneity analysis for ', pop_metric),
                 notes = "Filled in 0 for nontreated title votes num. Num votes and followers are at 1M.")
  }
}




# Stackedev -------------------------------------------------------------
# A bit more simplislistic stacked DiD as this approach uses only never treated as the comparison
# That is why the code is so simple

# data: The data frame to use.
# dv: Dependent variable.
# iv: Independent variable(s).
# cohort: The cohort variable.
# time: The time variable.
# never_treat: Variable indicating units never treated.
# unit_fe: Unit fixed effects variable.
# clust_unit: Clustering variable.
# covariates: Other covariates.
# other_fe: Other fixed effects to include.
# interact_cov: Whether to interact covariates with the stack.


data <- dt
data[, never_treat := !(treatment_group)]
data[, cohort := (cohort_period)]
dv = 'hrs_listened'
iv = 'treat'
summary(data[never_treat==1]$treat)
time = 'period'
never_treat = 'never_treat'
unit_fe = 'unit'
clust_unit = 'unit'
covariates = NULL
other_fe = NULL

# Define the stacked event study function
stackedev <- function(data, dv, iv, cohort, time, never_treat, unit_fe, clust_unit, covariates = NULL, other_fe = NULL, interact_cov = FALSE) {
  
  # Check for never treated units
  if (!all(data[[never_treat]] %in% c(0, 1))) {
    stop("Error: Stacked event study requires never treated comparison units.")
  }
  
  # Creating stacks for each treated cohort of units
  t_vals <- unique(data[never_treat != 1]$cohort)
  
  # Initialize an empty list to store stack data.tables
  stack_list <- list()
  
  for (i in t_vals) {
    cat("**** Building Stack", i, "****\n")
    # Generate the stack for the current cohort
    stack_data <- data[cohort == i | never_treat == 1, ]
    stack_data[, stack := i]
    # Add to the list
    stack_list[[as.character(i)]] <- stack_data
  }
  
  # Appending together each stack
  cat("**** Appending Stacks ****\n")
  all_stacks <- rbindlist(stack_list, use.names = TRUE)
  
  # Creating variable to estimate unit by stack variances
  all_stacks[, unit_stack := stack * get(clust_unit)]
  
  # Allowing covariates to be interacted with stack
  if (interact_cov) {
    for (cov in covariates) {
      all_stacks[[cov]] <- all_stacks[[cov]] * all_stacks$stack
    }
  }
  
  # Estimating model with fixed effects
  cat("**** Estimating Model with feols (fixest package) ****\n")
  
  # Construct the formula string piece by piece
  formula_parts <- c(dv, "~", iv)
  if (!is.null(covariates)) {
    formula_parts <- c(formula_parts, "+", paste(covariates, collapse = " + "))
  }
  formula_parts <- c(formula_parts, "|", unit_fe, "##stack", "+", time, "##stack")
  if (!is.null(other_fe)) {
    formula_parts <- c(formula_parts, "+", paste(other_fe, collapse = " + "))
  }
  
  # Now create the formula
  fe_formula <- as.formula(paste(formula_parts, collapse = " "))
  
  
  feols_result <- feols(fe_formula, data = all_stacks, cluster = ~ unit_stack)
  print(summary(feols_result))
  
  # Clean up
  all_stacks[, c("stack", "unit_stack") := NULL]
  
  return(feols_result)
}


# Beep -------------
beep()