
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

# Create relative time dummy
dt[, time_since_treat := period-cohort_period]
dt[cohort_period>max(period), time_since_treat:= -999] # nontreated group
dt[, treatment_group:= ifelse(time_since_treat != -999, 1,0)] # nontreated group
sort(unique(dt$time_since_treat))

unique(dt[time_since_treat == -999]$cohort_period)


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
  labs(title = "True Treatment Effects by Cohort Period and Period")
grayscale_plot

ggsave(paste(out_dir, 'plot_true_te_by_cohort_period_bw.png'))

# Plot avg TE by time since treatment ------------------------------------------
# Calculate avg te by period
avg_treat_period[, time_since_treat := period-as.numeric(cohort_period)]
avg_te_rel_time <- avg_treat_period[, .(mean_treat_effect = mean(mean_treat_effect)), by = time_since_treat]

# overall avg
avg = round(mean(avg_te_rel_time$mean_treat_effect),2)
print(paste0('Overall avg TE: ', avg))

# Create the grayscale bar graph with adjusted bar width
ggplot(avg_te_rel_time, aes(x = time_since_treat, y = mean_treat_effect)) +
  geom_bar(stat = "identity", fill = "gray", width = 0.5) +  # Adjust bar width here
  geom_text(aes(label = round(mean_treat_effect, 2)), vjust = -0.3) +
  scale_x_continuous(breaks = avg_te_rel_time$time_since_treat) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Time Since Treatment", y = "Mean Treatment Effect", 
       title = "Mean Treatment Effect Over Time",
       subtitle = paste('Overall avg. = ', avg)) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(paste(out_dir, 'plot_true_te_by_rel_period_bw.png'))


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
# First, find if there are any periods where everyone is treated
# I.e., all the treat dummies equal to one and there is no treat dummy equal to 0 in this period
periods_w_untreated <- sort(unique(dt[treat==0]$period)) # periods with at least some untreated
all_periods <- c(sort(unique(dt$period))) 
periods_all_treated <- setdiff(all_periods, periods_w_untreated)
# If there are no periods where all treated, do nothing
# Otherwise take only the periods before all is treated
if(length(periods_all_treated)==0){
  dt_did <- dt
}else{
  dt_did <- dt[period < periods_all_treated] # select periods before all is treated 
}

# Create dummy variables
dt_did <- dt_did %>% 
  dummy_cols(select_columns = c("cohort_period", "period"))

# Regression
formula <- as.formula(paste0('hrs_listened ~ treat'))
model <- feols(formula,
               data = dt_did, panel.id = "unit",
               fixef = c("unit", "period"), cluster = "unit")
summary(model) 



# C) did package by Santanna & Callaway ----------------------------------------

dt_cs <- copy(dt)
dt_cs[treatment_group == 0, cohort_period := 0] # according to the pkg instructions

out <- att_gt(yname = "hrs_listened",
              gname = "cohort_period",
              idname = "unit",
              tname = "period",
              xformla = ~1,
              data = dt_cs,
              est_method = "reg",
              allow_unbalanced_panel = T,
              control_group = c("nevertreated", "notyettreated")
)
out
rel_period_effects <- aggte(out, type = "dynamic")
group_effects <- aggte(out, type = "group")
simple_effects <- aggte(out, type = "simple")
ggdid(out)

# Convert result into df
ref_periods <- c(-2,-999) 
# TODO: make other methods also take -4 as the ref OR simply use +/-2 weeks around treatment
MAX_WEEKS = 2

CS <- rel_period_effects %>% 
  tidy() %>% 
  rename(t = event.time) %>% 
  filter(t > -MAX_WEEKS & t <= MAX_WEEKS) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = ref_periods[1], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  bind_rows(tibble(t = ref_periods[2], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(method = "CS")
write.csv(CS,paste0(out_dir, 'mod_CS_df.csv'))

# Why the estimates are slightly different? 
  # est procedure & randomness?
  # Also did package reports simult. conf band. 
  # using higher critical values to account for simultanous testing
  # Thus the diff in the significance in addition to diff in the SEs

# D) ETWFE       ---------------------------------------------------------------
dt[, time_since_treat_min1 := as.integer(time_since_treat==-1)]

mod_etwfe =
  etwfe(
    fml  = hrs_listened ~ time_since_treat_min1, # outcome ~ controls
    tvar = period,        # time variable
    gvar = cohort_period, # group variable
    data = dt,       # dataset
    vcov = ~unit  # vcov adjustment (here: clustered)
  )
summary(mod_etwfe)
mod_etwfe$collin.var

# Put the results into a DF
ref_periods = c(-999,-2)
results <- broom::tidy(mod_etwfe, conf.int = TRUE) %>% 
  mutate(t = as.numeric(sub(".*::(.):period::(.*).*", "\\2", term)) -
           as.numeric(sub(".*cohort_period::(.):.*", "\\1", term))) %>%
  select(-term) %>%
  relocate(t, .before = estimate) %>%
  # filter(t >= -1*max_t & t <= max_t) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = ref_periods[1], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  bind_rows(tibble(t = ref_periods[2], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(method = 'ETWFE')

# Write to CSV
write.csv(results, paste0(out_dir,'mod_result', '_ETWFE.csv'))

# TODO: do we need to try hand written one to see if we can have -1 period?

# E) Stacked DiD ---------------------------------------------------------------
head(dt)

# Simple TWFE --------------------------------------------------------------
# Define reference period: the most negative period
MAX_WEEKS = 2 # indicates the time window around treatment
sort(unique(dt$time_since_treat))
dt <- dt[(treatment_group==0) | (time_since_treat>=-MAX_WEEKS & time_since_treat<=MAX_WEEKS)]

# there are no untreated group so not c(REF_PERIOD, -999), ie. ref period the most neg and also untreated
REF_PERIODS = c(-999, -2)

run_twfe <- function(dv){
  formula <- as.formula(paste0(dv,"~ i(time_since_treat, ref = REF_PERIODS)")) 
  twfe_ols <- feols(formula, data = dt, panel.id = "unit",
                    cluster = "unit", fixef = c("unit", "period"))
  print(summary(twfe_ols))
  saveRDS(twfe_ols, paste0(out_dir, dv, '_twfe.rds'))
  
  # Save results as df
  export_reg_as_df(twfe_ols, dv, out_dir,method = 'twfe', ref_periods = REF_PERIODS)
}

mod_twfe_df <- data.table(run_twfe(dv = 'hrs_listened'))


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


# Stacked DiD regressions

outcome_variable = 'hrs_listened'
REF_PERIODS = c(-2, -999)

run_stacked_did_simple <- function(outcome_variable) {
  # Define the model formula
  model_formula_simple <- as.formula(paste(outcome_variable, "~ treat | unit^df + period^df"))
  
  # Fit the model using the 'feols' function from the 'fixest' package
  model_simple <- feols(model_formula_simple, data = stacked_data, cluster = "bracket_df")
  
  # Print the summary of the model
  print(summary(model_simple)) 
  return(model_simple)
}

mod_stacked_simple <- run_stacked_did_simple(outcome_variable)

# Model function
run_stacked_did <- function(outcome_variable, MAX_WEEKS, REF_PERIODS) {
  # outcome_variable = 'hrs_listened'
  # REF_PERIODS = c(-2, -999)
  
  # Model
  model_formula <- as.formula(paste(outcome_variable, "~ i(time_since_treat, ref = REF_PERIODS) | 
                                     unit^df + period^df"))
  
  model <- feols(model_formula, 
                 data = stacked_data, 
                 cluster = "bracket_df")
  print(summary(model))
  return(model)
}

mod_stacked <- run_stacked_did(outcome_variable, MAX_WEEKS, REF_PERIODS)

# Prepare and write model results to CSV
save_model_results <- function(model, MAX_WEEKS, REF_PERIODS, out_dir) {
  # Process results
  stacked <- broom::tidy(model,conf.int = TRUE) %>% 
    mutate(t =  as.double(str_replace(term, "time_since_treat::", ""))) %>% 
    filter(t >= -1*MAX_WEEKS & t <= MAX_WEEKS) %>% 
    select(t, estimate, conf.low, conf.high) %>% 
    bind_rows(tibble(t = REF_PERIODS[1], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    bind_rows(tibble(t = REF_PERIODS[2], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    mutate(method = "stacked")
  stacked <- as.data.table(stacked)
  write.csv(stacked,paste0(out_dir,'mod_result', '_stacked.csv'))
  print(stacked)
  return(stacked)
}

mod_stacked_df <- save_model_results(model=mod_stacked, MAX_WEEKS, REF_PERIODS, out_dir)


# Generate plot from coefficients
generate_stacked_plot <- function(mod_stacked_df, MAX_WEEKS, outcome_variable, out_dir) {
  # Plotting logic
  
  # Regular avg. TE
  stacked_treat <- mod_stacked_df[t>=0]
  stacked_treat[, SE := (conf.high - estimate)/1.96] # given .95 conf level
  stacked_treat[, var := SE^2]
  overall_ATT = sum(stacked_treat$estimate)/length(stacked_treat$estimate)
  overall_ATT_SE = (1/length(stacked_treat$estimate))*sqrt(sum(stacked_treat$var))
  overall_ATT_t_value = overall_ATT/overall_ATT_SE
  overall_ATT_p_value <- round(2 * (1 - pnorm(abs(overall_ATT_t_value))),3)  
  
  subtitle = paste0('Overall ATT = ', round(overall_ATT,3), 
                    generate_significance_stars(overall_ATT_p_value),
                    ' (p-value = ', overall_ATT_p_value, ')')
  
  # plot 
  coefs <- bind_rows(mod_stacked_df) 
  
  # coef_min = MAX_WEEKS*-1 # the most min is the ref period thus anchored at 0
  # coef_max = MAX_WEEKS
  
  plot <- coefs[t>=-MAX_WEEKS & t<=MAX_WEEKS] %>% 
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
    scale_x_continuous(breaks= sort(coefs$t))
  
  print(plot)
  
  # Save plot
  ggsave(paste0(out_dir,'_stacked_dynamic_max_period',MAX_WEEKS,'.png'))
  
  return(plot)
}
stacked_dyn_plot <- generate_plot(generate_stacked_plot, MAX_WEEKS, outcome_variable, out_dir)

# Beep -------------
beep()