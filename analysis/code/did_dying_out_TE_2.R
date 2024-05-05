
# Simulation study for the DiD article: 
# TODO: change the dv from hrs listened to dv or outcome var, sth generic

rm(list = ls())
source('../../simulation/code/sim_data_dying_out_TE_2.R') # Import simulation function and some utilities
source('common_def_func.R') # Import libraries and common functions

dt <- sim_data()

unique(dt$cohort_period)
unique(dt$period)
# dt <- dt[] # exclude period where everyone is treated so that we have an untreated group

# EDA and Analysis --------------------------------------------------------
# Check out the data

select_cols <- c('unit', 'period', 'cohort_period','treat','dep_var')

kable(head(dt[, ..select_cols]), 'simple')

kable(summary(dt[, ..select_cols]), 'simple')

dt[, .N, by = cohort_period] # group sizes
dt[, .(mean_dep_var = mean(dep_var)), by = cohort_period]

# Create relative time dummy
dt[, time_since_treat := period-cohort_period]
dt[cohort_period>max(period), time_since_treat:= -999] # nontreated group
dt[, treatment_group:= ifelse(time_since_treat != -999, 1,0)] # nontreated group
dt[treatment_group == 0, cohort_period := 999]
sort(unique(dt$time_since_treat))

unique(dt[time_since_treat == -999]$cohort_period)

unq_rel_t <- sort(unique(dt$time_since_treat))
MAX_WEEKS = max(setdiff(unq_rel_t,-999))

# Find number of units per time since treat
n_by_time_since_treat <- dt[time_since_treat>0, .N, by = time_since_treat]
tot_N <- sum(n_by_time_since_treat$N)
n_by_time_since_treat[, prop := N/tot_N]

# Based on the existence of untreated group decide control group and ref periods
there_is_untreated = (dt[cohort_period == 999, .N] > 0)

if(there_is_untreated){
  control_group = c("nevertreated","notyettreated") # CS
  
  most_neg_t <- min(setdiff(unq_rel_t,-999))
  ref_periods <- c(most_neg_t,-999) # TODO: change this into the most min one?
  
}else{
  control_group = c("notyettreated")
  ref_periods <- unq_rel_t[1:2] # the most two negative
  }



# Visualize the outcome variable -----------------------------------------------
avg_dv_period <- dt[, .(mean_dep_var = mean(dep_var)), by = c('cohort_period','period')] 

# Convert 'cohort_period' to a factor to have discrete color scale in the plot
avg_dv_period[, cohort_period := as.factor(cohort_period)]

# Plot
ggplot(avg_dv_period, aes(x = period, y = mean_dep_var, group = cohort_period, color = cohort_period)) +
  geom_line() +
  geom_vline(data = avg_dv_period[cohort_period == period], 
             aes(xintercept = period), linetype = "dashed") +
  scale_color_discrete(name = "Cohort-period") +
  labs(x = "Period", # title = "Trends in Mean Hours Listened by Cohort Period",
       y = "Outcome variable") +
  scale_x_continuous(breaks = sort(unique(avg_dv_period$period)))+
  my_theme() 
ggsave(paste0(out_dir, 'outcome_by_cohort_period.png'))


# Black and white friendly
# Define the linetypes and shapes based on the number of unique cohort_periods
line_types <- c("solid", "dashed", "dotted", "twodash", "longdash")
shapes <- c(16, 17, 18, 19, 15)  # Filled shapes
num_cohorts <- length(unique(avg_dv_period$cohort_period))
line_types <- line_types[1:num_cohorts]
shapes <- shapes[1:num_cohorts]

# Plot
gg <- ggplot(avg_dv_period, aes(x = period, y = mean_dep_var, group = cohort_period)) +
  geom_line(aes(linetype = cohort_period)) +
  geom_point(aes(shape = cohort_period)) +
  geom_vline(data = avg_dv_period[cohort_period == period], aes(xintercept = period, linetype = cohort_period), color = "black") +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  labs( # title = "Trends in Mean Hours Listened by Cohort Period",
       x = "Period",
       y = "Outcome variable",
       linetype = "Cohort-period",
       shape = "Cohort-period") +
  scale_x_continuous(breaks = sort(unique(avg_dv_period$period)))+
  my_theme() 

# Set color blindness-friendly palette for digital version and print
gg <- gg + scale_color_brewer(palette = "Dark2", name = "Cohort-period")

print(gg)
ggsave(paste0(out_dir, 'outcome_by_cohort_period_bw.png'))


# Plot true treatment effects --------------------------------------------------
avg_treat_period <- dt[treat == 1, .(mean_treat_effect = mean(tau_cum)), by = c('cohort_period','period')]

# Color plot
plot_te <- ggplot(avg_treat_period, aes(fill=factor(cohort_period), y=mean_treat_effect, x=period)) + 
  scale_fill_brewer(palette = "Set1") + # Color palette
  geom_bar(position=position_dodge2(preserve = "single"), stat="identity") +  
  labs(x = "Period", y = "Treatment effect", 
       # title = 'True treatment effects (hrs)',
       # subtitle = 'Treatment effect heterogeneity across cohorts'
       ) + 
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  guides(fill=guide_legend(title="Cohort-period")) + 
  scale_x_continuous(breaks = unique(avg_treat_period$period)) + 
  # scale_y_continuous(breaks = round(unique(avg_treat_period$mean_treat_effect))) + 
  my_theme()
plot_te

ggsave(paste0(out_dir, 'true_te_by_cohort_period.png'))

# Create a grayscale plot
grayscale_plot <- plot_te +
  scale_fill_grey(start = 0.8, end = 0.2) # + # Grayscale fill
  # labs(title = "True Treatment Effects by Cohort Period and Period")
grayscale_plot

ggsave(paste0(out_dir, 'true_te_by_cohort_period_bw.png'))

# Plot avg TE by time since treatment ------------------------------------------
# Calculate avg te by period
avg_treat_period[, time_since_treat := period-as.numeric(cohort_period)]
avg_te_rel_time <- avg_treat_period[, .(mean_treat_effect = mean(mean_treat_effect)), by = time_since_treat]

true_te_avg = mean(dt[treat == 1]$tau_cum)

# Find avg per cohort
true_te_cohort <- avg_treat_period[, .(mean_te_cohort = mean(mean_treat_effect)), by = cohort_period]
mean(true_te_cohort$mean_te_cohort)

# overall avg
true_te_avg_agg_rel = round(mean(avg_te_rel_time$mean_treat_effect),2)
print(paste0('Overall avg TE: ', true_te_avg))

# Create the grayscale bar graph with adjusted bar width
ggplot(avg_te_rel_time, aes(x = time_since_treat, y = mean_treat_effect)) +
  geom_bar(stat = "identity", fill = "gray", width = 0.5) +  # Adjust bar width here
  geom_text(aes(label = round(mean_treat_effect, 2)), vjust = -0.3) +
  scale_x_continuous(breaks = avg_te_rel_time$time_since_treat) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Time Since Treatment", y = "Avg. Treatment Effect" #, 
       # title = "Mean Treatment Effect Over Time",
       # subtitle = paste0('Overall avg. = ', true_te_avg)
        ) +
  my_theme() +
  theme(legend.position = "none")

ggsave(paste0(out_dir, 'true_te_by_rel_period_bw.png'))


# A) Canonical DiD -------------------------------------------------------------
formula <- as.formula('dep_var ~ treat')
canonical_did <- feols(formula,
                       data = dt, panel.id = "unit",
                       fixef = c("unit", "period"), cluster = "unit")
summary(canonical_did) 
simple_twfe_avg = canonical_did$coefficients
modelsummary(canonical_did)

# Assuming 'canonical_did' is your model object from 'feols'
modelsummary(canonical_did,
             title = "Model Results",
             output = "latex",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01))

msummary(canonical_did,
         output = paste0(out_dir,"canonical_did.tex"),
         stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
         fmt = 2)


# Bacon Decomposition
# bacon_decomp <- bacon(formula, dt, id_var="unit", time_var='period', quietly = F)
# summary(bacon_decomp)
# bacon_decomp_avg <- sum(bacon_decomp$weight * bacon_decomp$estimate)
# 
# # Illustrate one of the estimates. 
# # treated : 3, control: 2, est = -2.76, weight 0.03 Later vs Earlier Treated
# # group 3 is treated in time 3. so we look at periods 2 vs 3
# treat_bef = mean(dt[cohort_period==3 & period==2]$dep_var)
# treat_aft = mean(dt[cohort_period==3 & period>=3]$dep_var)
# control_bef = mean(dt[cohort_period==2 & period==2]$dep_var)
# control_aft = mean(dt[cohort_period==2 & period>=3]$dep_var)
# treat_diff = treat_aft- treat_bef
# control_diff = control_aft- control_bef
# treat_diff-control_diff
# 
# 
# ggplot(bacon_decomp) +
#   aes(x = weight, y = estimate, shape = factor(type)) +
#   geom_point(size = 2) +
#   geom_hline(yintercept = 0) + 
#   theme_minimal()  +
#   labs(x = "Weight", y = "Estimate", shape = "Type")
# ggsave(paste0(out_dir, 'bacon_decomp.png'))
# 
# # Assuming bacon_decomp is loaded in your R session
# library(xtable)
# bacon_decomp <- data.table(bacon_decomp)
# bacon_decomp[, treated:=as.integer(treated)]
# bacon_decomp[, untreated:=as.integer(untreated)]
# bacon_decomp[, estimate:=round(estimate,2)]
# bacon_decomp[, weight:=round(weight,2)]
# setnames(bacon_decomp, colnames(bacon_decomp), tools::toTitleCase(colnames(bacon_decomp)))
# # Convert the data frame to a LaTeX table
# latex_table <- xtable(bacon_decomp)
# 
# # Print the LaTeX code for the table
# print(latex_table, include.rownames = FALSE, 
#       hline.after = c(-1, 0), # Horizontal lines at the top of the table
#       booktabs = TRUE) # Use booktabs style for a more professional look

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
formula <- as.formula(paste0('dep_var ~ treat'))
model <- feols(formula,
               data = dt_did, panel.id = "unit",
               fixef = c("unit", "period"), cluster = "unit")
summary(model) 
model$coefficients


# C) did package by Santanna & Callaway ----------------------------------------

dt_cs <- copy(dt)
dt_cs[treatment_group == 0, cohort_period := 0] # according to the pkg instructions

out <- att_gt(yname = "dep_var",
              gname = "cohort_period",
              idname = "unit",
              tname = "period",
              xformla = ~1,
              data = dt_cs,
              est_method = "reg",
              allow_unbalanced_panel = T,
              control_group = control_group
)
out
cs_rel_period_effects <- aggte(out, type = "dynamic")
cs_group_effects <- aggte(out, type = "group")
cs_overall_avg  <- aggte(out, type = "simple")
ggdid(cs_rel_period_effects)

# Convert result into df
mod_cs_df <- cs_rel_period_effects %>% 
  tidy() %>% 
  rename(t = event.time) %>% 
  filter(t > -MAX_WEEKS & t <= MAX_WEEKS) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = ref_periods[1], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  bind_rows(tibble(t = ref_periods[2], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(method = "CS")
write.csv(mod_cs_df,paste0(out_dir, 'mod_CS_df.csv'))

# Why the estimates are slightly different? 
  # est procedure & randomness?
  # Also did package reports simult. conf band. 
  # using higher critical values to account for simultanous testing
  # Thus the diff in the significance in addition to diff in the SEs

# D) ETWFE       ---------------------------------------------------------------
dt[, time_since_treat_min1 := as.integer(time_since_treat==-1)]

mod_etwfe_pkg =
  etwfe(
    fml  = dep_var ~ 1, # outcome ~ controls
    tvar = period,        # time variable
    gvar = cohort_period, # group variable
    data = dt,       # dataset
    vcov = ~unit  # vcov adjustment (here: clustered)
  )
summary(mod_etwfe_pkg)
# can't do pre periods cause its trying to * w cohort and period dummies. 

# Put the results into a DF
mod_etwfe_pkg_df <- broom::tidy(mod_etwfe_pkg, conf.int = TRUE) %>% 
  mutate(t = as.numeric(sub(".*::(.):period::(.*).*", "\\2", term)) -
           as.numeric(sub(".*cohort_period::(.):.*", "\\1", term))) %>%
  select(-term) %>%
  relocate(t, .before = estimate) %>%
  # filter(t >= -1*max_t & t <= max_t) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = ref_periods[1], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  bind_rows(tibble(t = ref_periods[2], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(method = 'ETWFE')

mod_etwfe_pkg_avg_df <- mod_etwfe_pkg_df %>%
  group_by(t, method) %>%
  summarise(
    estimate = mean(estimate),
    conf.low = mean(conf.low),
    conf.high = mean(conf.high),
    .groups = 'drop' # This drops the grouping structure afterwards
  )

# View the results
mod_etwfe_pkg_avg_df <- data.table(mod_etwfe_pkg_avg_df)
print((mod_etwfe_pkg_df))
# Write to CSV
write.csv(mod_etwfe_pkg_avg_df, paste0(out_dir,'mod_result', '_ETWFE_pkg.csv'))

# E) Stacked DiD ---------------------------------------------------------------
head(dt)

# Simple TWFE --------------------------------------------------------------
# Define reference period: the most negative period
MAX_WEEKS # indicates the time window around treatment
sort(unique(dt$time_since_treat))
dt <- dt[(treatment_group==0) | (time_since_treat>=-MAX_WEEKS & time_since_treat<=MAX_WEEKS)]

# there are no untreated group so not c(REF_PERIOD, -999), ie. ref period the most neg and also untreated
ref_periods

run_twfe <- function(dv){
  formula <- as.formula(paste0(dv,"~ i(time_since_treat, ref = ref_periods)")) 
  twfe_ols <- feols(formula, data = dt, panel.id = "unit",
                    cluster = "unit", fixef = c("unit", "period"))
  print(summary(twfe_ols))
  saveRDS(twfe_ols, paste0(out_dir, dv, '_twfe.rds'))
  
  # Save results as df
  export_reg_as_df(twfe_ols, dv, out_dir,method = 'TWFE', ref_periods = ref_periods)
}

mod_twfe_df <- data.table(run_twfe(dv = 'dep_var'))


# Create stacked data -----------------------------------------------------
### for stacking
groups <- unique(dt[treatment_group == 1]$cohort_period)

sort(groups)
sort(unique(dt$period))

MAX_WEEKS # indicates the time window around treatment

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
    mutate(time_since_treat = if_else(cohort_period == i, time_since_treat, -999)) # TODO: check this. why don't we do -999 only for the untreated
}
stacked_data <- map_df(groups, getdata) %>% 
  mutate(bracket_df = paste(unit,df))

stacked_data<- as.data.table(stacked_data)
head(stacked_data[df==2,c('unit','period','cohort_period', 'time_since_treat','df','bracket_df')],100)
summary(stacked_data$time_since_treat)
summary(stacked_data$time_since_treat)

sort(unique(stacked_data$cohort_period))
summary(stacked_data[cohort_period==999]$time_since_treat) # for the untreated


# Stacked DiD regressions

outcome_variable = 'dep_var'
ref_periods

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
stacked_simple_avg_te <- mod_stacked_simple$coefficients

# Stacked requires an untreated group
# Error: in feols(model_formula_simple, data = stacked_data, ...:
# The only variable 'treat' is collinear with the fixed effects. In such
# circumstances, the estimation is void.

# Model function
run_stacked_did <- function(outcome_variable, MAX_WEEKS, ref_periods) {
  # outcome_variable = 'dep_var'
  # ref_periods = c(-2, -999)
  
  # Model
  model_formula <- as.formula(paste(outcome_variable, "~ i(time_since_treat, ref = ref_periods) | 
                                     unit^df + period^df"))
  
  model <- feols(model_formula, 
                 data = stacked_data, 
                 cluster = "bracket_df")
  print(summary(model))
  return(model)
}

mod_stacked <- run_stacked_did(outcome_variable, MAX_WEEKS, ref_periods)

# Prepare and write model results to CSV
save_model_results <- function(model, MAX_WEEKS, ref_periods, out_dir) {
  # Process results
  stacked <- broom::tidy(model,conf.int = TRUE) %>% 
    mutate(t =  as.double(str_replace(term, "time_since_treat::", ""))) %>% 
    filter(t >= -1*MAX_WEEKS & t <= MAX_WEEKS) %>% 
    select(t, estimate, conf.low, conf.high) %>% 
    bind_rows(tibble(t = ref_periods[1], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    bind_rows(tibble(t = ref_periods[2], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    mutate(method = "Stacked")
  stacked <- as.data.table(stacked)
  write.csv(stacked,paste0(out_dir,'mod_result', '_stacked.csv'))
  print(stacked)
  return(stacked)
}

mod_stacked_df <- save_model_results(model=mod_stacked, MAX_WEEKS, ref_periods, out_dir)

# Regular avg. TE
mod_stacked_df_N <- mod_stacked_df[t>=0]
mod_stacked_df_N <- merge(mod_stacked_df_N, n_by_time_since_treat, by.x = 't', by.y = 'time_since_treat')
mod_stacked_df_N[, estimate_prop:= prop*estimate]
stacked_avg_te <- sum(mod_stacked_df_N$estimate_prop)
mean(mod_stacked_df[t>=0]$estimate)

mod_stacked_df_N[, SE_prop := prop*((conf.high - estimate)/1.96)] # given .95 conf level
mod_stacked_df_N[, var_prop := SE_prop^2]
stacked_avg_te_SE = (1/nrow(mod_stacked_df_N))*sqrt(sum(mod_stacked_df_N$var_prop))
overall_ATT_t_value = stacked_avg_te/stacked_avg_te_SE
overall_ATT_p_value <- round(2 * (1 - pnorm(abs(overall_ATT_t_value))),3)  

subtitle = paste0('Overall ATT for stacked DiD = ', round(stacked_avg_te,3), 
                  generate_significance_stars(overall_ATT_p_value),
                  ' (p-value = ', overall_ATT_p_value, ')')

print(subtitle)


# Manual ETWFE ------------------------------------------------------------
# DiD ETWFE ---------------------------------------------------------------
# Do a weighed regression using Inversed Propensity Scores as weight
dt[,time_since_treat_min1:= NULL]
# Make dummy variables for each cohort period and post period to use in the regression
make_dummies <- function(dt){
  dum <- as.data.table(dummy_cols(dt, select_columns = c("treat","cohort_period","time_since_treat")))
  head(dum)
  
  periods <- sort(unique(dt$period))
  t <- periods[1:length(periods)]
  myfun <- function(t,period) as.numeric(period == t)
  tmp <- dt[, lapply(t, myfun, period)]
  setnames(tmp, c(paste0("t_",t)))
  dummied <- cbind(dum, tmp)
  return(dummied)
}

dt_dummied <- make_dummies(dt)
colnames(dt_dummied)

# Make regression formula
sort(unique(dt_dummied$time_since_treat))
sort(unique(dt_dummied$cohort_period))

cols <- colnames(dt_dummied)
cols
time_since_treat_cols <- cols[grepl('time_since_treat_',cols)]
cohort_period_cols <- cols[grepl('cohort_period_',cols)]

# Filter out the '999' values from both sets of columns
filtered_time_since_treat_cols <- time_since_treat_cols[!grepl('999', time_since_treat_cols)]
filtered_cohort_period_cols <- cohort_period_cols[!grepl('999', cohort_period_cols)]

# Convert the suffixes to numeric values, excluding '-999'
time_since_treat_values <- as.numeric(sub("time_since_treat_", "", filtered_time_since_treat_cols))
time_since_treat_values <- time_since_treat_values[time_since_treat_values != -999]

# Sort the values and exclude the minimum three
sorted_values <- sort(time_since_treat_values)
excluded_values <- head(sorted_values, 5)

# Remove the minimum three time_since_treat values
filtered_time_since_treat_cols <- filtered_time_since_treat_cols[!as.numeric(sub("time_since_treat_", "", filtered_time_since_treat_cols)) %in% excluded_values]

# Add backticks around time_since_treat variables
filtered_time_since_treat_cols <- paste0("`", filtered_time_since_treat_cols, "`")

# Create all combinations of the filtered cohort_period and time_since_treat columns
covariate_combinations <- expand.grid(filtered_cohort_period_cols, filtered_time_since_treat_cols)
covariate_combinations <- apply(covariate_combinations, 1, function(x) paste(x[1], x[2], sep = ":"))

# Create the regression formula
dv <- "dep_var"  # Replace with your actual dependent variable
formula <- as.formula(paste(dv, "~", paste(covariate_combinations, collapse = " + ")))

# Output the formula
formula
lm(formula, dt_dummied)

# OLS regression for log duration
reg_etwfe_ols <- function(formula, dt){
  etwfe <- feols(formula,
                 data = dt, panel.id = "unit",
                 cluster = "unit", fixef = c("cohort_period", "period"))
  return(etwfe)
}

etwfe_manual <- reg_etwfe_ols(formula,dt_dummied)
etwfe_manual$collin.var
# Need to be careful about the collinear variables when including - periods
summary(etwfe_manual)
etwfe_manual_tidy <- broom::tidy(etwfe_manual, conf.int = TRUE)%>%
  select(-std.error, -statistic,-p.value)
etwfe_manual_tidy


etwfe_manual_tidy <- etwfe_manual_tidy %>%
  mutate(
    cohort_period_num = as.numeric(str_extract(term, "(?<=cohort_period_)\\d+")),
    time_since_treat_num = as.numeric(str_extract(term, "(?<=time_since_treat_)[-\\d]+"))
  )

etwfe_manual_tidy <- data.table(etwfe_manual_tidy)
etwfe_manual_tidy[, term:= NULL]
etwfe_manual_tidy[, cohort_period_num:= NULL] # TODO: seems like the last period is taken as reference or sth
setnames(etwfe_manual_tidy, 'time_since_treat_num', 't')
mod_etwfe_manual_df <- etwfe_manual_tidy %>%
  group_by(t) %>%
  summarise(
    estimate = mean(estimate),
    conf.low = mean(conf.low),
    conf.high = mean(conf.high),
    .groups = 'drop' # This drops the grouping structure afterwards
  )
mod_etwfe_manual_df <- data.table(mod_etwfe_manual_df)
mod_etwfe_manual_df$t
mod_etwfe_manual_df[, method:= 'ETWFE_manual']
mod_cs_df$t

  

# Graph all models --------------------------------------------------------
# Put all the results df's together
mod_all_df <- rbind(mod_twfe_df,mod_etwfe_pkg_avg_df,mod_etwfe_manual_df, mod_stacked_df, mod_cs_df)
mod_all_df <- mod_all_df[t!=-999]
fwrite(mod_all_df, paste0(out_dir,'all_mod_df.csv'))

# True effects
mod_tru_df <- copy(avg_te_rel_time)
setnames(mod_tru_df, c('time_since_treat','mean_treat_effect'), c('t','estimate'))
# mod_tru_df[, t := t-1]
colnames(mod_all_df)
unq_t <- sort(unique(mod_all_df$t))
unq_t
sort(unique(mod_tru_df$t))
t_add <- setdiff(unq_t,sort(unique(mod_tru_df$t)))

mod_tru_df <- rbind(mod_tru_df, data.table(t= t_add, estimate = 0))
# "conf.low"  "conf.high" "method"
mod_tru_df[, conf.low := 0L]
mod_tru_df[, conf.high := 0L]
mod_tru_df[, method := 'True effect']
mod_all_df <- rbind(mod_all_df,mod_tru_df)

# Set estimates to 0 for negative periods for ETWFE method
# Set this to TRUE if you want to add negative periods for ETWFE, or FALSE if not
add_negative_periods_for_etwfe <- TRUE

if (add_negative_periods_for_etwfe) {
  # Find the unique negative periods from other methods
  negative_periods <- unique(mod_all_df[t < 0 & method != "ETWFE", t])
  
  # Create a data.table with the missing negative periods for ETWFE
  missing_etwfe_data <- data.table(
    t = negative_periods,
    estimate = 0,
    conf.low = 0,
    conf.high = 0,
    method = "ETWFE"
  )
  
  # Combine the missing ETWFE data with the original data
  mod_all_df <- rbindlist(list(mod_all_df, missing_etwfe_data), fill = TRUE)
  
  # Order the data for plotting
  setorder(mod_all_df, method, t)
}

# TWFE
simple_twfe_avg <- canonical_did$coefficients[1]
simple_twfe_se <- canonical_did$se[1]
ci_lower = simple_twfe_avg - 1.96 * simple_twfe_se
ci_upper = simple_twfe_avg + 1.96 * simple_twfe_se

min_x <- min(mod_all_df$t)
max_x <- max(mod_all_df$t)

plot <- unique(mod_all_df[method!='ETWFE_manual' & method!='TWFE'], by = c('method','t')) %>%
  ggplot(aes(x = t, y = estimate, color = method, shape = method)) + 
  geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 2) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), size = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", size = .25) +
  scale_color_brewer(name="Estimation Method", palette="Set1") + # Choose a color palette
  scale_shape_manual(name="Estimation Method", values = 1:6) + # Change the shape for different groups
  theme(legend.position= 'bottom',  
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#f0f0f0", linetype = 1), # Adds major grid lines
        panel.grid.minor = element_blank(), # Adds minor grid lines
        axis.line = element_line(color = "gray"),
        text = element_text(size = 16)) +
  labs(y="Estimate", x = "Period since treatment") + 
  guides(color = guide_legend(nrow = 3), shape = guide_legend(nrow = 3)) +
  scale_x_continuous(breaks = -MAX_WEEKS:MAX_WEEKS, limits = c(min_x-.4, (max_x+.5))) + # 
  my_theme()

print(plot)  

ribbon_subset <- unique(mod_all_df[(t >= 0 & t <= 7) & method == 'TWFE', ], by = 't')


plot +
  # Replace geom_hline with geom_segment
  geom_segment(data = ribbon_subset, aes(x = -.5, y = simple_twfe_avg, xend = 7.3, yend = simple_twfe_avg), 
               linetype = "dashed", color = "red", show.legend = FALSE) +
  geom_ribbon(data = ribbon_subset,aes(ymin = ci_lower, ymax = ci_upper), 
              fill = "red", alpha = 0.05, color = NA, show.legend = FALSE) +
  # Modify annotate to position the label correctly
  annotate("text", x = 7, y = simple_twfe_avg, label = "TWFE", 
           hjust = 1.1, vjust = 1.5, color = "red", show.legend = FALSE)


plot +
  # Add horizontal lines
  # geom_hline(yintercept = true_te_avg, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = simple_twfe_avg, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "red", alpha = 0.05, color = NA) + 
  # Add text labels for the horizontal lines
  # annotate("text", x = Inf, y = true_te_avg, label = "True avg te", hjust = 1.1, vjust = 1.5, color = "blue") +
  annotate("text", x = Inf, y = simple_twfe_avg, label = "TWFE", hjust = 1.1, vjust = 1.5, color = "red")
ggsave(paste0(out_dir,'all_mod.png'))



# Add avg: cs_overall_avg, true_te_avg, simple_twfe_avg, stacked_simple_avg_te,
# stacked_avg_te, stacked_avg_te

# Dynamic TWFE
mod_twfe_df_N <- mod_twfe_df[t>=0]
mod_twfe_df_N <- merge(mod_twfe_df_N, n_by_time_since_treat, by.x = 't', by.y = 'time_since_treat')
mod_twfe_df_N[, estimate_prop:= prop*estimate]
twfe_dyn_avg_te <- sum(mod_twfe_df_N$estimate_prop)

mod_twfe_df_N[, SE_prop := prop*((conf.high - estimate)/1.96)] # given .95 conf level
mod_twfe_df_N[, var_prop := SE_prop^2]
twfe_dyn_avg_te_SE = (1/nrow(mod_twfe_df_N))*sqrt(sum(mod_twfe_df_N$var_prop))
twfe_dyn_avg_te_t_value = twfe_dyn_avg_te/twfe_dyn_avg_te_SE
twfe_dyn_avg_te_p_value <- round(2 * (1 - pnorm(abs(twfe_dyn_avg_te_t_value))),3)  

# CS
cs_overall_ATT <- cs_overall_avg$overall.att
cs_overall_se <- cs_overall_avg$overall.se

# Stacked
stacked_avg_te
stacked_avg_te_SE

# ETWFE
etwfe_avg_te <- emfx(mod_etwfe_pkg,type = c("simple"))
etwfe_overall_ATT <- etwfe_avg_te$estimate
etwfe_overall_se <- etwfe_avg_te$std.error

library(forcats)

# Create a data frame with your values
values <- data.frame(
  method = factor(c("CS", "True Effect", "TWFE", "Dynamic TWFE", 'ETWFE', "Stacked"),
                  levels = c("True Effect", "TWFE", "Dynamic TWFE", "CS", "Stacked", "ETWFE")),
  value = c(cs_overall_ATT, true_te_avg, simple_twfe_avg, twfe_dyn_avg_te, etwfe_overall_ATT, stacked_avg_te),  # Replace with actual values
  se = c(cs_overall_se, 0, simple_twfe_se, twfe_dyn_avg_te_SE, etwfe_overall_se, stacked_avg_te_SE) # Replace with actual SE values
)

# Reorder the factor levels for 'method'
values$method <- fct_relevel(values$method, "True Effect", "TWFE", "Dynamic TWFE", "CS", "Stacked", "ETWFE")

values <- as.data.table(values)

# Create the bar plot with error bars and add the text on top
plot <- ggplot(values[method!='Dynamic TWFE'], aes(x = method, y = value, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), width = .2) +
  geom_text(aes(label = round(value, 2)), vjust = -3, color = "black", size = 5) +  # Adjust 'vjust' and 'size' as needed
  scale_fill_manual(values = c("True Effect" = "black", "TWFE" = "grey", "CS" = "grey", 
                               "Stacked" = "grey", "ETWFE" = "grey")) +
  my_theme_rotate() +
  theme(legend.position = "none") +
  labs(x = "", y = "Value") + # , title = "Comparison of Overall ATT Estimates"
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), # Adjust to ensure bars and error bars aren't cut off
                     limits = c(0,11))  

plot
ggsave(paste0(out_dir, 'overall_att_comparison.png'))


# Beep -------------
beep()
