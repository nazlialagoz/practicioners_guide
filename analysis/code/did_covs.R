# Covariates --------------------------------------------------------------
# TWFE & ETWFE & Stacked
rm(list = ls())
source('sim_data_cov.R') # Import simulation function and some utilities
source('common_def_func.R') # Import libraries and common functions

dt <- sim_data()
head(dt)


# Prep --------------------------------------------------------------------
dt[, covariate.f := as.factor(covariate)]

unique(dt$cohort_period)
unique(dt$period)
# dt <- dt[] # exclude period where everyone is treated so that we have an untreated group

# EDA and Analysis --------------------------------------------------------
# Check out the data
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



# Visualization -----------------------------------------------------------

avg_dv_period <- dt[, .(mean_dep_var = mean(dep_var)), by = c('cohort_period','period')] 

# Convert 'cohort_period' to a factor to have discrete color scale in the plot
avg_dv_period[, cohort_period := as.factor(cohort_period)]

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

unique(dt$covariate)
unique(dt$cov_effect)


# Visualize the treatment effect w/ cov -----------------------------------
# Per cohort, rel time, and covariate. 
# How to find avg effect of it. 
avg_dv_period <- dt[, .(mean_dep_var = mean(dep_var)), by = c('cohort_period','time_since_treat','covariate.f')] 
avg_dv_period <- avg_dv_period[cohort_period!=999]
avg_dv_period[, time_since_treat := as.integer(time_since_treat)]
avg_dv_period[, `Cohort-period` :=  cohort_period]
# Assuming avg_dv_period is your data frame
ggplot(avg_dv_period, aes(x = (time_since_treat), y = mean_dep_var, 
                          group = interaction(`Cohort-period`, covariate.f),
                          color = factor(covariate.f), linetype = factor(covariate.f))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = -4:7)+
  labs(x = "Time Since Treatment", y = "Mean Dependent Variable",
       color = "Covariate Value", linetype = "Covariate Value") +
  my_theme() +
  scale_color_manual(values = c("0" = "black", "1" = "darkgray")) +
  scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"))  +
  facet_wrap(~`Cohort-period`, labeller = "label_both") # If you want separate plots for each coh
ggsave(paste0(out_dir, 'dv_cov.png'))

# True treatment effect
dt[, total_effect := treat*(tau_cum+cov_effect)]
true_avg = mean(dt[treat==1]$total_effect) # 10.24
true_avg_cov1 = (dt[treat==1 & covariate.f==1, 
                        .(mean(total_effect))]) # 12.03
true_avg_cov0 = mean(dt[treat==1 & covariate.f==0]$total_effect) # 8.291

# TWFE -------------------------------------------------------------------
# Simple
dt[, treat_covariate := treat*covariate]
formula <- as.formula('dep_var ~ treat + treat_covariate')
twfe <- feols(formula,
                       data = dt, panel.id = "unit",
                       fixef = c("unit", "period"), cluster = "unit")
summary(twfe) 
simple_twfe_avg = twfe$coefficients

msummary(twfe,
         output = paste0(out_dir,"canonical_did_cov.tex"),
         stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
         fmt = 2)

formula <- as.formula('dep_var ~ treat + covariate')
# Bacon Decomposition
bacon_decomp <- bacon(formula, dt, id_var="unit", time_var='period', quietly = F)
summary(bacon_decomp)
bacon_decomp$two_by_twos
#> # Bacon Decomposition
#> bacon_decomp <- bacon(formula, dt, id_var="unit", time_var='period', quietly = F)
# type  weight  avg_est
# 1         Both Treated 0.16419 -4.51856
# 2 Treated vs Untreated 0.83581 13.65014

# Dynamic


# ETWFE -------------------------------------------------------------------
mod_etwfe_pkg =
  etwfe(
    fml  = dep_var ~ 0, # outcome ~ controls
    tvar = period,        # time variable
    xvar = covariate.f,
    gvar = cohort_period, # group variable
    data = dt,       # dataset
    vcov = ~unit  # vcov adjustment (here: clustered)
  )
summary(mod_etwfe_pkg)

emfx(mod_etwfe_pkg, type = "event") # dynamic ATE a la an event study

msummary(mod_etwfe_pkg,
         output = paste0(out_dir,"etwfe_cov.tex"),
         stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
         fmt = 2)


etwfe <- emfx(
  mod_etwfe_pkg,
  type = c("simple"),
  by_xvar = T,
  collapse = "auto")
# TODO: put the results into a latex table
etwfe$contrast

# Assuming etwfe is your data frame with the model results
# Convert your data frame to a LaTeX table
latex_table <- xtable(etwfe)

# Print the LaTeX table to the console
print(latex_table, include.rownames=FALSE)

# Optionally, you can use the print.xtable function to write the table to a file
print(latex_table, include.rownames=FALSE, file=paste0(out_dir,"etwfe_cov.tex"))


# Stacked -----------------------------------------------------------------
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
  model_formula_simple <- as.formula(paste(outcome_variable, "~ treat + treat_covariate | unit^df + period^df"))
  
  # Fit the model using the 'feols' function from the 'fixest' package
  model_simple <- feols(model_formula_simple, data = stacked_data, cluster = "bracket_df")
  
  # Print the summary of the model
  print(summary(model_simple)) 
  return(model_simple)
}

mod_stacked_simple <- run_stacked_did_simple(outcome_variable)
stacked_simple_avg_te <- mod_stacked_simple$coefficients


msummary(mod_stacked_simple,
         output = paste0(out_dir,"stacked_did_cov.tex"),
         stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
         fmt = 2)


# ETWFE seems to be better but it cannot do continous


# Combine Stacked & ETWFE -------------------------------------------------
# Model outcomes for Stacked DiD
stacked_did_data <- data.frame(
  term = c("treat", "treat_covariate"),
  estimate = stacked_did_results$Estimate, # c(6.94020, 3.36778),
  std.error = stacked_did_results$Std.Error, # c(0.495011, 0.638373),
  stars = c("***", "***") # Manually adding stars for significance levels
)

# Model outcomes for ETWFE
etwfe_data <- data.frame(
  term = c("treat", "treat x covariate"),
  estimate = c(etwfe$estimate[1], etwfe$estimate[2] - etwfe$estimate[1]), # Calculating the interaction term
  std.error = c(etwfe$std.error[1], sqrt(etwfe$std.error[1]^2 + etwfe$std.error[2]^2)), # Assuming independence for the interaction term
  stars = c("***", "***") # Manually adding stars for significance levels
)

# Create a new data frame to combine the results
combined_results <- data.frame(
  Term = stacked_did_data$term,
  StackedDiD = paste0(stacked_did_data$estimate, stacked_did_data$stars, "\n", "(", stacked_did_data$std.error, ")"),
  ETWFE = paste0(etwfe_data$estimate, etwfe_data$stars, "\n", "(", etwfe_data$std.error, ")")
)

# Set row names to NULL to avoid printing them
row.names(combined_results) <- NULL

# Convert to LaTeX table
latex_table <- xtable(combined_results, align = "llcc", caption = "Results from Stacked DiD with a Moderator", label = "tab:stacked_cov")
print(latex_table, include.rownames = FALSE, hline.after = c(-1, 0, 2), booktabs = TRUE,
      sanitize.text.function = function(x) {
        # Replace line breaks with LaTeX double backslash for new line
        gsub("\n", " \\\\\n", x)
      },
      caption.placement = "top")

# Optionally, write to file
print(latex_table, include.rownames = FALSE, hline.after = c(-1, 0, 2), booktabs = TRUE,
      sanitize.text.function = function(x) {
        gsub("\n", " \\\\\n", x)
      },
      file = "model_results_table.tex", caption.placement = "top")

# TODO: also add the TWFE models to the table