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

# Assuming avg_dv_period is your data frame
ggplot(avg_dv_period[cohort_period!=999], aes(x = time_since_treat, y = mean_dep_var, 
                          group = interaction(cohort_period, covariate.f),
                          color = factor(covariate.f), linetype = factor(covariate.f))) +
  geom_line() +
  geom_point() +
  labs(x = "Time Since Treatment", y = "Mean Dependent Variable",
       color = "Covariate Value", linetype = "Covariate Value") +
  theme_minimal() +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
  facet_wrap(~cohort_period) # If you want separate plots for each coh
ggsave(paste0(out_dir, 'dv_cov.pnd'))

# TWFE -------------------------------------------------------------------
# Simple
formula <- as.formula('dep_var ~ treat + covariate')
twfe <- feols(formula,
                       data = dt, panel.id = "unit",
                       fixef = c("unit", "period"), cluster = "unit")
summary(twfe) 
simple_twfe_avg = twfe$coefficients

# Bacon Decomposition
bacon_decomp <- bacon(formula, dt, id_var="unit", time_var='period', quietly = F)
summary(bacon_decomp)
bacon_decomp_avg <- sum(bacon_decomp$weight * bacon_decomp$estimate)


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

emfx(
  mod_etwfe_pkg,
  type = c("simple"),
  by_xvar = T,
  collapse = "auto")



# Stacked -----------------------------------------------------------------


