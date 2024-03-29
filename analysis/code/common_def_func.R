# Common libraries and functions
library(didimputation)
library(PanelMatch)
library(broom)
library(tidyverse)
library(panelView)
library(fect)
library(MetBrewer)
library(stargazer)
library(modelsummary)
library(ggpubr)
library(data.table)
library(fastDummies) # Create dummy variables
library(fixest) # Fixed-effects regression
library(kableExtra) # Make nice tables
library(bacondecomp) # Goodman-Bacon Decomposition
library(did) # Difference-in-differences
library(etwfe)
library(beepr)
library(dplyr)
library(stringr)
library(xtable)


dir.create('../output/',  showWarnings = FALSE)
out_dir <- '../output/'

# Export regression results as DF
export_reg_as_df <- function(model, dv, out_dir,method, ref_periods) {
  # Perform operations
  results <- broom::tidy(model, conf.int = TRUE) %>% 
    mutate(t =  as.double(str_replace(term, "time_since_treat::", ""))) %>% 
    # filter(t >= -1*max_t & t <= max_t) %>% 
    select(t, estimate, conf.low, conf.high) %>% 
    bind_rows(tibble(t = ref_periods[1], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    bind_rows(tibble(t = ref_periods[2], estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    mutate(method = method)
  
  # Write to CSV
  write.csv(results, paste0(out_dir, dv, '',method,'.csv'))
  
  # Return the results
  return((results))
}

# Define preferred ggplot theme
# my_theme <- function() {
#   theme(
#     panel.background = element_rect(fill = "white"),
#     panel.grid.major = element_line(color = "#f0f0f0", linetype = 1), # Adds major grid lines
#     panel.grid.minor = element_blank(), # Adds minor grid lines
#     axis.line = element_line(color = "gray"),
#     text = element_text(size = 20)
#   )
# }

library(ggplot2)

# Define a ggplot theme for academic papers
my_theme <- function(base_size = 20) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(color = "#dddddd", size = 0.5), # Adds major grid lines
      panel.grid.minor = element_blank(), # No minor grid lines
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.text = element_text(size = base_size * 0.8, color = "black"), # Slightly smaller text for axis labels
      axis.title = element_text(size = base_size, color = "black"), # Title size same as base
      legend.position = "bottom", # Position of the legend
      legend.text = element_text(size = base_size * 0.8), # Slightly smaller text for legend
      legend.title = element_text(size = base_size * 0.8), # Slightly smaller text for legend title
      plot.title = element_text(size = base_size * 1.2, hjust = 0.5), # Larger text for title
      plot.subtitle = element_text(size = base_size, hjust = 0.5), # Same size as base for subtitle
      plot.caption = element_text(size = base_size * 0.8, hjust = 0.5) # Slightly smaller text for caption
    )
}

# Update the my_theme function to include rotated x-axis text
my_theme_rotate <- function(base_size = 20) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(color = "#dddddd", size = 0.5), # Adds major grid lines
      panel.grid.minor = element_blank(), # No minor grid lines
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.text = element_text(size = base_size * 0.8, color = "black"), # Slightly smaller text for axis labels
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # Rotate x-axis labels
      axis.title = element_text(size = base_size, color = "black"), # Title size same as base
      legend.position = "bottom", # Position of the legend
      legend.text = element_text(size = base_size * 0.8), # Slightly smaller text for legend
      legend.title = element_text(size = base_size * 0.8), # Slightly smaller text for legend title
      plot.title = element_text(size = base_size * 1.2, hjust = 0.5), # Larger text for title
      plot.subtitle = element_text(size = base_size, hjust = 0.5), # Same size as base for subtitle
      plot.caption = element_text(size = base_size * 0.8, hjust = 0.5) # Slightly smaller text for caption
    )
}


generate_significance_stars <- function(p_value) {
  sig_stars <- ''
  if(p_value <= 0.05) {
    sig_stars <- '*'
  }
  return(sig_stars)
}

# Example usage:
generate_significance_stars(0.04)
