
# ==============================================================================

# GOAL:  To test whether the accuracy of the scale indicators changes over time 
# and to use this insight to determine how often the scales should be calibrated

# ==============================================================================

# ---------------------------------
# Read in libraries and data
# ---------------------------------

rm(list=ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Read in datafile called "Drift-measurements-stony-point.csv"
d <- read.csv('XXX') # CHANGE DIRECTORY

head(d)
str(d)

# ---------------------------------
# Calculate error in scale
# ---------------------------------

# For each day, we read the scale indicators (for scale 1 and 2) for when
# there was no weight on the scale (0kg), and when there were known weights of 
# 1kg, 5kg, and 6kg placed on the scales. The values are the total reading for 
# each weight. Therefore calculate the error by subtracting the static weight 
# from the indicator reading

d <- d %>% mutate(Scale1_0kg_diff = Scale1_0kg - 0,
                  Scale1_1kg_diff = Scale1_1kg - 1,
                  Scale1_5kg_diff = Scale1_5kg - 5,
                  Scale1_6kg_diff = Scale1_6kg - 6,
                  Scale2_0kg_diff = Scale2_0kg - 0,
                  Scale2_1kg_diff = Scale2_1kg - 1,
                  Scale2_5kg_diff = Scale2_5kg - 5,
                  Scale2_6kg_diff = Scale2_6kg - 6)

# ---------------------------------
# Standardise the data
# ---------------------------------

# Want to assess whether the changes in readings are due to changes in the zero (tare)
# value, or due to the accuracy of the indicators drifting over time.
# To do this, subtract the zero value (the reading when there was no weight on
# the scales) from each reading where there was a weight on the scale

d <- d %>%
  mutate(
    Scale1_1kg_corrected = Scale1_1kg_diff - Scale1_0kg_diff,
    Scale1_5kg_corrected = Scale1_5kg_diff - Scale1_0kg_diff,
    Scale1_6kg_corrected = Scale1_6kg_diff - Scale1_0kg_diff,
    Scale2_1kg_corrected = Scale2_1kg_diff - Scale2_0kg_diff,
    Scale2_5kg_corrected = Scale2_5kg_diff - Scale2_0kg_diff,
    Scale2_6kg_corrected = Scale2_6kg_diff - Scale2_0kg_diff
  )

# ---------------------------------
# Change the format of the data from wide to long
# ---------------------------------

# Use pivot_longer to transform the specified columns into rows
# 'names_to' creates the new column 'Factor' with the original column names
# 'values_to' creates a column for the actual numerical values

# We make two different datasets because we will plot the results separately

transformed_data_diff <- d %>%
  select(Days_since_start, 
         Scale1_0kg_diff, Scale1_1kg_diff,  Scale1_5kg_diff,
         Scale1_6kg_diff, Scale2_0kg_diff, Scale2_1kg_diff, Scale2_5kg_diff,
         Scale2_6kg_diff) %>% 
  pivot_longer(
    cols = starts_with("Scale"), # Selects all columns starting with "Scale"
    names_to = "Combo",         # New column for original column names
    values_to = "Scale_error_kg"          # New column for the values
  ) %>% 
  mutate(Scale = str_sub(Combo, 1, 6),
         Static_weight = str_sub(Combo, 8, 10)) # Using str_sub from stringr for consistency

transformed_data_corrected <- d %>%
  select(Days_since_start, 
         Scale1_1kg_corrected,  Scale1_5kg_corrected,
         Scale1_6kg_corrected, Scale2_1kg_corrected, Scale2_5kg_corrected,
         Scale2_6kg_corrected) %>% 
  pivot_longer(
    cols = starts_with("Scale"), # Selects all columns starting with "Scale"
    names_to = "Combo",         # New column for original column names
    values_to = "Scale_error_kg"          # New column for the values
  ) %>% 
  mutate(Scale = str_sub(Combo, 1, 6),
         Static_weight = str_sub(Combo, 8, 10)) # Using str_sub from stringr for consistency


# ---------------------------------
# Now plot
# ---------------------------------

# Shared scales: set limits so both plots carry all four levels
# (check these match your Static_weight labels exactly)
wt_levels <- c("0kg", "1kg", "5kg", "6kg")

shared_scales <- list(
  scale_fill_manual(name = "Static weight", limits = wt_levels,
                    values = c("black", "#E69F00", "#56B4E9", "#009E73")),
  scale_shape_manual(name = "Static weight", limits = wt_levels,
                     values = c(21, 22, 23, 24))
)

drift_plot <- function(dat) {
  ggplot(dat, aes(x = Days_since_start, y = 1000 * Scale_error_kg,
                  fill = Static_weight, shape = Static_weight)) +
    geom_point(size = 4, alpha = 0.6, colour = "black", show.legend = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ Scale) +
    labs(x = "Days since calibration", y = "Scale drift (g)") +
    xlim(0, 100) +
    ylim(-50, 150) +
    shared_scales +
    theme_bw(base_size = 16) +
    theme(strip.background = element_rect(fill = "lightgrey"))
}

p1 <- drift_plot(transformed_data_diff)
p2 <- drift_plot(transformed_data_corrected)

combined <- (p1 / p2) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 16, face = "bold"))

combined

ggsave("XXX", plot = combined, width = 7, height = 9, dpi = 600, units = "in") #CHANGE DIRECTORY

