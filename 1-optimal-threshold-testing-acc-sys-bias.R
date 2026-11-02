
# ==============================================================================
# All of the below analyses use the dataset of paired true versus estimated 
# weights collected by deploying a third scale in front of the permanent APMS
# 
# GOAL 1:  To determine the optimal cut-off threshold for the difference 
#          between the two APMS scales  
# GOAL 2:  To test the accuracy of the APMS algorithm by comparing the true 
#          and estimated weights 
# GOAL 3:  Check for systematic bias - does the APMS algorithm consistently
#          over- or under-estimate? 
# ==============================================================================

# ---------------------------------
# Read in libraries and data
# ---------------------------------

rm(list=ls())

library(tidyverse)
library(ggpubr)
library(forcats)

# Load datafile called "Movement_calibrations_for_R.csv"
d <- read.csv("XXX") # CHANGE DIRECTORY

# ---------------------------------
# Clean up structure of data
# ---------------------------------

str(d)

# set factor variables
head(d)
d$algorithm_weight_present <- as.factor(d$algorithm_weight_present)
d$design <- as.factor(d$design)
d$colony <- as.factor(d$colony)

# extract year
d <- d %>% 
  mutate(year = substr(file_name_1, 8, 11))

d$year <- as.factor(d$year)

unique(d$year)

# remove rows where no true weights - these rows correspond to crossings when 
# the APMS algorithm was not running and so there are no estimated weights
d <- d %>% filter(algorithm_weight_present != "no - algorithm not running")

# some basic data summaries 
table(d$algorithm_weight_present)
d %>% group_by(year, colony, algorithm_weight_present) %>% summarise(n())
d %>% group_by(algorithm_weight_present) %>% summarise(n())

# Remove crossings where the algorithm was unable to estimate a penguin weight
# because, for example, too many penguins crossed at once or the penguin crossed 
# too quickly
df1 <- d %>% filter(algorithm_weight_present == "yes")

# summary of final dataset
df1 %>% summarise(n())
df1 %>% group_by(year, colony) %>% summarise(n())

# Make sure the dead weight of the third scale is accounted for properly by 
# adding the dead weight to the total weight
df1 <- df1 %>% 
  mutate(total_weight_scale_3 = 
           total_weight_scale_3 + dead_weight_scale_3)

# Calculate difference between true and estimated weights
# This value is how many grams the algorithm was off by
# Positive values mean algorithm was over-estimating penguin, negative values
# mean algorithm was underestimating values
df1 <- df1 %>% mutate(diff = (estimated_from_algorithm - penguin_weight_scale_3))

# Raw difference
ggplot(data = df1, aes(x=1000*diff)) +
  geom_histogram(fill = 'grey', colour = 'black') +
  theme_bw() +
  labs(x = "Error (g)",
       y = "Count") +
  annotate("text", x = -220, y = 11, label = "A", size = 10)

# Absolute difference
ggplot(data = df1, aes(x=abs(1000*diff))) +
  geom_histogram(fill = 'grey', colour = 'black') +
  theme_bw() +
  labs(x = "Absolute error (g)",
       y = "Count") +
  annotate("text", x = 0, y = 8.5, label = "B", size = 10)

# Summary stats for absolute difference
df1 %>% summarise(mean_abs_diff_g = 1000*mean(abs(diff)),
                 sd_abs_diff_g = 1000*sd(abs(diff)),
                 median_abs_diff_g = 1000*median(abs(diff)),
                 min_abs_diff_g = 1000*min(abs(diff)),
                 max_abs_diff_g = 1000*max(abs(diff)))

# ---------------------------------
# Calculate accuracy of APMS algorithm
# ---------------------------------

# To account for different size penguins, convert difference between true and 
# estimated weights to a percentage accuracy (see methods for details) 

df1 <- df1 %>% 
  mutate(accuracy = 1-(abs(estimated_from_algorithm - penguin_weight_scale_3)/penguin_weight_scale_3),
         accuracy_perc = 100*accuracy)

# Accuracy is left-skewed and not normally distributed
# Therefore use non-parametric tests going forward
ggplot(df1, aes(x = accuracy)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = 'grey', colour = 'black') +
  theme_bw() +
  labs(x = "Accuracy",
       y = "Count") +
  annotate("text", x = min(df1$accuracy), y = 50, label = "C", size = 10)

hist(df1$accuracy)
shapiro.test(df1$accuracy)

# ---------------------------------
# Goal 1: To determine optimal threshold for difference between scales, we
# sub sample to find balance point between accuracy vs data retention
# ---------------------------------

# Summarise differences between the scales for whole dataset
df1 %>%
  summarise(mean = round(mean(1000*abs(difference_btwn_scales)), 2),
            sd = round(sd(1000*abs(difference_btwn_scales)), 2),
            median = round(median(1000*abs(difference_btwn_scales)), 2),
            min = round(min(1000*abs(difference_btwn_scales)), 2),
            max = round(max(1000*abs(difference_btwn_scales)), 2))

# Create a sequence of thresholds in difference between the scales
# We increase threshold by 0.05kg for 0.5-5kg
thresholds <- seq(0.05, 0.5, by = 0.05)

# Map through thresholds to calculate number of data points versus accuracy
library(purrr)

tradeoff_data <- map_df(thresholds, function(t) {
  filtered_df <- df1 %>% filter(abs(difference_btwn_scales) <= t)
  
  n <- nrow(filtered_df)
  
  data.frame(
    threshold = t,
    n_remaining = n,
    percent_data_kept = (n / nrow(df1)) * 100,
    mean_accuracy = mean(filtered_df$accuracy) * 100,
    sd_accuracy = sd(filtered_df$accuracy) * 100,
    se_accuracy = (sd(filtered_df$accuracy) * 100) / sqrt(n)
  )
})

# View the results as a table
print(tradeoff_data)

# View the results as a line plot
dual_axis<-
  ggplot(tradeoff_data, aes(x = threshold*1000)) +
  
  # Scale the sample size (%) so it fits in the 97-100 range for display
  geom_errorbar(aes(ymin = mean_accuracy - se_accuracy,
                    ymax = mean_accuracy + se_accuracy,
                    color = "Mean accuracy (%)"),
                width = 0.5,           # Width of the horizontal whisker
                size = 0.9,
                alpha = 0.7) +       # Slight transparency so points stay visible
  geom_point(aes(y = 96 + (percent_data_kept / 100) * 4, 
                 color = "Data retained (%)", 
                 shape = "Data retained (%)"),
             size = 3) +
  geom_point(aes(y = mean_accuracy, 
                 color = "Mean accuracy (%)", 
                 shape = "Mean accuracy (%)"),
             size = 3) +
  
  geom_line(aes(y = 96 + (percent_data_kept / 100) * 4, 
                color = "Data retained (%)",
                shape = "Data retained (%)"), 
            size = 1.2) +
  geom_line(aes(y = mean_accuracy, 
                color = "Mean accuracy (%)",
                shape = "Mean accuracy (%)"), 
            size = 1.2) +
  
    
  # Set the axis to show the transformed values correctly
  scale_y_continuous(
    name = "Mean accuracy (%)",
    limits = c(97, 100),
    sec.axis = sec_axis(~ (. - 96) / 4 * 100, name = "Data retained (%)")
  ) +
  
  scale_x_continuous(
    name = "Scale difference threshold (g)",
    breaks = seq(0.05*1000, 0.5*1000, by = 0.05*1000)
  ) +
  
  scale_color_manual(name = NULL,
                     values = c("Data retained (%)" = "black", 
                                "Mean accuracy (%)" = "#E69F00")) +
  
  scale_shape_manual(name = NULL,
                     values = c("Data retained (%)" = 1, 
                                "Mean accuracy (%)" = 23)) +
  
  theme_bw(base_size = 16) +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.8, 0.1), # Adjust these numbers to move it around
    legend.background = element_rect(fill = 'white', colour = 'black'),
    #legend.text = element_text(size = 12),
    legend.title = element_blank())
    #axis.text = element_text(size = 12),
    #axis.title = element_text(size = 14)) 

dual_axis

ggsave(dual_axis, filename = 'XXX',
       width = 9, height = 6, dpi = 300) # CHANGE DIRECTORY

# It looks like 150g is most optimal threshold - apply to dataset
df <- df1 %>% filter(abs(difference_btwn_scales) < 0.15)

# ---------------------------------
# GOAL 2: Once we have applied the optimal threshold, we can begin assessing
# how accurately the APMS algorithm estimates penguin weights
# ---------------------------------

# Basic reporting stats from filtered dataset

df %>% group_by(year, colony) %>% summarise(n())
df %>% summarise(n())
df %>% group_by(confidence_score) %>% summarise(n())

# TRUE - ESTIMATED BEFORE FILTERING
# Absolute difference
df1 %>% summarise(mean_abs_diff_g = round(1000*mean(abs(diff)),2),
                 sd_abs_diff_g = round(1000*sd(abs(diff)),2),
                 median_abs_diff_g = round(1000*median(abs(diff)),2),
                 min_abs_diff_g = round(1000*min(abs(diff)),2),
                 max_abs_diff_g = round(1000*max(abs(diff)),2))

# Raw difference
df1 %>% summarise(mean_diff_g = round(1000*mean(diff),2),
                 sd_diff_g = round(1000*sd(diff),2),
                 median_diff_g = round(1000*median(diff),2),
                 min_diff_g = round(1000*min(diff),2),
                 max_diff_g = round(1000*max(diff),2))

# TRUE - ESTIMATED AFTER FILTERING
# Absolute difference
df %>% summarise(mean_abs_diff_g = round(1000*mean(abs(diff)),2),
                  sd_abs_diff_g = round(1000*sd(abs(diff)),2),
                  median_abs_diff_g = round(1000*median(abs(diff)),2),
                  min_abs_diff_g = round(1000*min(abs(diff)),2),
                  max_abs_diff_g = round(1000*max(abs(diff)),2))

# Raw difference
df %>% summarise(mean_diff_g = round(1000*mean(diff),2),
                  sd_diff_g = round(1000*sd(diff),2),
                  median_diff_g = round(1000*median(diff),2),
                  min_diff_g = round(1000*min(diff),2),
                  max_diff_g = round(1000*max(diff),2))

# ACCURACY BEFORE
df1 %>%
  summarise(mean = round(mean(accuracy_perc), 2),
            sd = round(sd(accuracy_perc), 2),
            median = round(median(accuracy_perc), 2),
            min = round(min(accuracy_perc), 2),
            max = round(max(accuracy_perc), 2))

# ACCURACY AFTER
df %>%
  summarise(mean = round(mean(accuracy_perc), 2),
            sd = round(sd(accuracy_perc), 2),
            median = round(median(accuracy_perc), 2),
            min = round(min(accuracy_perc), 2),
            max = round(max(accuracy_perc), 2))

# ---------------------------------
# Correlate true and estimated weights
# ---------------------------------
# Fit linear model to see summary statistics
model1 <- lm(estimated_from_algorithm ~ penguin_weight_scale_3, data = df)
summary(model1)

# Test if fitted line is significantly different from reference line which
# has intercept = 0 and slope = 1
library(car)
linearHypothesis(model1, c("(Intercept) = 0", "penguin_weight_scale_3 = 1"))

# Extract co-efficients for plotting
cf <- coef(model1)
r2 <- summary(model1)$r.squared

eq_lab <- sprintf("italic(y) == italic(x) ~ '%s' ~ '%.3f' * ',' ~~ italic(R)^2 == '%.2f'",
                  ifelse(cf[1] < 0, "\u2212", "+"), abs(cf[1]), r2)
# Plot
acc_plot <-
  ggplot(data = df,
       aes(x=penguin_weight_scale_3, y = estimated_from_algorithm)) +
  # reference line 1:1 
  geom_abline(aes(slope = 1, intercept = 0, linetype = 'dashed', colour = 'Reference (1:1)'), 
              size = 1.5, linetype = 'dashed',
              key_glyph = draw_key_path) +
  # add data points
  geom_point(size = 2.5, alpha = 0.7, shape = 21, colour = 'black', fill = "grey") +
  # add in regression line and corresponding stats
  geom_smooth(aes(colour = "Regression"),
              method = "lm", linetype = 'solid', size = 1) +
  # Print out regression line
  annotate("text", x = 2.2, y = 3.9, label = eq_lab, parse = TRUE,
           hjust = 0, size = 7) +
  # Specify legend items
  scale_colour_manual(name = "Model fit",
                      values = c("Reference (1:1)" = '#E69F00',
                                 "Regression" = 'black')) +
  # change axes titles
  labs(x = expression(italic(W[true]) ~ (kg)),
       y = expression(italic(W[est]) ~ (kg))) +
  # specify themes
  theme_bw(base_size = 16) +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.80, 0.2), # Adjust these numbers to move it around
    legend.background = element_rect(fill = 'white', colour = 'black'))
    #legend.text = element_text(size = 12),
    #legend.title = element_text(size = 14),
    #axis.text = element_text(size = 14),
    #axis.title = element_text(size = 14)) 

acc_plot

ggsave("XXX", plot = acc_plot, width = 7, height = 5.5, dpi = 900, units = "in") # CHANGE DIRECTORY

# ---------------------------------
# Now see if any factors affect accuracy
# ---------------------------------

# ---------------------------------
# See if there is a difference in accuracy between static versus dynamic weights
# ---------------------------------

# Reminder of how the APMS algorithm works:
# If both readings are static or both are dynamic, the algorithm takes the 
# average of the two readings
# If one reading is static and one dynamic, the algorithm discards the dynamic
# reading and uses the static reading as the final estimate

# First make sure we know which crossing was classified as static versus dynamic

df$scale_1_type <- as.factor(df$scale_1_type)
df$scale_2_type <- as.factor(df$scale_2_type)

df <- df %>%
  mutate(estimate_type = case_when(
    scale_1_type == 'static' | scale_2_type == 'static' ~ 'static',
    scale_1_type == 'dynamic' & scale_2_type == 'dynamic' ~ 'dynamic'
  ))

df$estimate_type <- as.factor(df$estimate_type)

hist(df$accuracy)
ggplot() +
  geom_histogram(data = df, aes(x = accuracy)) +
  facet_wrap(~estimate_type) +
  theme_bw()

# p > 0.05: Data is considered normally distributed
# p < 0.05: Data significantly deviates from normality

by(df$accuracy, df$estimate_type, shapiro.test)

# Calculate the Wilcoxon test results 
estimate_type_test <- wilcox.test(accuracy ~ estimate_type, data = df)
estimate_type_test

# Create the label string exactly how you want it
estimate_type_stat_label <- 
  paste0("W = ", round(estimate_type_test$statistic, 1),
         ", p = ", format.pval(estimate_type_test$p.value, digits = 3))
estimate_type_stat_label

set.seed(123)

plot_c <-
  ggplot(df, aes(x = estimate_type, y = accuracy)) +
  geom_boxplot() +
  geom_jitter(width=0.02, alpha = 0.7, size = 0.6) +
  # This adds the Wilcoxon test result
  annotate("text", x = 1.5, y = 0.925, label = estimate_type_stat_label, size = 5) +
  # Add the mean value
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, fill = "#E69F00", color = "black") +
  theme_bw(base_size = 16) +
  labs(x = "Type of weight estimate",
       y = "Accuracy")

plot_c

ggsave('XXX', plot = plot_c, width = 8, height = 6, dpi = 800, units = "in") # CHANGE DIRECTORY

# ---------------------------------
# See if there is a relationship between accuracy and difference between the scales
# ---------------------------------

# Could reasonably predict that accuracy of final estimate decreases as 
# difference between the scales increases
# But if no relationship found, then suggests the threshold that we set in goal 
# 1 is working

ggplot(df, aes(x=difference_btwn_scales)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Difference between scale 1 and 2 (kg)",
       y = "Count")

ggplot(df, aes(x=abs(difference_btwn_scales))) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Absolute difference between scale 1 and 2 (kg)",
       y = "Count")

summary_diff_btwn_scales <- df %>%
  summarise(mean = round(mean(abs(difference_btwn_scales)), 2),
            sd = round(sd(abs(difference_btwn_scales)), 2),
            median = round(median(abs(difference_btwn_scales)), 2),
            min = round(min(abs(difference_btwn_scales)), 2),
            max = round(max(abs(difference_btwn_scales)), 2))

summary_diff_btwn_scales

ggplot(data = df, aes(x = 1000*difference_btwn_scales, y = 100*accuracy)) +
  geom_point() +
  #geom_smooth(aes(colour = "Regression"),
  #            method = "lm", linetype = 'solid') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  labs(x = "Difference between scale 1 and 2 (g)",
       y = "Accuracy (%)")

model <- lm(difference_btwn_scales ~ accuracy, data = df)
summary(model)

shapiro.test(df$difference_btwn_scales)
shapiro.test(df$accuracy)

cor.test(df$difference_btwn_scales, df$accuracy, method = "spearman", exact = FALSE)

# No significant linear relationship found

# ---------------------------------
# GOAL 3: Check for systematic bias
# ---------------------------------

# Step 1: calculate residuals as the difference between the true and estimated weights
# We have done this above, but redo to ensure naming makes sense
df <- df %>%
  mutate(
    residual  = estimated_from_algorithm - penguin_weight_scale_3,
    mean_pair = (estimated_from_algorithm + penguin_weight_scale_3) / 2
  )

# Step 2: check if residuals differ from 0

hist(df$estimated_from_algorithm)
hist(df$penguin_weight_scale_3)
hist(df$residual)

# p > 0.05: Data is considered normally distributed
# p < 0.05: Data significantly deviates from normality

shapiro.test(df$residual)

# One-sample wilcoxon test on the residuals you already calculated
# NOTE: a paired Wilcoxon test would give exact same result - it calculates the 
# residuals inside the test, whereas we have just calculated the residuals 
# manually first 
wilcox.test(df$residual, mu = 0)

# Summary error metrics

df %>%
  summarise(
    ME   = mean(residual),
    MAE  = mean(abs(residual)),
    RMSE = sqrt(mean(residual^2))
  )

# Step 3. BLAND-ALTMAN PLOT

ba_mean  <- mean(df$residual)
ba_sd    <- sd(df$residual)
loa_up   <- ba_mean + 1.96 * ba_sd
loa_down <- ba_mean - 1.96 * ba_sd

ba_plot<-
  ggplot(df, aes(x = mean_pair, y = residual)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue", size = 1.5) +  # reveals non-linear trends
  geom_hline(yintercept = ba_mean,  colour = "black",    linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = loa_up,   colour = "#E69F00", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = loa_down, colour = "#E69F00", linetype = "dashed", linewidth = 1) +
  annotate("text", x = max(df$mean_pair), y = ba_mean,  label = paste("Bias:", round(ba_mean, 2)),  
           hjust = 1, vjust = 1.3, size = 5) +
  annotate("text", x = max(df$mean_pair), y = loa_up,   label = paste("ULoA:", round(loa_up, 2)),   
           hjust = 1, vjust = 1.3, size = 5) +
  annotate("text", x = max(df$mean_pair), y = loa_down, label = paste("LLoA:", round(loa_down, 2)), 
           hjust = 1, vjust = 1.3, size = 5) +
  labs(
    x = expression("Mean of " * italic(W)[true] * " and " * italic(W)[est] * " (kg)"),
    y = expression(italic(W)[est] * " \u2212 " * italic(W)[true] * " (kg)")) +
  theme_bw(base_size = 16) +
  theme(plot.margin = margin(10, 20, 10, 10))

ba_plot

ggsave('XXX', plot = ba_plot, width = 10, height = 5, dpi = 600, units = "in") #CHANGE DIRECTORY
