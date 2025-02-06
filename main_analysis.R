###############################################################################
# LOAD LIBRARIES
###############################################################################
library(ggalluvial)
library(ggplot2)
library(MASS)
library(dplyr)
library(tidyr)
library(ggpubr)
library(rcompanion)
library(effsize)

###############################################################################
# 0. DATA INPUT
###############################################################################

# Load the CSV file while preserving column names
data <- read.csv("~/Desktop/Trabajo/Natrox/data/data.csv", header = TRUE)

# Correct inconsistencies in some columns
data$pain_level_before_natrox <- gsub("Moderado", "Moderate", data$pain_level_before_natrox)
data$treatment_continuity <- gsub("Discontinuo", "Discontinuous", data$treatment_continuity)

###############################################################################
# 1. OVERALL WILCOXON TEST
###############################################################################
#
# We compare "pain_level_before_natrox" vs. "pain_level_after_natrox" using 
# a paired Wilcoxon test (ordinal data). The alternative hypothesis is that 
# pain after treatment is lower, so we set 'alternative = "greater"'.
#

# Convert columns to ordered factors to respect the ordinal nature
data$pain_level_before_natrox <- factor(
  data$pain_level_before_natrox,
  levels = c("No Pain", "Low", "Moderate", "High"),
  ordered = TRUE
)

data$pain_level_after_natrox <- factor(
  data$pain_level_after_natrox,
  levels = c("No Pain", "Low", "Moderate", "High"),
  ordered = TRUE
)

# Paired Wilcoxon test
wilcox_test <- wilcox.test(
  x = as.numeric(data$pain_level_before_natrox),
  y = as.numeric(data$pain_level_after_natrox),
  paired = TRUE,
  alternative = "greater"  # Hypothesis: pain is lower after treatment
)

# Extract the p-value in scientific notation
p_value <- formatC(wilcox_test$p.value, format = "e", digits = 2)

###############################################################################
# 1A. SANKEY PLOT FOR OVERALL PAIN TRANSITION
###############################################################################
#
# This plot visualizes transitions from "pain_level_before_natrox" to 
# "pain_level_after_natrox".
#

# Create a data frame with transitions between pain levels
sankey_data <- data.frame(
  Before = data$pain_level_before_natrox,
  After  = data$pain_level_after_natrox
)

# Define a color palette for pain levels
pain_colors <- c(
  "No Pain"   = "#00AFBB",
  "Low"       = "#E7B800",
  "Moderate"  = "#FC4E07",
  "High"      = "#7570B3"
)

# Reverse the factor levels so "High" is at the top and "No Pain" at the bottom
sankey_data <- sankey_data %>%
  mutate(
    Before = factor(Before, levels = rev(levels(factor(Before)))),
    After  = factor(After,  levels = rev(levels(factor(After))))
  )

# Build the Sankey plot
ggplot(sankey_data, aes(axis1 = Before, axis2 = After)) +
  geom_alluvium(aes(fill = Before), width = 1/12) +
  geom_stratum(aes(fill = after_stat(stratum)), width = 1/8, color = NA) + 
  scale_x_discrete(limits = c("Before Natrox", "After Natrox"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = pain_colors) +
  labs(x = NULL, y = NULL, fill = "Pain Level") +
  theme_void() +
  theme(
    legend.position = "top",
    plot.margin = unit(c(0.5, 0.5, 0.1, 0.5), "cm")
  ) +
  # Column labels at the bottom
  annotate("text", x = 1,   y = -1.5, label = "Before Natrox", size = 3.5, fontface = "plain", hjust = 0.5) +
  annotate("text", x = 2,   y = -1.5, label = "After Natrox",  size = 3.5, fontface = "plain", hjust = 0.5) +
  # p-value in the middle
  annotate("text", x = 1.5, y = -1.5, 
           label = paste("p-value:", p_value), size = 3.5, fontface = "italic", hjust = 0.5)

###############################################################################
# 2. INDIVIDUAL WILCOXON TESTS BY TREATMENT CONTINUITY
###############################################################################
#
# We split the dataset by "treatment_continuity" (Continuous vs. Discontinuous)
# and run separate Wilcoxon tests to see if pain is reduced after treatment.
#

# Subset function
filter_by_treatment <- function(data, treatment_type) {
  subset(data, treatment_continuity == treatment_type)
}

data_continuous     <- filter_by_treatment(data, "Continuous")
data_discontinuous  <- filter_by_treatment(data, "Discontinuous")

# Function to ensure pain levels are ordered factors
prepare_factors <- function(df) {
  df$pain_level_before_natrox <- factor(
    df$pain_level_before_natrox,
    levels = c("No Pain", "Low", "Moderate", "High"),
    ordered = TRUE
  )
  df$pain_level_after_natrox <- factor(
    df$pain_level_after_natrox,
    levels = c("No Pain", "Low", "Moderate", "High"),
    ordered = TRUE
  )
  return(df)
}

data_continuous    <- prepare_factors(data_continuous)
data_discontinuous <- prepare_factors(data_discontinuous)

# Wilcoxon test (paired)
run_wilcox_test <- function(data_before, data_after) {
  wilcox.test(
    x          = as.numeric(data_before),
    y          = as.numeric(data_after),
    paired     = TRUE,
    alternative = "greater"
  )
}

wilcox_continuous <- run_wilcox_test(
  data_continuous$pain_level_before_natrox,
  data_continuous$pain_level_after_natrox
)

wilcox_discontinuous <- run_wilcox_test(
  data_discontinuous$pain_level_before_natrox,
  data_discontinuous$pain_level_after_natrox
)

cat("Results for continuous treatment:\n")
print(wilcox_continuous)

cat("Results for discontinuous treatment:\n")
print(wilcox_discontinuous)

###############################################################################
# 3. COMPARING THE EFFECTIVENESS OF PAIN REDUCTION BETWEEN GROUPS
###############################################################################
#
# We calculate the absolute difference in pain for each patient 
# (before - after) and compare them between continuous vs. discontinuous 
# groups via either a t-test or Mann-Whitney U, depending on normality 
# and variance homogeneity.
#

# Calculate the difference in pain (before - after)
calculate_diff <- function(df) {
  df$diff_pain <- as.numeric(df$pain_level_before_natrox) - 
    as.numeric(df$pain_level_after_natrox)
  return(df)
}

data_continuous    <- calculate_diff(data_continuous)
data_discontinuous <- calculate_diff(data_discontinuous)

# Descriptive summaries
summarize_differences <- function(df) {
  list(
    summary = summary(df$diff_pain),
    mean    = mean(df$diff_pain),
    sd      = sd(df$diff_pain)
  )
}

summary_continuous     <- summarize_differences(data_continuous)
summary_discontinuous  <- summarize_differences(data_discontinuous)

cat("\nDescriptive summary - Continuous treatment:\n")
print(summary_continuous)

cat("\nDescriptive summary - Discontinuous treatment:\n")
print(summary_discontinuous)

# Normality tests and variance test
shapiro_continuous     <- shapiro.test(data_continuous$diff_pain)
shapiro_discontinuous  <- shapiro.test(data_discontinuous$diff_pain)
var_test               <- var.test(data_continuous$diff_pain, data_discontinuous$diff_pain)

cat("\nNormality test - Continuous group:\n")
print(shapiro_continuous)

cat("\nNormality test - Discontinuous group:\n")
print(shapiro_discontinuous)

cat("\nVariance homogeneity test:\n")
print(var_test)

# Decide between t-test or Mann-Whitney U
if (
  shapiro_continuous$p.value > 0.05 && 
  shapiro_discontinuous$p.value > 0.05 && 
  var_test$p.value > 0.05
) {
  t_test <- t.test(
    x         = data_continuous$diff_pain,
    y         = data_discontinuous$diff_pain,
    alternative = "two.sided",
    var.equal = TRUE
  )
  cat("\nResults of t-test (equal variances):\n")
  print(t_test)
} else {
  mann_whitney_test <- wilcox.test(
    x          = data_continuous$diff_pain,
    y          = data_discontinuous$diff_pain,
    alternative = "two.sided",
    exact      = FALSE
  )
  cat("\nResults of Mann-Whitney U test:\n")
  print(mann_whitney_test)
}

###############################################################################
# 4. CALCULATE THE PROPORTION OF "NO PAIN"
###############################################################################
#
# We also look at the proportion of patients who end up with "No Pain" 
# after treatment (overall, continuous, and discontinuous). Additionally, 
# we run a Fisher's exact test on a 2x2 contingency table comparing 
# "No Pain" vs "Other" in both treatment groups.
#

calculate_proportion <- function(df) {
  counts <- table(df$pain_level_after_natrox)
  proportion_no_pain <- counts["No Pain"] / sum(counts)
  return(proportion_no_pain)
}

prop_continuous    <- calculate_proportion(data_continuous)
prop_discontinuous <- calculate_proportion(data_discontinuous)
prop_general       <- calculate_proportion(data)

# Build a contingency table for Fisher's exact test
contingency_table <- matrix(
  c(
    table(data_continuous$pain_level_after_natrox)["No Pain"],
    sum(table(data_continuous$pain_level_after_natrox)) - 
      table(data_continuous$pain_level_after_natrox)["No Pain"],
    table(data_discontinuous$pain_level_after_natrox)["No Pain"],
    sum(table(data_discontinuous$pain_level_after_natrox)) - 
      table(data_discontinuous$pain_level_after_natrox)["No Pain"]
  ),
  nrow = 2,
  byrow = TRUE
)

colnames(contingency_table) <- c("No Pain", "Other")
rownames(contingency_table) <- c("Continuous", "Discontinuous")

cat("\nContingency table:\n")
print(contingency_table)

fisher_test <- fisher.test(contingency_table)

cat("\nProportion of 'No Pain' - Continuous group:\n")
print(prop_continuous)

cat("\nProportion of 'No Pain' - Discontinuous group:\n")
print(prop_discontinuous)

cat("\nFisher's exact test results:\n")
print(fisher_test)

###############################################################################
# 4A. SANKEY PLOTS FOR EACH TREATMENT GROUP
###############################################################################
#
# We generate two separate Sankey plots (one for the continuous group, 
# one for the discontinuous group) and display their respective Wilcoxon 
# p-values.
#

# Format p-values for each subgroup Wilcoxon test
p_value_continuous    <- formatC(wilcox_continuous$p.value,    format = "e", digits = 2)
p_value_discontinuous <- formatC(wilcox_discontinuous$p.value, format = "e", digits = 2)

# Build data for Sankey plots
prepare_sankey_data <- function(df, group_name) {
  data.frame(
    Before = df$pain_level_before_natrox,
    After  = df$pain_level_after_natrox,
    Group  = group_name
  )
}

sankey_continuous     <- prepare_sankey_data(data_continuous,    "Continuous")
sankey_discontinuous  <- prepare_sankey_data(data_discontinuous, "Discontinuous")

# Reverse factor levels for both sets
sankey_continuous$Before <- factor(
  sankey_continuous$Before, 
  levels = rev(levels(sankey_continuous$Before))
)
sankey_continuous$After  <- factor(
  sankey_continuous$After,  
  levels = rev(levels(sankey_continuous$After))
)

sankey_discontinuous$Before <- factor(
  sankey_discontinuous$Before, 
  levels = rev(levels(sankey_discontinuous$Before))
)
sankey_discontinuous$After  <- factor(
  sankey_discontinuous$After,  
  levels = rev(levels(sankey_discontinuous$After))
)

# Sankey for the continuous group
plot_continuous <- ggplot(sankey_continuous, aes(axis1 = Before, axis2 = After)) +
  geom_alluvium(aes(fill = Before), width = 1/12) +
  geom_stratum(aes(fill = after_stat(stratum)), width = 1/8, color = NA) +
  scale_x_discrete(limits = c("Before Natrox", "After Natrox"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = pain_colors) +
  labs(x = NULL, y = NULL, fill = "Pain Level") +
  theme_void() +
  theme(
    legend.position = "top",
    plot.margin = unit(c(0.5, 0.5, 0.1, 0.5), "cm")
  ) +
  # Labels and p-value
  annotate("text", x = 1,   y = -1.5, label = "Before Natrox", size = 3.5, fontface = "plain", hjust = 0.5) +
  annotate("text", x = 2,   y = -1.5, label = "After Natrox",  size = 3.5, fontface = "plain", hjust = 0.5) +
  annotate("text", x = 1.5, y = -1.5, label = paste("p-value:", p_value_continuous), 
           size = 3.5, fontface = "italic", hjust = 0.5)

# Sankey for the discontinuous group
plot_discontinuous <- ggplot(sankey_discontinuous, aes(axis1 = Before, axis2 = After)) +
  geom_alluvium(aes(fill = Before), width = 1/12) +
  geom_stratum(aes(fill = after_stat(stratum)), width = 1/8, color = NA) +
  scale_x_discrete(limits = c("Before Natrox", "After Natrox"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = pain_colors) +
  labs(x = NULL, y = NULL, fill = "Pain Level") +
  theme_void() +
  theme(
    legend.position = "top",
    plot.margin = unit(c(0.5, 0.5, 0.1, 0.5), "cm")
  ) +
  # Labels and p-value
  annotate("text", x = 1,   y = -1.5, label = "Before Natrox", size = 3.5, fontface = "plain", hjust = 0.5) +
  annotate("text", x = 2,   y = -1.5, label = "After Natrox",  size = 3.5, fontface = "plain", hjust = 0.5) +
  annotate("text", x = 1.5, y = -1.5, label = paste("p-value:", p_value_discontinuous),
           size = 3.5, fontface = "italic", hjust = 0.5)

# Print the two Sankey plots
print(plot_continuous)
print(plot_discontinuous)

###############################################################################
# 5. BINARY LOGISTIC REGRESSION BY CLINICAL VARIABLES
###############################################################################
#
# We transform the outcome into a binary variable ("No Pain" vs. "Pain") 
# and run a logistic regression for each clinical condition to estimate 
# the odds ratio (OR) of having "No Pain".
#

# Transform the dependent variable into a binary outcome
# "No Pain" vs. "Pain" (where "Pain" combines Low, Moderate, High)
data$pain_binary <- ifelse(data$pain_level_after_natrox == "No Pain", "No Pain", "Pain")
data$pain_binary <- factor(data$pain_binary, levels = c("No Pain", "Pain"))

# List of clinical variables to assess
clinical_variables <- c(
  "diabetes_type_2",
  "hypertension",
  "chronic_renal_failure",
  "venous_insufficiency",
  "arterial_insufficiency",
  "lymphedema",
  "anaemia"
)

# Create a dataframe to store summarized results
binaryResults <- data.frame(
  Variable   = character(),
  OR         = numeric(),
  `2.5% CI`  = numeric(),
  `97.5% CI` = numeric(),
  `p-value`  = numeric(),
  stringsAsFactors = FALSE
)

# Fit a logistic regression model for each clinical variable
for (variable in clinical_variables) {
  try({
    # Define the model formula
    formula <- as.formula(paste("pain_binary ~", variable))
    
    # Fit the logistic regression
    model <- glm(formula, data = data, family = binomial)
    
    # Summary of the model
    summary_model <- summary(model)
    
    # Compute odds ratios and confidence intervals
    odds_ratios   <- exp(coef(model))
    conf_intervals <- exp(confint(model))
    
    # Extract p-values
    p_values <- summary_model$coefficients[, "Pr(>|z|)"]
    
    # Append results for this variable
    binaryResults <- rbind(
      binaryResults,
      data.frame(
        Variable   = variable,
        OR         = odds_ratios[2],
        `2.5% CI`  = conf_intervals[2, 1],
        `97.5% CI` = conf_intervals[2, 2],
        `p-value`  = p_values[2],
        stringsAsFactors = FALSE
      )
    )
    
    # Print detailed results to console
    cat("\nResults for", variable, ":\n")
    print(summary_model)
    cat("Odds Ratios:\n")
    print(odds_ratios)
    cat("Confidence Intervals (exp):\n")
    print(conf_intervals)
    
  }, silent = TRUE)  # If any variable causes an error, skip gracefully
}


# Optionally, you can save the results as a CSV file:
# write.csv(binaryResults, "binaryResults.csv", row.names = FALSE)


###############################################################################
# 6.1 SEPARATING THE WOUND DURATION (MONTHS) INTO TERTILES
###############################################################################
#
# In this section, we partition the "duration_of_wound_before_NATROX_months"
# variable into three roughly equal groups (tertiles), which will allow us
# to explore whether the wound duration before treatment influences 
# wound size reduction differently.

# Calculate tertiles (33.3% and 66.6% percentiles) from the data
tertiles <- quantile(
  data$duration_of_wound_before_NATROX_months, 
  probs = c(1/3, 2/3), 
  na.rm = TRUE
)

# Print the tertile cut-off values
print(tertiles)

# Classify each observation into tertiles based on the duration
data$durationBeforeNatroxTertile <- cut(
  data$duration_of_wound_before_NATROX_months,
  breaks = c(-Inf, tertiles[1], tertiles[2], Inf),
  labels = c("T1", "T2", "T3"),
  include.lowest = TRUE
)

# Show frequency in each tertile
table(data$durationBeforeNatroxTertile)


###############################################################################
# 6.2 SEPARATING THE TREATMENT DURATION (WEEKS) INTO TERTILES
###############################################################################
#
# In this section, we partition the "treatment_duration_weeks"
# variable into three roughly equal groups (tertiles), which will allow us
# to explore whether the duration of treatment influences 
# wound size reduction differently.

# Calculate tertiles (33.3% and 66.6% percentiles) from the data
tertiles_treatment <- quantile(
  data$treatment_duration_weeks, 
  probs = c(1/3, 2/3), 
  na.rm = TRUE
)

# Print the tertile cut-off values
print(tertiles_treatment)

# Classify each observation into tertiles based on the treatment duration
data$treatmentDurationTertile <- cut(
  data$treatment_duration_weeks,
  breaks = c(-Inf, tertiles_treatment[1], tertiles_treatment[2], Inf),
  labels = c("T1", "T2", "T3"),
  include.lowest = TRUE
)

# Show frequency in each tertile
table(data$treatmentDurationTertile)


###############################################################################
# 7. ANALYSIS OF WOUND SIZE REDUCTION (GENERAL)
###############################################################################
#
# We compare initial vs final measurements of wound (length, width, area)
# for the entire dataset. For each variable:
#   1) We compute the difference (Initial - Final).
#   2) Check normality with Shapiro-Wilk.
#   3) Depending on normality, run paired t-test or Wilcoxon signed-rank test.

# Define pairs of initial and final variables for comparison
variables <- list(
  c("Initial_length_cm", "final_length_cm"),
  c("Initial_width_cm",  "final_width_cm"),
  c("Initial_area_cm2",  "final_area_cm2")
)

# Create new columns for differences (Initial - Final)
for (var in variables) {
  initial_var <- var[1]
  final_var   <- var[2]
  diff_var    <- paste0("diff_", initial_var)  # e.g. diff_Initial_length_cm
  
  data[[diff_var]] <- data[[initial_var]] - data[[final_var]]
}

# Perform normality tests and paired tests for each difference variable
for (var in variables) {
  initial_var <- var[1]
  final_var   <- var[2]
  diff_var    <- paste0("diff_", initial_var)
  
  # Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(data[[diff_var]])
  
  cat("\n=====================================\n")
  cat("Analyzing:", diff_var, "\n")
  print(shapiro_test)
  
  # If normally distributed, use paired t-test; otherwise Wilcoxon test
  if (shapiro_test$p.value > 0.05) {
    t_test <- t.test(data[[initial_var]], data[[final_var]], paired = TRUE)
    cat("\nUsing Paired t-test:\n")
    print(t_test)
  } else {
    wilcox_test <- wilcox.test(data[[initial_var]], data[[final_var]], paired = TRUE)
    cat("\nUsing Wilcoxon Signed-Rank Test:\n")
    print(wilcox_test)
  }
  cat("=====================================\n")
}


###############################################################################
# 8.1 ANALYSIS OF WOUND SIZE REDUCTION BY TERTILE
#     (durationBeforeNatroxTertile)
###############################################################################
#
# Similar to above, but we now break it down by each tertile of 
# "durationBeforeNatroxTertile". Each subgroup is tested separately.

# Same variable pairs as before
variables <- list(
  c("Initial_length_cm", "final_length_cm"),
  c("Initial_width_cm",  "final_width_cm"),
  c("Initial_area_cm2",  "final_area_cm2")
)

# Loop over each tertile of wound duration
for (tertile in levels(data$durationBeforeNatroxTertile)) {
  cat("\n\n#############################################\n")
  cat("ANALYZING TERTILE:", tertile, "\n")
  cat("#############################################\n\n")
  
  # Subset data for the current tertile
  subset_data <- data[data$durationBeforeNatroxTertile == tertile, ]
  
  # Perform the analysis for each (Initial, Final) pair
  for (var in variables) {
    initial_var <- var[1]
    final_var   <- var[2]
    diff_var    <- paste0("diff_", initial_var)
    
    # Compute differences specifically for this tertile subset
    subset_data[[diff_var]] <- subset_data[[initial_var]] - subset_data[[final_var]]
    
    # Test normality on the difference
    shapiro_test <- shapiro.test(subset_data[[diff_var]])
    
    cat("\n=====================================\n")
    cat("Tertile:", tertile, "| Variable:", diff_var, "\n")
    print(shapiro_test)
    
    # Choose paired t-test or Wilcoxon test
    if (shapiro_test$p.value > 0.05) {
      t_test <- t.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)
      cat("\nUsing Paired t-test:\n")
      print(t_test)
    } else {
      wilcox_test <- wilcox.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)
      cat("\nUsing Wilcoxon Signed-Rank Test:\n")
      print(wilcox_test)
    }
    cat("=====================================\n")
  }
}


###############################################################################
# 8.2 PLOTTING WOUND SIZE RESULTS BY TERTILE (durationBeforeNatroxTertile)
###############################################################################
#
# Example code for generating bar plots (with error bars) of wound size 
# differences by tertile. It assumes you have some form of summarized data 
# (e.g., plot_summary) containing Mean, SE, Timepoint, etc.
#

# Summarize data: Calculate mean and standard error for each tertile & timepoint
plot_summary <- data %>%
  pivot_longer(cols = c("Initial_length_cm", "final_length_cm", 
                        "Initial_width_cm", "final_width_cm",
                        "Initial_area_cm2", "final_area_cm2"),
               names_to = "Measurement", values_to = "Value") %>%
  mutate(Timepoint = ifelse(grepl("Initial", Measurement), "Initial", "Final"),
         Measurement = gsub("Initial_|final_", "", Measurement)) %>%
  group_by(durationBeforeNatroxTertile, Measurement, Timepoint) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            SE = sd(Value, na.rm = TRUE) / sqrt(n()), .groups = "drop")  # Standard Error

# Create a list of statistical test results
stat_tests <- list()

# Compute p-values for each tertile and variable
for (tertile in unique(data$durationBeforeNatroxTertile)) {
  for (var in c("Initial_length_cm", "Initial_width_cm", "Initial_area_cm2")) {
    
    initial_var <- var
    final_var <- gsub("Initial", "final", var)  # Match final variable name
    subset_data <- data[data$durationBeforeNatroxTertile == tertile, ]
    
    # Check normality for correct test selection
    shapiro_test <- shapiro.test(subset_data[[initial_var]] - subset_data[[final_var]])
    
    if (shapiro_test$p.value > 0.05) {
      # Normal → Paired t-test
      p_value <- t.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)$p.value
    } else {
      # Non-normal → Wilcoxon signed-rank test
      p_value <- wilcox.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)$p.value
    }
    
    # Convert p-value to significance levels
    Significance <- case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
    
    # Store statistical results for later annotation
    stat_tests <- append(stat_tests, list(
      data.frame(durationBeforeNatroxTertile = tertile, Measurement = gsub("Initial_", "", var),
                 Timepoint1 = "Initial", Timepoint2 = "Final",
                 p_value = p_value, Significance = Significance)
    ))
  }
}

# Combine all p-values into a single dataframe
stat_tests_df <- bind_rows(stat_tests)


# Function that creates a plot for a specific measurement 
# (e.g., "area_cm2", "length_cm", or "width_cm")
create_individual_plot <- function(measurement_name, y_ticks) {
  
  # 1) Filter summary data for this measurement
  plot_data <- plot_summary %>% 
    filter(Measurement == measurement_name)
  
  # 2) Ensure "Initial" always appears left, "Final" right
  plot_data <- plot_data %>% 
    mutate(Timepoint = factor(Timepoint, levels = c("Initial", "Final")))
  
  # 3) Dynamically set vertical position for significance markers
  plot_data <- plot_data %>%
    group_by(durationBeforeNatroxTertile) %>%
    mutate(y_position = max(Mean + SE) + 0.1 * max(Mean + SE)) %>%
    ungroup()
  
  # 4) Build the plot
  p <- ggplot(plot_data, aes(x = durationBeforeNatroxTertile, y = Mean, fill = Timepoint)) +
    geom_bar(
      stat = "identity",
      position = position_dodge(width = 0.7, preserve = "single"),
      alpha = 0.8
    ) +
    geom_errorbar(
      aes(ymin = Mean - SE, ymax = Mean + SE),
      position = position_dodge(width = 0.7, preserve = "single"),
      width = 0.2
    ) +
    # Horizontal lines for reference
    geom_hline(yintercept = 0, color = "black", size = 0.4) +
    geom_hline(yintercept = y_ticks, linetype = "dashed", color = "gray60", size = 0.3) +
    # Axis, legend, theme
    labs(title = NULL, x = NULL, y = NULL, fill = "Timepoint") +
    theme_minimal() +
    theme(
      axis.text.x     = element_text(size = 10, family = "Arial", face = "plain"),
      axis.text.y     = element_text(size = 10, family = "Arial", face = "plain"),
      strip.text.x    = element_text(size = 11, family = "Arial", face = "plain"),
      legend.text     = element_text(size = 10, family = "Arial"),
      legend.title    = element_text(size = 11, family = "Arial", face = "plain"),
      panel.grid      = element_blank()
    ) +
    scale_fill_manual(values = c("Initial" = "#E69F00", "Final" = "#56B4E9")) +
    # Significance markers (asterisks)
    geom_text(
      aes(y = y_position, label = Significance),
      size = 4, fontface = "plain", color = "black"
    ) +
    # Significance lines
    geom_segment(
      aes(x = as.numeric(factor(durationBeforeNatroxTertile)) - 0.2,
          xend = as.numeric(factor(durationBeforeNatroxTertile)) + 0.2,
          y = y_position - 0.02 * max(Mean + SE),
          yend = y_position - 0.02 * max(Mean + SE)),
      color = "black", size = 0.3
    )
  
  return(p)
}

# Example usage: generating separate plots for area, length, and width
plot_area   <- create_individual_plot("area_cm2",   y_ticks = c(25, 50, 75))
plot_length <- create_individual_plot("length_cm",  y_ticks = c(2.5, 5, 7.5))
plot_width  <- create_individual_plot("width_cm",   y_ticks = c(2.5, 5, 7.5))

# Display the plots
print(plot_area)
print(plot_length)
print(plot_width)


##############################################################################
# 9.1 ANALYSIS OF WOUND SIZE REDUCTION BY TERTILE (treatmentDurationTertile)
##############################################################################
#
# Similar to above, but we now break it down by each tertile of 
# "treatmentDurationTertile". Each subgroup is tested separately.

# Same variable pairs as before
variables <- list(
  c("Initial_length_cm", "final_length_cm"),
  c("Initial_width_cm",  "final_width_cm"),
  c("Initial_area_cm2",  "final_area_cm2")
)

# Loop over each tertile of treatment duration
for (tertile in levels(data$treatmentDurationTertile)) {
  cat("\n\n#############################################\n")
  cat("ANALYZING TERTILE:", tertile, "\n")
  cat("#############################################\n\n")
  
  # Subset data for the current tertile
  subset_data <- data[data$treatmentDurationTertile == tertile, ]
  
  # Perform the analysis for each (Initial, Final) pair
  for (var in variables) {
    initial_var <- var[1]
    final_var   <- var[2]
    diff_var    <- paste0("diff_", initial_var)
    
    # Compute differences specifically for this tertile subset
    subset_data[[diff_var]] <- subset_data[[initial_var]] - subset_data[[final_var]]
    
    # Test normality on the difference
    shapiro_test <- shapiro.test(subset_data[[diff_var]])
    
    cat("\n=====================================\n")
    cat("Tertile:", tertile, "| Variable:", diff_var, "\n")
    print(shapiro_test)
    
    # Choose paired t-test or Wilcoxon test
    if (shapiro_test$p.value > 0.05) {
      t_test <- t.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)
      cat("\nUsing Paired t-test:\n")
      print(t_test)
    } else {
      wilcox_test <- wilcox.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)
      cat("\nUsing Wilcoxon Signed-Rank Test:\n")
      print(wilcox_test)
    }
    cat("=====================================\n")
  }
}


###############################################################################
# 9.2 PLOTTING WOUND SIZE RESULTS BY TERTILE (treatmentDurationTertile)
###############################################################################
#
# Example code for generating bar plots (with error bars) of wound size 
# differences by tertile (treatmentDurationTertile). It uses a `plot_summary` 
# containing Mean, SE, Timepoint, etc.
#

# Summarize data: Calculate mean and standard error for each tertile & timepoint
plot_summary <- data %>%
  pivot_longer(cols = c("Initial_length_cm", "final_length_cm", 
                        "Initial_width_cm", "final_width_cm",
                        "Initial_area_cm2", "final_area_cm2"),
               names_to = "Measurement", values_to = "Value") %>%
  mutate(Timepoint = ifelse(grepl("Initial", Measurement), "Initial", "Final"),
         Measurement = gsub("Initial_|final_", "", Measurement)) %>%
  group_by(treatmentDurationTertile, Measurement, Timepoint) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE   = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create a list of statistical test results
stat_tests <- list()

# Compute p-values for each tertile and each wound-size variable
for (tertile in unique(data$treatmentDurationTertile)) {
  for (var in c("Initial_length_cm", "Initial_width_cm", "Initial_area_cm2")) {
    
    initial_var <- var
    final_var   <- gsub("Initial", "final", var)  # Match final variable name
    subset_data <- data[data$treatmentDurationTertile == tertile, ]
    
    # Check normality for correct test selection
    shapiro_test <- shapiro.test(subset_data[[initial_var]] - subset_data[[final_var]])
    
    if (shapiro_test$p.value > 0.05) {
      # Normal → Paired t-test
      p_value <- t.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)$p.value
    } else {
      # Non-normal → Wilcoxon signed-rank test
      p_value <- wilcox.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)$p.value
    }
    
    # Convert p-value to significance levels
    Significance <- dplyr::case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
    
    # Store statistical results for later annotation
    stat_tests <- append(stat_tests, list(
      data.frame(
        treatmentDurationTertile = tertile,
        Measurement = gsub("Initial_", "", var),
        Timepoint1  = "Initial",
        Timepoint2  = "Final",
        p_value     = p_value,
        Significance = Significance
      )
    ))
  }
}

# Combine all p-values into a single dataframe
stat_tests_df <- dplyr::bind_rows(stat_tests)


# Function that creates a plot for a specific measurement 
# (e.g., "area_cm2", "length_cm", or "width_cm")
create_individual_plot <- function(measurement_name, y_ticks) {
  
  # 1) Filter summary data for this measurement
  plot_data <- plot_summary %>% 
    filter(Measurement == measurement_name)
  
  # 2) Ensure "Initial" always appears left, "Final" right
  plot_data <- plot_data %>% 
    mutate(Timepoint = factor(Timepoint, levels = c("Initial", "Final")))
  
  # 3) Dynamically set vertical position for significance markers
  plot_data <- plot_data %>%
    group_by(treatmentDurationTertile) %>%
    mutate(y_position = max(Mean + SE) + 0.1 * max(Mean + SE)) %>%
    ungroup()
  
  # 4) Build the plot
  p <- ggplot(plot_data, aes(x = treatmentDurationTertile, y = Mean, fill = Timepoint)) +
    geom_bar(
      stat = "identity",
      position = position_dodge(width = 0.7, preserve = "single"),
      alpha = 0.8
    ) +
    geom_errorbar(
      aes(ymin = Mean - SE, ymax = Mean + SE),
      position = position_dodge(width = 0.7, preserve = "single"),
      width = 0.2
    ) +
    # Horizontal reference lines
    geom_hline(yintercept = 0, color = "black", size = 0.4) +
    geom_hline(yintercept = y_ticks, linetype = "dashed", color = "gray60", size = 0.3) +
    # Axis, legend, theme
    labs(title = NULL, x = NULL, y = NULL, fill = "Timepoint") +
    theme_minimal() +
    theme(
      axis.text.x     = element_text(size = 10, family = "Arial", face = "plain"),
      axis.text.y     = element_text(size = 10, family = "Arial", face = "plain"),
      strip.text.x    = element_text(size = 11, family = "Arial", face = "plain"),
      legend.text     = element_text(size = 10, family = "Arial"),
      legend.title    = element_text(size = 11, family = "Arial", face = "plain"),
      panel.grid      = element_blank()
    ) +
    scale_fill_manual(values = c("Initial" = "#E69F00", "Final" = "#56B4E9")) +
    # Significance markers (asterisks) – from computed y_position
    geom_text(
      aes(y = y_position, label = Significance),
      size = 4, fontface = "plain", color = "black"
    ) +
    # Significance lines
    geom_segment(
      aes(x = as.numeric(factor(treatmentDurationTertile)) - 0.2,
          xend = as.numeric(factor(treatmentDurationTertile)) + 0.2,
          y = y_position - 0.02 * max(Mean + SE),
          yend = y_position - 0.02 * max(Mean + SE)),
      color = "black", size = 0.3
    )
  
  return(p)
}

# Example usage: generating separate plots for area, length, and width
plot_area   <- create_individual_plot("area_cm2",   y_ticks = c(25, 50, 75))
plot_length <- create_individual_plot("length_cm",  y_ticks = c(3, 6, 9))
plot_width  <- create_individual_plot("width_cm",   y_ticks = c(2.5, 5, 7.5))

# Display the plots
print(plot_area)
print(plot_length)
print(plot_width)


###############################################################################
# 10. ANALYSIS OF WOUND SIZE REDUCTION BY TREATMENT CONTINUITY
###############################################################################
#
# Splits the data by "treatment_continuity" (e.g., "Continuous" vs. "Discontinuous")
# to analyze changes in initial vs. final wound measurements in each subgroup.

# Reuse the same variable pairs
variables <- list(
  c("Initial_length_cm", "final_length_cm"),
  c("Initial_width_cm",  "final_width_cm"),
  c("Initial_area_cm2",  "final_area_cm2")
)

# Initialize a data frame to store p-values and significance
stat_tests_treatment <- data.frame()

# Loop over each treatment continuity group
for (continuity in unique(data$treatment_continuity)) {
  cat("\n\n#############################################\n")
  cat("Analyzing for Treatment Continuity:", continuity, "\n")
  cat("#############################################\n\n")
  
  # Subset data for the current continuity group
  subset_data <- data[data$treatment_continuity == continuity, ]
  
  # For each (Initial, Final) pair, run normality + appropriate test
  for (var in variables) {
    initial_var <- var[1]
    final_var   <- var[2]
    diff_var    <- paste0("diff_", initial_var)
    
    # Compute differences for this subgroup
    subset_data[[diff_var]] <- subset_data[[initial_var]] - subset_data[[final_var]]
    
    shapiro_test <- shapiro.test(subset_data[[diff_var]])
    
    cat("\n=====================================\n")
    cat("Treatment Continuity:", continuity, "| Variable:", diff_var, "\n")
    print(shapiro_test)
    
    # Paired t-test if normal; else Wilcoxon
    if (shapiro_test$p.value > 0.05) {
      t_test <- t.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)
      cat("\nUsing Paired t-test:\n")
      print(t_test)
      p_value <- t_test$p.value
    } else {
      wilcox_test <- wilcox.test(subset_data[[initial_var]], subset_data[[final_var]], paired = TRUE)
      cat("\nUsing Wilcoxon Signed-Rank Test:\n")
      print(wilcox_test)
      p_value <- wilcox_test$p.value
    }
    
    # Store the results
    stat_tests_treatment <- rbind(
      stat_tests_treatment,
      data.frame(
        treatment_continuity = continuity,
        Measurement          = gsub("Initial_", "", initial_var),
        p_value              = p_value
      )
    )
    cat("=====================================\n")
  }
}

# Assign significance levels to p-values
stat_tests_treatment$Significance <- cut(
  stat_tests_treatment$p_value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "ns")
)


###############################################################################
# 11. EFFECT SIZE CALCULATIONS
###############################################################################
#
# We compute two types of effect sizes:
#   1) Cramér's V for the contingency table of pain levels (before vs. after).
#   2) Cohen's d or Wilcoxon r for the wound size changes, depending on normality.

# 11A. Cramér's V for PAIN LEVEL
# Create a contingency table of pain levels (before vs. after)
pain_table <- table(data$pain_level_before_natrox, data$pain_level_after_natrox)

# Compute Cramér’s V (with a simulated p-value)
cramers_v_result <- cramerV(pain_table, simulate.p.value = TRUE)
cat("\nCramér's V for pain level transitions:\n")
print(cramers_v_result)


# 11B. Effect Sizes (WOUND SIZE) - Cohen's d or Wilcoxon r
# Define pairs of initial and final variables again
variable_pairs <- list(
  c("Initial_length_cm", "final_length_cm"),
  c("Initial_width_cm",  "final_width_cm"),
  c("Initial_area_cm2",  "final_area_cm2")
)

# Prepare an output data frame
effect_size_results <- data.frame(
  Variable        = character(),
  Test            = character(),
  Effect_Size     = numeric(),
  Interpretation  = character(),
  stringsAsFactors = FALSE
)

# Loop through each pair (Initial vs Final)
for (vars in variable_pairs) {
  initial_var <- vars[1]
  final_var   <- vars[2]
  
  # Compute the difference
  differences <- data[[initial_var]] - data[[final_var]]
  
  # Normality check
  shapiro_test <- shapiro.test(differences)
  
  if (shapiro_test$p.value > 0.05) {
    # If normally distributed, use Cohen's d
    effect_size_info <- cohen.d(data[[initial_var]], data[[final_var]], paired = TRUE)
    effect_size <- effect_size_info$estimate
    test_used   <- "Cohen's d"
    
    # Interpretation rules of thumb for Cohen's d
    interpretation <- ifelse(
      effect_size < 0.2, "Very Small",
      ifelse(effect_size < 0.5, "Small",
             ifelse(effect_size < 0.8, "Medium", "Large")
      )
    )
    
  } else {
    # If not normally distributed, use Wilcoxon effect size r
    wilcox_test <- wilcox.test(data[[initial_var]], data[[final_var]], paired = TRUE)
    
    # Approx. effect size r = |z| / sqrt(n)
    # Convert the p-value to z-statistic via qnorm(p/2)
    r_effect_size <- abs(qnorm(wilcox_test$p.value / 2) / sqrt(nrow(data)))
    effect_size   <- r_effect_size
    test_used     <- "Wilcoxon r"
    
    # Interpretation rules of thumb for r
    interpretation <- ifelse(
      effect_size < 0.1, "Very Small",
      ifelse(effect_size < 0.3, "Small",
             ifelse(effect_size < 0.5, "Medium", "Large")
      )
    )
  }
  
  # Save results to the data frame
  effect_size_results <- rbind(
    effect_size_results,
    data.frame(
      Variable       = initial_var,
      Test           = test_used,
      Effect_Size    = effect_size,
      Interpretation = interpretation,
      stringsAsFactors = FALSE
    )
  )
}

cat("\nEffect size results (Cohen's d or Wilcoxon r):\n")
print(effect_size_results)
