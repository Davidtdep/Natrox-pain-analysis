###############################################################################
# LOAD LIBRARIES
###############################################################################
library(ggalluvial)
library(ggplot2)
library(MASS)
library(dplyr)  # Needed for %>% pipes, mutate(), arrange(), etc.

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

