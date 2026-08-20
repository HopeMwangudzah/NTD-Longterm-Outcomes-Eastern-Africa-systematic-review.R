# ---------------------------------------------------------------
# SENSITIVITY ANALYSIS — Attrition Bias Assessment
# ---------------------------------------------------------------

rm(list = ls())
cat("\014")
gc()

library(meta)
library(dplyr)
library(readxl)
library(knitr)

# -------------------------------
# 1. Paths
# -------------------------------
data_path <- "/Users/or24054/Downloads/SRM_Ntds_Hope.xlsx"
output_dir <- file.path(dirname(data_path), "Sensitivity_Analysis")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -------------------------------
# 2. Read data
# -------------------------------
data <- readxl::read_excel(data_path)
colnames(data) <- trimws(colnames(data))

data <- data %>%
  rename(
    Outcome = Outcome,
    Age_group = `Age group`,
    Study_ID = `Study id`,
    Events = Events,
    Total = Total
  ) %>%
  filter(!is.na(Outcome), !is.na(Events), !is.na(Total))

# Check for baseline data columns
if ("Events_baseline" %in% colnames(data)) {
  data <- data %>% rename(Events_baseline = Events_baseline)
} else {
  data$Events_baseline <- NA
}

if ("Total_baseline" %in% colnames(data)) {
  data <- data %>% rename(Total_baseline = Total_baseline)
} else {
  data$Total_baseline <- NA
}

if ("Attrition_pct" %in% colnames(data)) {
  data <- data %>% rename(Attrition_pct = Attrition_pct)
} else {
  data$Attrition_pct <- NA
}

# -------------------------------
# 3. Function to run sensitivity analysis
# -------------------------------
run_sensitivity <- function(outcome_name, data_all, output_dir, threshold = 30) {
  
  message("\n========================================")
  message("SENSITIVITY ANALYSIS: ", outcome_name)
  message("========================================")
  
  outcome_data <- data_all %>% filter(Outcome == outcome_name)
  
  if (nrow(outcome_data) < 2) {
    message("Not enough data for ", outcome_name)
    return(NULL)
  }
  
  # Check for attrition data
  attrition_data <- outcome_data %>%
    filter(!is.na(Events_baseline), !is.na(Total_baseline), !is.na(Attrition_pct))
  
  if (nrow(attrition_data) == 0) {
    message("No attrition data for ", outcome_name)
    return(NULL)
  }
  
  message("\nStudies with attrition data:")
  print(attrition_data %>% select(Study_ID, Events, Total, Events_baseline, Total_baseline, Attrition_pct))
  
  high_attrition <- attrition_data %>%
    filter(Attrition_pct > threshold) %>%
    pull(Study_ID)
  
  if (length(high_attrition) == 0) {
    message("\nNo studies exceed ", threshold, "% attrition")
    return(NULL)
  }
  
  message("\nStudies >", threshold, "% attrition: ", paste(high_attrition, collapse = ", "))
  
  # PRIMARY ANALYSIS (Follow-up data)
  message("\n--- Analysis 1: Primary (Follow-up) ---")
  meta_primary <- metaprop(
    event = Events,
    n = Total,
    studlab = Study_ID,
    data = outcome_data,
    sm = "PLOGIT",
    method.tau = "ML",
    random = TRUE,
    fixed = FALSE
  )
  
  pooled_primary <- exp(meta_primary$TE.random) / (1 + exp(meta_primary$TE.random))
  lower_primary <- exp(meta_primary$lower.random) / (1 + exp(meta_primary$lower.random))
  upper_primary <- exp(meta_primary$upper.random) / (1 + exp(meta_primary$upper.random))
  
  message("Pooled: ", round(pooled_primary, 3), 
          " (95% CI: ", round(lower_primary, 3), "-", round(upper_primary, 3), ")")
  message("I²: ", round(meta_primary$I2, 1), "%")
  message("N studies: ", meta_primary$k)
  
  # SENSITIVITY 1: Exclude high attrition
  message("\n--- Analysis 2: Exclude >", threshold, "% attrition ---")
  outcome_data_excl <- outcome_data %>% filter(!Study_ID %in% high_attrition)
  
  if (nrow(outcome_data_excl) < 2) {
    message("Not enough studies after exclusion")
    meta_excl <- NULL
    pooled_excl <- NA
    lower_excl <- NA
    upper_excl <- NA
  } else {
    meta_excl <- metaprop(
      event = Events,
      n = Total,
      studlab = Study_ID,
      data = outcome_data_excl,
      sm = "PLOGIT",
      method.tau = "ML",
      random = TRUE,
      fixed = FALSE
    )
    
    pooled_excl <- exp(meta_excl$TE.random) / (1 + exp(meta_excl$TE.random))
    lower_excl <- exp(meta_excl$lower.random) / (1 + exp(meta_excl$lower.random))
    upper_excl <- exp(meta_excl$upper.random) / (1 + exp(meta_excl$upper.random))
    
    message("Pooled: ", round(pooled_excl, 3),
            " (95% CI: ", round(lower_excl, 3), "-", round(upper_excl, 3), ")")
    message("I²: ", round(meta_excl$I2, 1), "%")
    message("N studies: ", meta_excl$k)
  }
  
  # SENSITIVITY 2: Use baseline data
  message("\n--- Analysis 3: Use baseline data ---")
  outcome_data_baseline <- outcome_data %>%
    mutate(
      Events_final = ifelse(!is.na(Events_baseline), Events_baseline, Events),
      Total_final = ifelse(!is.na(Total_baseline), Total_baseline, Total)
    )
  
  meta_baseline <- metaprop(
    event = Events_final,
    n = Total_final,
    studlab = Study_ID,
    data = outcome_data_baseline,
    sm = "PLOGIT",
    method.tau = "ML",
    random = TRUE,
    fixed = FALSE
  )
  
  pooled_baseline <- exp(meta_baseline$TE.random) / (1 + exp(meta_baseline$TE.random))
  lower_baseline <- exp(meta_baseline$lower.random) / (1 + exp(meta_baseline$lower.random))
  upper_baseline <- exp(meta_baseline$upper.random) / (1 + exp(meta_baseline$upper.random))
  
  message("Pooled: ", round(pooled_baseline, 3),
          " (95% CI: ", round(lower_baseline, 3), "-", round(upper_baseline, 3), ")")
  message("I²: ", round(meta_baseline$I2, 1), "%")
  message("N studies: ", meta_baseline$k)
  
  baseline_used <- outcome_data_baseline %>%
    filter(!is.na(Events_baseline)) %>%
    pull(Study_ID)
  if (length(baseline_used) > 0) {
    message("Baseline data used for: ", paste(baseline_used, collapse = ", "))
  }
  
  # Comparison table
  comparison <- data.frame(
    Analysis = c("Primary (Follow-up)", 
                 paste0("Exclude >", threshold, "% attrition"),
                 "Use baseline data"),
    N_Studies = c(meta_primary$k,
                  ifelse(is.null(meta_excl), NA, meta_excl$k),
                  meta_baseline$k),
    Pooled_Proportion = c(round(pooled_primary, 3),
                          round(pooled_excl, 3),
                          round(pooled_baseline, 3)),
    Lower_CI = c(round(lower_primary, 3),
                 round(lower_excl, 3),
                 round(lower_baseline, 3)),
    Upper_CI = c(round(upper_primary, 3),
                 round(upper_excl, 3),
                 round(upper_baseline, 3)),
    I2 = c(round(meta_primary$I2, 1),
           ifelse(is.null(meta_excl), NA, round(meta_excl$I2, 1)),
           round(meta_baseline$I2, 1)),
    Difference_from_Primary = c(0,
                                round(pooled_excl - pooled_primary, 3),
                                round(pooled_baseline - pooled_primary, 3)),
    Absolute_Diff_pct = c(0,
                          round((pooled_excl - pooled_primary) * 100, 1),
                          round((pooled_baseline - pooled_primary) * 100, 1))
  )
  
  message("\n--- Comparison ---")
  print(kable(comparison, format = "simple"))
  
  # Interpretation
  message("\n--- INTERPRETATION ---")
  abs_diff <- abs(comparison$Absolute_Diff_pct[3])
  if (abs_diff < 5) {
    message("✅ ROBUST: <5% difference (", round(abs_diff, 1), "%)")
    interpretation <- "ROBUST"
  } else if (abs_diff < 10) {
    message("⚠️ MODERATE: 5-10% difference (", round(abs_diff, 1), "%)")
    interpretation <- "MODERATE"
  } else {
    message("❌ HIGH SENSITIVITY: >10% difference (", round(abs_diff, 1), "%)")
    interpretation <- "HIGH SENSITIVITY"
  }
  
  comparison$Robustness <- c(interpretation, interpretation, interpretation)
  
  # Save
  write.csv(comparison,
            file.path(output_dir, paste0(outcome_name, "_Sensitivity.csv")),
            row.names = FALSE)
  
  # Forest plots
  pdf(file.path(output_dir, paste0(outcome_name, "_Sensitivity_Plots.pdf")),
      width = 12, height = 10)
  
  par(mfrow = c(3, 1))
  
  forest(meta_primary,
         main = paste0(outcome_name, " - Primary (Follow-up)"),
         leftcols = c("studlab", "event", "n"),
         rightcols = c("effect", "ci"),
         digits = 2)
  
  if (!is.null(meta_excl)) {
    forest(meta_excl,
           main = paste0(outcome_name, " - Exclude High Attrition"),
           leftcols = c("studlab", "event", "n"),
           rightcols = c("effect", "ci"),
           digits = 2)
  }
  
  forest(meta_baseline,
         main = paste0(outcome_name, " - Baseline Data"),
         leftcols = c("studlab", "event", "n"),
         rightcols = c("effect", "ci"),
         digits = 2)
  
  dev.off()
  
  message("\n✅ Complete for ", outcome_name)
  
  return(comparison)
}

# -------------------------------
# 4. Run for outcomes with attrition data
# -------------------------------
outcomes_with_attrition <- data %>%
  filter(!is.na(Attrition_pct)) %>%
  pull(Outcome) %>%
  unique()

message("\n=== Outcomes with attrition data ===")
message(paste(outcomes_with_attrition, collapse = ", "))

results_list <- list()

for (outcome in outcomes_with_attrition) {
  result <- run_sensitivity(outcome, data, output_dir, threshold = 30)
  if (!is.null(result)) {
    results_list[[outcome]] <- result
  }
}

# -------------------------------
# 5. Combined summary
# -------------------------------
if (length(results_list) > 0) {
  message("\n========================================")
  message("COMBINED SUMMARY")
  message("========================================")
  
  combined <- do.call(rbind, lapply(names(results_list), function(outcome) {
    df <- results_list[[outcome]]
    df$Outcome <- outcome
    df[, c("Outcome", "Analysis", "N_Studies", "Pooled_Proportion", 
           "Lower_CI", "Upper_CI", "I2", "Absolute_Diff_pct", "Robustness")]
  }))
  
  write.csv(combined,
            file.path(output_dir, "Combined_Sensitivity_Summary.csv"),
            row.names = FALSE)
  
  message("\n--- Overall Robustness ---")
  for (outcome in names(results_list)) {
    baseline_diff <- results_list[[outcome]]$Absolute_Diff_pct[3]
    robustness <- results_list[[outcome]]$Robustness[1]
    message("  ", outcome, ": ", robustness, " (", round(baseline_diff, 1), "% change)")
  }
  
  message("\n✅ All analyses complete!")
  message("📁 Files saved in: ", output_dir)
} else {
  message("\n⚠️ No outcomes required sensitivity analysis")
}

message("\n========================================")
message("ANALYSIS COMPLETE")
message("========================================")