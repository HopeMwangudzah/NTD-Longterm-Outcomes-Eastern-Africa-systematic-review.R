# Clear everything
rm(list = ls())
cat("\014")

# Load packages
library(readxl)
library(dplyr)
library(officer)
library(flextable)
library(openxlsx)

# Setup
output_dir <- "~/Desktop/NTD_Summaries"
dir.create(output_dir, showWarnings = FALSE)

# Read data
file_path <- "/Users/or24054/Downloads/SRM_Ntds_Hope.xlsx"
data <- read_excel(file_path, sheet = "Age group")

cat("\n========== DATA LOADED ==========\n")
cat("Total rows:", nrow(data), "\n\n")

# Filter data
cat("========== FILTERING DATA ==========\n")
filtered_data <- data %>%
  filter(`NTD subtype` %in% c("Myelomeningocele", "Encephalocele")) %>%
  filter(
    (Outcome == "Mortality" & `Age group` == "13 months-2 years") |
      (Outcome == "Hydrocephalus")
  )

cat("Filtered rows:", nrow(filtered_data), "\n")
cat("Mortality (13 months-2 years):", sum(filtered_data$Outcome == "Mortality"), "\n")
cat("Hydrocephalus (all ages):", sum(filtered_data$Outcome == "Hydrocephalus"), "\n\n")

if(nrow(filtered_data) == 0) {
  stop("No data found after filtering!")
}

# Sum Events and Total by subtype and outcome
cat("========== SUMMING EVENTS AND TOTALS ==========\n")
summed_data <- filtered_data %>%
  group_by(`NTD subtype`, Outcome) %>%
  summarise(
    total_events = sum(Events, na.rm = TRUE),
    total_n = sum(Total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(percentage = round((total_events / total_n) * 100, 1))

print(summed_data)
cat("\n")

# Extract values for each group
mmc_mort <- summed_data %>% filter(`NTD subtype` == "Myelomeningocele" & Outcome == "Mortality")
enc_mort <- summed_data %>% filter(`NTD subtype` == "Encephalocele" & Outcome == "Mortality")
mmc_hydro <- summed_data %>% filter(`NTD subtype` == "Myelomeningocele" & Outcome == "Hydrocephalus")
enc_hydro <- summed_data %>% filter(`NTD subtype` == "Encephalocele" & Outcome == "Hydrocephalus")

# Build contingency table
cat("========== CHI-SQUARE TESTS ==========\n")

# Separate test for MORTALITY
cat("\n----- MORTALITY (13 months-2 years) -----\n")
mort_mmc_events <- ifelse(nrow(mmc_mort) > 0, mmc_mort$total_events, 0)
mort_mmc_nonevents <- ifelse(nrow(mmc_mort) > 0, mmc_mort$total_n - mmc_mort$total_events, 0)
mort_enc_events <- ifelse(nrow(enc_mort) > 0, enc_mort$total_events, 0)
mort_enc_nonevents <- ifelse(nrow(enc_mort) > 0, enc_mort$total_n - enc_mort$total_events, 0)

mortality_table <- matrix(
  c(mort_mmc_events, mort_enc_events, mort_mmc_nonevents, mort_enc_nonevents),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Events", "Non-events"),
    c("Myelomeningocele", "Encephalocele")
  )
)

print(mortality_table)

mortality_chi <- chisq.test(mortality_table)
cat("\nMortality Chi-Square (Q):", round(mortality_chi$statistic, 2), "\n")
cat("Mortality P-value:", round(mortality_chi$p.value, 4), "\n")
cat("Significant?:", ifelse(mortality_chi$p.value < 0.05, "YES", "NO"), "\n")

# Calculate I² for mortality
mort_studies <- filtered_data %>% filter(Outcome == "Mortality")
mort_num_studies <- nrow(mort_studies)
mort_df <- mort_num_studies - 1

if(mort_df > 0) {
  mort_i_squared <- max(0, ((mortality_chi$statistic - mort_df) / mortality_chi$statistic) * 100)
  cat("I² (heterogeneity):", round(mort_i_squared, 1), "%\n")
  mort_i2 <- round(mort_i_squared, 1)
} else {
  cat("I²: Cannot calculate (insufficient studies)\n")
  mort_i2 <- "N/A"
}

mort_min_expected <- min(mortality_chi$expected)
cat("Minimum expected count:", round(mort_min_expected, 2), "\n")

if(mort_min_expected < 5) {
  cat(">>> Using Fisher's Exact Test <<<\n")
  mortality_fisher <- fisher.test(mortality_table)
  cat("Fisher's P-value:", round(mortality_fisher$p.value, 4), "\n")
  mort_test_name <- "Fisher's Exact"
  mort_q <- "N/A"
  mort_p <- mortality_fisher$p.value
} else {
  mort_test_name <- "Chi-Square"
  mort_q <- round(mortality_chi$statistic, 2)
  mort_p <- mortality_chi$p.value
}

# Separate test for HYDROCEPHALUS
cat("\n----- HYDROCEPHALUS (all ages) -----\n")
hydro_mmc_events <- ifelse(nrow(mmc_hydro) > 0, mmc_hydro$total_events, 0)
hydro_mmc_nonevents <- ifelse(nrow(mmc_hydro) > 0, mmc_hydro$total_n - mmc_hydro$total_events, 0)
hydro_enc_events <- ifelse(nrow(enc_hydro) > 0, enc_hydro$total_events, 0)
hydro_enc_nonevents <- ifelse(nrow(enc_hydro) > 0, enc_hydro$total_n - enc_hydro$total_events, 0)

hydrocephalus_table <- matrix(
  c(hydro_mmc_events, hydro_enc_events, hydro_mmc_nonevents, hydro_enc_nonevents),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Events", "Non-events"),
    c("Myelomeningocele", "Encephalocele")
  )
)

print(hydrocephalus_table)

hydro_chi <- chisq.test(hydrocephalus_table)
cat("\nHydrocephalus Chi-Square (Q):", round(hydro_chi$statistic, 2), "\n")
cat("Hydrocephalus P-value:", round(hydro_chi$p.value, 4), "\n")
cat("Significant?:", ifelse(hydro_chi$p.value < 0.05, "YES", "NO"), "\n")

# Calculate I² for hydrocephalus
hydro_studies <- filtered_data %>% filter(Outcome == "Hydrocephalus")
hydro_num_studies <- nrow(hydro_studies)
hydro_df <- hydro_num_studies - 1

if(hydro_df > 0) {
  hydro_i_squared <- max(0, ((hydro_chi$statistic - hydro_df) / hydro_chi$statistic) * 100)
  cat("I² (heterogeneity):", round(hydro_i_squared, 1), "%\n")
  hydro_i2 <- round(hydro_i_squared, 1)
} else {
  cat("I²: Cannot calculate (insufficient studies)\n")
  hydro_i2 <- "N/A"
}

hydro_min_expected <- min(hydro_chi$expected)
cat("Minimum expected count:", round(hydro_min_expected, 2), "\n")

if(hydro_min_expected < 5) {
  cat(">>> Using Fisher's Exact Test <<<\n")
  hydro_fisher <- fisher.test(hydrocephalus_table)
  cat("Fisher's P-value:", round(hydro_fisher$p.value, 4), "\n")
  hydro_test_name <- "Fisher's Exact"
  hydro_q <- "N/A"
  hydro_p <- hydro_fisher$p.value
} else {
  hydro_test_name <- "Chi-Square"
  hydro_q <- round(hydro_chi$statistic, 2)
  hydro_p <- hydro_chi$p.value
}

cat("\n")

# Create summary table
summary_table <- data.frame(
  Outcome = c("Mortality (13m-2y)", "Hydrocephalus"),
  MMC_events = c(
    ifelse(nrow(mmc_mort) > 0, mmc_mort$total_events, 0),
    ifelse(nrow(mmc_hydro) > 0, mmc_hydro$total_events, 0)
  ),
  MMC_total = c(
    ifelse(nrow(mmc_mort) > 0, mmc_mort$total_n, 0),
    ifelse(nrow(mmc_hydro) > 0, mmc_hydro$total_n, 0)
  ),
  MMC_percent = c(
    ifelse(nrow(mmc_mort) > 0, mmc_mort$percentage, 0),
    ifelse(nrow(mmc_hydro) > 0, mmc_hydro$percentage, 0)
  ),
  Enceph_events = c(
    ifelse(nrow(enc_mort) > 0, enc_mort$total_events, 0),
    ifelse(nrow(enc_hydro) > 0, enc_hydro$total_events, 0)
  ),
  Enceph_total = c(
    ifelse(nrow(enc_mort) > 0, enc_mort$total_n, 0),
    ifelse(nrow(enc_hydro) > 0, enc_hydro$total_n, 0)
  ),
  Enceph_percent = c(
    ifelse(nrow(enc_mort) > 0, enc_mort$percentage, 0),
    ifelse(nrow(enc_hydro) > 0, enc_hydro$percentage, 0)
  ),
  Test = c(mort_test_name, hydro_test_name),
  Q_value = c(mort_q, hydro_q),
  I_squared = c(paste0(mort_i2, "%"), paste0(hydro_i2, "%")),
  P_value = c(round(mort_p, 4), round(hydro_p, 4)),
  Significant = c(
    ifelse(mort_p < 0.05, "YES", "NO"),
    ifelse(hydro_p < 0.05, "YES", "NO")
  )
)

cat("========== SUMMARY TABLE ==========\n")
print(summary_table)
cat("\n")

# Create Word document
cat("========== CREATING WORD DOCUMENT ==========\n")
doc <- read_docx()

doc <- doc %>%
  body_add_par("NTD Subtype Comparison: MMC vs Encephalocele", style = "heading 1") %>%
  body_add_par("") %>%
  body_add_par(paste("Date:", Sys.Date()), style = "Normal") %>%
  body_add_par("") %>%
  body_add_par("Statistical Test Results", style = "heading 2") %>%
  body_add_par("Mortality (13 months - 2 years):", style = "heading 3") %>%
  body_add_par(paste("Test:", mort_test_name), style = "Normal") %>%
  body_add_par(paste(ifelse(mort_test_name == "Chi-Square", paste("Chi-Square (Q):", mort_q), "Q: N/A")), style = "Normal") %>%
  body_add_par(paste("I² (Heterogeneity):", mort_i2, "%"), style = "Normal") %>%
  body_add_par(paste("P-value:", round(mort_p, 4)), style = "Normal") %>%
  body_add_par(paste("Significant:", ifelse(mort_p < 0.05, "YES", "NO")), style = "Normal") %>%
  body_add_par("") %>%
  body_add_par("Hydrocephalus (all ages):", style = "heading 3") %>%
  body_add_par(paste("Test:", hydro_test_name), style = "Normal") %>%
  body_add_par(paste(ifelse(hydro_test_name == "Chi-Square", paste("Chi-Square (Q):", hydro_q), "Q: N/A")), style = "Normal") %>%
  body_add_par(paste("I² (Heterogeneity):", hydro_i2, "%"), style = "Normal") %>%
  body_add_par(paste("P-value:", round(hydro_p, 4)), style = "Normal") %>%
  body_add_par(paste("Significant:", ifelse(hydro_p < 0.05, "YES", "NO")), style = "Normal") %>%
  body_add_par("")

comparison_for_word <- data.frame(
  Outcome = c("Mortality (13m-2y)", "Hydrocephalus"),
  Myelomeningocele = paste0(
    summary_table$MMC_percent, "% ",
    "(", summary_table$MMC_events, "/", summary_table$MMC_total, ")"
  ),
  Encephalocele = paste0(
    summary_table$Enceph_percent, "% ",
    "(", summary_table$Enceph_events, "/", summary_table$Enceph_total, ")"
  ),
  Test = summary_table$Test,
  Q = summary_table$Q_value,
  I_squared = summary_table$I_squared,
  P_value = summary_table$P_value
)

ft <- flextable(comparison_for_word) %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  width(j = 1, width = 1.8) %>%
  width(j = 2:3, width = 1.8) %>%
  width(j = 4, width = 1.2) %>%
  width(j = 5:7, width = 0.9) %>%
  bg(bg = "#E8F4F8", part = "header")

doc <- doc %>%
  body_add_par("Comparison Table", style = "heading 2") %>%
  body_add_flextable(ft)

word_file <- file.path(output_dir, "MMC_vs_Encephalocele_Summary.docx")
print(doc, target = word_file)
cat("Word saved:", word_file, "\n")

# Create Excel
cat("\n========== CREATING EXCEL ==========\n")
wb <- createWorkbook()

addWorksheet(wb, "Summary")
writeData(wb, "Summary", "NTD Subtype Comparison", startRow = 1)
writeData(wb, "Summary", paste("Date:", Sys.Date()), startRow = 2)

writeData(wb, "Summary", "Mortality (13 months-2 years)", startRow = 4)
mort_results <- data.frame(
  Statistic = c("Test", "Chi-Square (Q)", "I² (Heterogeneity)", "P-value", "Significant?"),
  Value = c(mort_test_name, mort_q, paste0(mort_i2, "%"), round(mort_p, 4), ifelse(mort_p < 0.05, "YES", "NO"))
)
writeData(wb, "Summary", mort_results, startRow = 5)

writeData(wb, "Summary", "Hydrocephalus (all ages)", startRow = 11)
hydro_results <- data.frame(
  Statistic = c("Test", "Chi-Square (Q)", "I² (Heterogeneity)", "P-value", "Significant?"),
  Value = c(hydro_test_name, hydro_q, paste0(hydro_i2, "%"), round(hydro_p, 4), ifelse(hydro_p < 0.05, "YES", "NO"))
)
writeData(wb, "Summary", hydro_results, startRow = 12)

writeData(wb, "Summary", "Complete Comparison", startRow = 18)
writeData(wb, "Summary", summary_table, startRow = 19)

addWorksheet(wb, "Summed_Data")
writeData(wb, "Summed_Data", "Summed Events and Totals", startRow = 1)
writeData(wb, "Summed_Data", summed_data, startRow = 3)

addWorksheet(wb, "Contingency_Tables")
writeData(wb, "Contingency_Tables", "Mortality Contingency Table", startRow = 1)
mort_df <- as.data.frame.matrix(mortality_table)
mort_df <- cbind(Category = rownames(mort_df), mort_df)
writeData(wb, "Contingency_Tables", mort_df, startRow = 3)

writeData(wb, "Contingency_Tables", "Hydrocephalus Contingency Table", startRow = 8)
hydro_df <- as.data.frame.matrix(hydrocephalus_table)
hydro_df <- cbind(Category = rownames(hydro_df), hydro_df)
writeData(wb, "Contingency_Tables", hydro_df, startRow = 10)

excel_file <- file.path(output_dir, "MMC_vs_Encephalocele_Data.xlsx")
saveWorkbook(wb, excel_file, overwrite = TRUE)
cat("Excel saved:", excel_file, "\n")

# Final summary
cat("\n========================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("========================================\n")
cat("\nMORTALITY (13m-2y):\n")
cat("  Test:", mort_test_name, "\n")
if(mort_test_name == "Chi-Square") {
  cat("  Q =", mort_q, "\n")
}
cat("  I² =", mort_i2, "%\n")
cat("  P-value =", round(mort_p, 4), "\n")
cat("  Significant:", ifelse(mort_p < 0.05, "YES", "NO"), "\n")

cat("\nHYDROCEPHALUS (all ages):\n")
cat("  Test:", hydro_test_name, "\n")
if(hydro_test_name == "Chi-Square") {
  cat("  Q =", hydro_q, "\n")
}
cat("  I² =", hydro_i2, "%\n")
cat("  P-value =", round(hydro_p, 4), "\n")
cat("  Significant:", ifelse(hydro_p < 0.05, "YES", "NO"), "\n")
cat("========================================\n")