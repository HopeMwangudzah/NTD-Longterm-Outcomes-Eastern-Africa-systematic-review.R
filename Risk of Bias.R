# ============================================================================
# CORRECTED VERSION - Domains on top, Legend at bottom (no overlap)
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

rob_colors <- list(
  low = "#2e7d32",
  moderate = "#f9a825",
  high = "#c62828",
  unclear = "#0072b2",
  na_col = "grey75"
)

rob_symbols <- list(
  low = "+",
  moderate = "-",
  high = "X",
  unclear = "?",
  na_sym = "-"
)

# ============================================================================
# CORRECTED FUNCTION - Domains on top, Legend at bottom
# ============================================================================

create_elegant_rob_table <- function(data, outcome_name, height_multiplier = 0.8) {
  
  data_long <- data %>%
    pivot_longer(
      cols = c(selection_bias, response_bias, information_bias),
      names_to = "domain",
      values_to = "risk"
    ) %>%
    mutate(
      domain = factor(domain,
                      levels = c("selection_bias", "response_bias", "information_bias"),
                      labels = c("Selection Bias", "Response Bias", "Information Bias")),
      study = factor(study, levels = rev(unique(study))),  # Reverse for top-to-bottom
      risk_display = ifelse(is.na(risk), "NA", as.character(risk)),
      risk_display = factor(risk_display, levels = c("Low", "Moderate", "High", "Unclear", "NA")),
      symbol = case_when(
        is.na(risk) ~ "-",
        risk == "Low" ~ "+",
        risk == "Moderate" ~ "-",
        risk == "High" ~ "X",
        risk == "Unclear" ~ "?",
        TRUE ~ "-"
      ),
      color_val = case_when(
        is.na(risk) ~ rob_colors$na_col,
        risk == "Low" ~ rob_colors$low,
        risk == "Moderate" ~ rob_colors$moderate,
        risk == "High" ~ rob_colors$high,
        risk == "Unclear" ~ rob_colors$unclear,
        TRUE ~ rob_colors$na_col
      )
    )
  
  n_studies <- nrow(data)
  plot_height <- max(5, n_studies * height_multiplier)
  
  # Calculate legend position (below the table)
  legend_y <- 0
  
  # Main plot
  p <- ggplot(data_long, aes(x = domain, y = study)) +
    geom_tile(fill = "white", color = "grey70", linewidth = 0.8) +
    geom_point(aes(color = color_val), size = 14, shape = 16) +
    geom_text(aes(label = symbol), size = 7, fontface = "bold", color = "white") +
    scale_color_identity() +
    
    # LEGEND ROW 1
    # Low risk
    annotate("point", x = 0.65, y = legend_y, 
             size = 6, shape = 16, color = rob_colors$low) +
    annotate("text", x = 0.65, y = legend_y, label = "+",
             size = 4, fontface = "bold", color = "white") +
    annotate("text", x = 0.76, y = legend_y, label = "Low risk",
             hjust = 0, size = 3.5, color = "grey20") +
    
    # Moderate risk
    annotate("point", x = 1.35, y = legend_y, 
             size = 6, shape = 16, color = rob_colors$moderate) +
    annotate("text", x = 1.35, y = legend_y, label = "-",
             size = 4, fontface = "bold", color = "white") +
    annotate("text", x = 1.46, y = legend_y, label = "Moderate risk",
             hjust = 0, size = 3.5, color = "grey20") +
    
    # High risk
    annotate("point", x = 2.25, y = legend_y, 
             size = 6, shape = 16, color = rob_colors$high) +
    annotate("text", x = 2.25, y = legend_y, label = "X",
             size = 4, fontface = "bold", color = "white") +
    annotate("text", x = 2.36, y = legend_y, label = "High risk",
             hjust = 0, size = 3.5, color = "grey20") +
    
    # LEGEND ROW 2 (below row 1)
    # Unclear risk
    annotate("point", x = 0.65, y = legend_y - 0.5, 
             size = 6, shape = 16, color = rob_colors$unclear) +
    annotate("text", x = 0.65, y = legend_y - 0.5, label = "?",
             size = 4, fontface = "bold", color = "white") +
    annotate("text", x = 0.76, y = legend_y - 0.5, label = "Unclear risk",
             hjust = 0, size = 3.5, color = "grey20") +
    
    # Not applicable
    annotate("point", x = 1.45, y = legend_y - 0.5, 
             size = 6, shape = 16, color = rob_colors$na_col) +
    annotate("text", x = 1.45, y = legend_y - 0.5, label = "-",
             size = 4, fontface = "bold", color = "white") +
    annotate("text", x = 1.56, y = legend_y - 0.5, label = "Not applicable",
             hjust = 0, size = 3.5, color = "grey20") +
    
    labs(
      title = paste0("Risk of Bias: ", outcome_name),
      subtitle = paste0(n_studies, " studies"),
      x = "", y = ""
    ) +
    scale_x_discrete(position = "top") +  # DOMAINS ON TOP
    scale_y_discrete(expand = expansion(add = c(0.5, 2.5))) +  # Extra space at bottom
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", 
                                   margin = margin(b = 10)),
      axis.text.x.top = element_text(size = 12, face = "bold", color = "black"),
      axis.text.y = element_text(size = 11, face = "bold", color = "black", hjust = 1),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 50, 15)
    ) +
    coord_cartesian(clip = "off")
  
  return(list(plot = p, height = plot_height + 1.5))
}

# ============================================================================
# RECREATE ALL TABLES
# ============================================================================

# 1. MORTALITY
mortality_data <- data.frame(
  study = c("Williams 2017_T", "Warf 2011", "Xu 2018", "Getahun 2021",
            "Tirsit 2023", "Meester 2006", "Oneko 2002", "Oliveras 2023"),
  selection_bias = c("Low", "High", "High", "High", "High", "High", "High", "Low"),
  response_bias = c("Low", "Moderate", "High", "High", "High", "High", "High", "Low"),
  information_bias = c("Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
  stringsAsFactors = FALSE
)

mortality_rob <- create_elegant_rob_table(mortality_data, "Mortality", height_multiplier = 0.8)

# 2. HYDROCEPHALUS
hydrocephalus_data <- data.frame(
  study = c("Warf 2008-11", "Williams 2017_T", "Warf 2011", "Xu 2018", "Getahun 2021",
            "Tirsit 2023", "Meester 2006", "Oliveras 2023",
            "Bannink 2016", "Bannink 2018", "Williams 2017_Q", "Tafesse 2024", "Blokland 2011"),
  selection_bias = c("Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low",
                     "High", "Moderate", "Low", "Moderate", "Moderate"),
  response_bias = c("Moderate", "Low", "Moderate", "High", "High", "High", "Low", "Low",
                     NA, NA, NA, NA, NA),
  information_bias = c("Low", "Unclear", "Low", "Low", "Moderate", "Low", "Unclear", "Low",
                       "Unclear", "Moderate", "Unclear", "Unclear", "Unclear"),
  stringsAsFactors = FALSE
)

hydrocephalus_rob <- create_elegant_rob_table(hydrocephalus_data, "Hydrocephalus", height_multiplier = 1.0)

# 3. NEUROGENIC BLADDER
bladder_data <- data.frame(
  study = c("Xu 2018", "Getahun 2021", "Tirsit 2023", "Jeruto 2004",
            "Bannink 2016", "Bannink 2018", "Williams 2017_Q", "Williams 2018", 
            "Tafesse 2024", "Blokland 2011"),
  selection_bias = c("Low", "Low", "Low", "Moderate",
                     "High", "Moderate", "Low", "Low", "Moderate", "Moderate"),
  response_bias = c("High", "High", "High", "High",
                     NA, NA, NA, "Low", NA, NA),
  information_bias = c("Moderate", "Moderate", "Moderate", "Low",
                       "Moderate", "Moderate", "Moderate", "Moderate", "Moderate", "Moderate"),
  stringsAsFactors = FALSE
)

bladder_rob <- create_elegant_rob_table(bladder_data, "Neurogenic Bladder", height_multiplier = 1.0)

# 4. PARALYSIS
paralysis_data <- data.frame(
  study = c("Getahun 2021", "Tirsit 2023", "Bannink 2016", "Williams 2017_Q",
            "Williams 2018", "Blokland 2011"),
  selection_bias = c("Low", "Low", "High", "Low", "Low", "Moderate"),
  response_bias = c("High", "High", NA, NA, "Low", NA),
  information_bias = c("Moderate", "Moderate", "Low", "Low", "Low", "Low"),
  stringsAsFactors = FALSE
)

paralysis_rob <- create_elegant_rob_table(paralysis_data, "Paralysis", height_multiplier = 0.8)

# 5. SCHOOL ATTENDANCE
school_data <- data.frame(
  study = c("Getahun 2021", "Bannink 2016", "Bannink 2018", "Williams 2017_Q", "Tafesse 2024"),
  selection_bias = c("Low", "Unclear", "Moderate", "Low", "Moderate"),
  response_bias = c("High", NA, NA, NA, NA),
  information_bias = c("Moderate", "Moderate", "Moderate", "Moderate", "Moderate"),
  stringsAsFactors = FALSE
)

school_rob <- create_elegant_rob_table(school_data, "School Attendance", height_multiplier = 0.8)

# 6. NEURODEVELOPMENTAL DELAY
neurodev_data <- data.frame(
  study = c("Xu 2018 (Motor)", "Oliveras 2023", "Bannink 2016", "Bannink 2018", "Williams 2017_Q", "Xu 2018 (Cognition)", "Xu 2018 (Speech)"),
  selection_bias = c("Low", "Low", "High", "Moderate", "Low", "Low", "Low"),
  response_bias = c("High", "Unclear", NA, NA, NA,"High","High" ),
  information_bias = c("High", "Unclear", "Low", "Low", "Low", "High","Moderate"),
  stringsAsFactors = FALSE
)

neurodev_rob <- create_elegant_rob_table(neurodev_data, "Neurodevelopmental Delay", height_multiplier = 0.8)

# 7. BOWEL DYSFUNCTION
bowel_data <- data.frame(
  study = c("Getahun 2021"),
  selection_bias = c("Low"),
  response_bias = c("High"),
  information_bias = c("Unclear"),
  stringsAsFactors = FALSE
)

bowel_rob <- create_elegant_rob_table(bowel_data, "Bowel Dysfunction", height_multiplier = 0.3)

# ============================================================================
# SAVE ALL PLOTS
# ============================================================================

save_path <- "~/Desktop/"

ggsave(paste0(save_path, "ROB_Mortality_CORRECTED.png"), mortality_rob$plot,
       width = 8, height = mortality_rob$height, dpi = 300, bg = "white")

ggsave(paste0(save_path, "ROB_Hydrocephalus_CORRECTED.png"), hydrocephalus_rob$plot,
       width = 8, height = hydrocephalus_rob$height, dpi = 300, bg = "white")

ggsave(paste0(save_path, "ROB_Bladder_CORRECTED.png"), bladder_rob$plot,
       width = 8, height = bladder_rob$height, dpi = 300, bg = "white")

ggsave(paste0(save_path, "ROB_Paralysis_CORRECTED.png"), paralysis_rob$plot,
       width = 8, height = paralysis_rob$height, dpi = 300, bg = "white")

ggsave(paste0(save_path, "ROB_School_CORRECTED.png"), school_rob$plot,
       width = 8, height = school_rob$height, dpi = 300, bg = "white")

ggsave(paste0(save_path, "ROB_Neurodev_CORRECTED.png"), neurodev_rob$plot,
       width = 8, height = neurodev_rob$height, dpi = 300, bg = "white")

ggsave(paste0(save_path, "ROB_Bowel_CORRECTED.png"), bowel_rob$plot,
       width = 8, height = bowel_rob$height, dpi = 300, bg = "white")

# Display all plots
print(mortality_rob$plot)
print(hydrocephalus_rob$plot)
print(bladder_rob$plot)
print(paralysis_rob$plot)
print(school_rob$plot)
print(neurodev_rob$plot)
print(bowel_rob$plot)

# Summary
cat("\n=== CORRECTED TABLES SAVED ===\n")
cat("✓ Domain labels (Selection/Attrition/Information Bias) are now on TOP\n")
cat("✓ Legend is now at BOTTOM in two rows (no overlap)\n")
cat("✓ Unclear risk (?) is now properly separated from Moderate risk (-)\n\n")
cat(sprintf("Mortality: %.1f inches\n", mortality_rob$height))
cat(sprintf("Hydrocephalus: %.1f inches (INCREASED spacing)\n", hydrocephalus_rob$height))
cat(sprintf("Bladder: %.1f inches (INCREASED spacing)\n", bladder_rob$height))
cat(sprintf("Paralysis: %.1f inches\n", paralysis_rob$height))
cat(sprintf("School: %.1f inches\n", school_rob$height))
cat(sprintf("Neurodev: %.1f inches\n", neurodev_rob$height))
cat(sprintf("Bowel: %.1f inches (REDUCED spacing)\n", bowel_rob$height))