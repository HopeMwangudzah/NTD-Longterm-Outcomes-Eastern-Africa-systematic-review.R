# ===============================================================
# NTD FOREST PLOTS — VECTOR MANUSCRIPT ENGINE (PDF OUTPUT)


rm(list = ls())
cat("\014")
gc()

# ===============================================================
# Libraries
# ===============================================================
library(meta)
library(dplyr)
library(readxl)
library(forcats)
library(grid)

# ===============================================================
# Paths
# ===============================================================
data_path <- "/Users/or24054/Downloads/SRM_Ntds_Hope.xlsx"

output_dir <- file.path(
  dirname(data_path),
  "Meta_Results_Images"
)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ===============================================================
# Read Excel sheet
# ===============================================================
data <- read_excel(
  data_path,
  sheet = "Age group"
)

colnames(data) <- trimws(colnames(data))

# ===============================================================
# Clean data
# ===============================================================
data <- data %>%
  filter(
    !is.na(Outcome),
    !is.na(Events),
    !is.na(Total)
  ) %>%
  mutate(
    `Study id` = gsub(
      "\\.1|\\.2",
      "",
      `Study id`
    ),
    Age_group = fct_relevel(
      `Age group`,
      "0-27 days",
      "30 days-12 months",
      "13 months-2 years",
      "3-5 years",
      "6-11 years",
      "12-17 years"
    )
  )

# ===============================================================
# Outcomes
# ===============================================================
outcomes <- unique(data$Outcome)

# ===============================================================
# Main loop
# ===============================================================
for (outcome in outcomes) {
  
  message("\n==============================")
  message("Processing: ", outcome)
  message("==============================")
  
  subset_data <- data %>%
    filter(Outcome == outcome)
  
  if (nrow(subset_data) < 2) next
  
  # =============================================================
  # Structural settings per outcome type
  # =============================================================
  is_mortality <- tolower(outcome) == "mortality"
  
  show_overall <- !is_mortality
  refline_value <- if (is_mortality) 0.06 else NA
  
  # =============================================================
  # Meta-analysis
  # =============================================================
  meta_obj <- metaprop(
    event = subset_data$Events,
    n = subset_data$Total,
    studlab = subset_data$`Study id`,
    subgroup = subset_data$Age_group,
    sm = "PLOGIT",
    random = TRUE,
    common = FALSE,
    method.tau = "ML"
  )
  
  # =============================================================
  # Remove subgroup heterogeneity for single-study groups
  # =============================================================
  subgroup_counts <- table(subset_data$Age_group)
  meta_plot <- meta_obj
  
  for (sg in names(subgroup_counts)) {
    if (subgroup_counts[sg] < 2) {
      meta_plot$I2.w[sg] <- NA
      meta_plot$tau2.w[sg] <- NA
      meta_plot$pval.Q.w[sg] <- NA
    }
  }
  
  # =============================================================
  # LINE EXPANSION ENGINE
  # =============================================================
  n_studies <- nrow(subset_data)
  n_subgroups <- length(unique(subset_data$Age_group))
  n_poly_subgroups <- sum(subgroup_counts >= 2)
  
  total_plot_rows <- n_studies + (n_subgroups * 2) + n_poly_subgroups + (if(show_overall) 2 else 0)
  
  # -------------------------------------------------------------
  # Strict Manuscript Dimensions & Typography Anchors
  # -------------------------------------------------------------
  canvas_width     <- 7.5   
  current_fontsize <- 8.5   
  current_spacing  <- 1.15  
  title_size       <- 11.0  
  
  # Calculate custom proportional plot heights without crossing your 8.5" limit
  calculated_h <- 2.8 + (total_plot_rows * 0.24)
  final_height <- max(min(calculated_h, 8.5), 4.5) 
  
  # Continuous Linear Adjustment Rules 
  top_margin  <- 1.1 + (total_plot_rows * 0.03)
  y_title_pos <- 0.92 + (total_plot_rows * 0.0018)
  y_title_pos <- min(y_title_pos, 0.965) 
  
  bottom_margin  <- 5.5
  y_footnote_pos <- 0.05
  
  # =============================================================
  # Output file assignment (.pdf extension)
  # =============================================================
  output_file <- file.path(
    output_dir,
    paste0(
      "Meta_",
      gsub("[^A-Za-z0-9]", "_", outcome),
      ".pdf"
    )
  )
  
  # =============================================================
  # PDF device opening (Native vector scaling format)
  # =============================================================
  pdf(
    file = output_file,
    width = canvas_width, 
    height = final_height,
    useDingbats = FALSE # Ensures maximum cross-platform font compatibility
  )
  
  # =============================================================
  # Plot Execution
  # =============================================================
  tryCatch({
    
    forest(
      meta_plot,
      
      # ---------------------------------------------------------
      # Core structure settings
      # ---------------------------------------------------------
      backtransf = TRUE,
      overall = show_overall,
      subgroup = TRUE,
      
      # ---------------------------------------------------------
      # KEEP subgroup heterogeneity details intact
      # ---------------------------------------------------------
      print.subgroup.het = TRUE,
      print.I2 = TRUE,
      print.tau2 = TRUE,
      print.pval.Q = TRUE,
      
      # ---------------------------------------------------------
      # Native forest text silencers kept active
      # ---------------------------------------------------------
      overall.hetstat = FALSE,
      test.subgroup = FALSE,
      print.test.subgroup.random = FALSE,
      
      # ---------------------------------------------------------
      # Axis clear labels
      # ---------------------------------------------------------
      xlab = "",
      bylab = "",
      print.subgroup.name = FALSE,
      
      # ---------------------------------------------------------
      # Column Definitions
      # ---------------------------------------------------------
      leftcols = "studlab",
      leftlabs = "Study",
      rightcols = c("effect", "ci"),
      rightlabs = c("Proportion", "95% CI"),
      
      # ---------------------------------------------------------
      # Visual Layout Tunings
      # ---------------------------------------------------------
      refline = refline_value,
      col.square = "black",
      col.diamond = "black",
      col.predict = "black",
      
      fontsize = current_fontsize,
      spacing = current_spacing,
      bottom = bottom_margin,
      top = top_margin,             
      
      plotwidth = "3.2cm", 
      addline = TRUE       
    )
    
    # ==========================================================
    # Manuscript Title Header
    # ==========================================================
    grid.text(
      paste0(
        outcome,
        " in Neural Tube Defects by Age Group"
      ),
      y = unit(y_title_pos, "npc"), 
      gp = gpar(
        fontsize = title_size, 
        fontface = "bold"
      )
    )
    
    # ==========================================================
    # Build Unified Custom Footnote Block
    # ==========================================================
    footnote_lines <- c()
    
    if (show_overall) {
      raw_p <- meta_obj$pval.Q[1]
      formatted_p <- format.pval(raw_p, digits = 3)
      
      if (!substring(formatted_p, 1, 1) %in% c("<", ">")) {
        formatted_p <- paste0("= ", formatted_p)
      }
      
      het_line <- paste0(
        "Overall heterogeneity: I² = ", round(meta_obj$I2[1], 1), 
        "%, p ", formatted_p, 
        ", τ² = ", round(meta_obj$tau2[1], 4)
      )
      footnote_lines <- c(footnote_lines, het_line)
    }
    
    if (!is.null(meta_obj$pval.Q.b.random)) {
      raw_p_sub <- meta_obj$pval.Q.b.random[1]
      formatted_p_sub <- format.pval(raw_p_sub, digits = 3)
      
      if (!substring(formatted_p_sub, 1, 1) %in% c("<", ">")) {
        formatted_p_sub <- paste0("= ", formatted_p_sub)
      }
      
      subgroup_line <- paste0(
        "Test for subgroup differences: Q = ", round(meta_obj$Q.b.random[1], 2),
        ", df = ", meta_obj$df.Q.b[1],
        ", p ", formatted_p_sub
      )
      footnote_lines <- c(footnote_lines, subgroup_line)
    }
    
    # ==========================================================
    # Render Consolidated Footnote
    # ==========================================================
    if (length(footnote_lines) > 0) {
      
      final_footnote_text <- paste(footnote_lines, collapse = "\n")
      
      grid.text(
        final_footnote_text,
        x = unit(0.04, "npc"), 
        y = unit(y_footnote_pos, "npc"), 
        just = c("left", "bottom"),
        gp = gpar(
          fontsize = max(current_fontsize - 1, 7.5), 
          fontface = "italic",
          lineheight = 1.30 
        )
      )
    }
    
  }, error = function(e) {
    message("❌ Plot failed: ", e$message)
  }, finally = {
    dev.off()
  })
  
  message("✅ Saved Vector PDF: ", output_file)
}
