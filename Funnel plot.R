# === Complete script: meta-analysis (all outcomes) + publication bias ===

# Clean
rm(list = ls()); gc(); cat("\014")

# ----- Packages -----
pkgs <- c("meta", "dplyr", "readxl", "forcats", "grid")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
lapply(pkgs, library, character.only = TRUE)

# ----- Paths -----
file_path <- "/Users/or24054/Downloads/SRM_Ntds_Hope.xlsx"
output_dir <- file.path(dirname(file_path), "Meta_Results")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

message("Data file: ", file_path)
message("Output dir: ", output_dir)

# ----- Read data -----
message("\nLoading data...")
data <- readxl::read_excel(file_path)
colnames(data) <- trimws(colnames(data))
message("Loaded: ", nrow(data), " rows x ", ncol(data), " cols")

# Check required columns
required_cols <- c("Study id", "Outcome", "Age group", "Events", "Total")
miss <- setdiff(required_cols, names(data))
if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

# Keep only rows with needed values
data <- data %>% filter(!is.na(Outcome), !is.na(Events), !is.na(Total))

# Optionally reorder Age group if those levels exist
if ("0-27 days" %in% data$`Age group`) {
  data <- data %>%
    mutate(`Age group` = forcats::fct_relevel(
      `Age group`,
      "0-27 days", "30 days-12 months", "13 months-2 years",
      "3-5 years", "6-11 years", "12-17 years"
    ))
}

# ----- Helper: safe p extractor for metabias objects -----
extract_pval <- function(obj) {
  if (is.null(obj)) return(NA_real_)
  if (!is.null(obj$p.value.bias)) return(as.numeric(obj$p.value.bias))
  if (!is.null(obj$p.value)) return(as.numeric(obj$p.value))
  # fallback try: some metabias objects print differently
  out <- tryCatch(as.numeric(obj), error = function(e) NA_real_)
  return(out)
}

# ----- Prepare summary storage -----
summary_rows <- list()

# ----- Loop through outcomes -----
outcomes <- unique(data$Outcome)
message("\nFound outcomes: ", paste(outcomes, collapse = " | "))

for (outcome in outcomes) {
  message("\n========================================")
  message("Processing outcome: ", outcome)
  message("========================================")
  
  subset_data <- data %>% filter(Outcome == outcome)
  raw_rows <- nrow(subset_data)
  message("Raw rows: ", raw_rows)
  if (raw_rows < 2) {
    message("Skipping outcome (fewer than 2 rows).")
    # still record summary row
    summary_rows[[outcome]] <- list(
      Outcome = outcome,
      Raw_rows = raw_rows,
      Unique_studies = NA_integer_,
      Bias_assessed = FALSE,
      Egger_p = NA_real_,
      Begg_p = NA_real_,
      Forest_file = NA_character_,
      Funnel_file = NA_character_
    )
    next
  }
  
  # ----- For mortality: mark repeated timepoints with '*' (visual only) -----
  if (tolower(outcome) == "mortality") {
    # auto-detect repeated Study id values (appear >1)
    dup_ids <- names(which(table(subset_data$`Study id`) > 1))
    if (length(dup_ids) > 0) {
      subset_data <- subset_data %>%
        mutate(`Study id` = ifelse(`Study id` %in% dup_ids,
                                   paste0(`Study id`, "*"),
                                   `Study id`))
      message("Marked ", length(dup_ids), " repeated Study id(s) with '*'.")
    } else {
      message("No repeated study IDs found for mortality.")
    }
  }
  
  # ----- Run metaprop -----  
  meta_obj <- tryCatch({
    metaprop(
      event = subset_data$Events,
      n = subset_data$Total,
      studlab = subset_data$`Study id`,
      data = subset_data,
      sm = "PLOGIT",
      method.tau = "ML",
      random = TRUE,
      fixed = FALSE,
      byvar = subset_data$`Age group`
    )
  }, error = function(e) {
    message("metaprop ERROR for ", outcome, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(meta_obj)) {
    summary_rows[[outcome]] <- list(
      Outcome = outcome,
      Raw_rows = raw_rows,
      Unique_studies = NA_integer_,
      Bias_assessed = FALSE,
      Egger_p = NA_real_,
      Begg_p = NA_real_,
      Forest_file = NA_character_,
      Funnel_file = NA_character_
    )
    next
  }
  
  # Suppress subgroup heterogeneity when k.w == 1
  if (!is.null(meta_obj$k.w)) {
    for (i in seq_along(meta_obj$k.w)) {
      if (meta_obj$k.w[i] == 1) {
        meta_obj$Q.w.random[i] <- NA
        meta_obj$pval.Q.w.random[i] <- NA
        meta_obj$I2.w[i] <- NA
        meta_obj$lower.I2.w[i] <- NA
        meta_obj$upper.I2.w[i] <- NA
        meta_obj$tau.w[i] <- NA
      }
    }
  }
  
  # ----- Forest plot (save PDF) -----
  safe_name <- gsub("[^A-Za-z0-9]", "_", outcome)
  forest_file <- file.path(output_dir, paste0("Meta_", safe_name, ".pdf"))
  pdf(forest_file, width = 10, height = 8)
  tryCatch({
    forest(
      meta_obj,
      layout = "meta",
      backtransf = TRUE,
      prediction = TRUE,
      print.tau2 = TRUE,
      print.I2 = TRUE,
      print.pval.Q = TRUE,
      leftcols = "studlab",
      leftlabs = "Study",
      rightcols = c("effect", "ci"),
      rightlabs = c("Proportion", "95% CI"),
      digits = 2,
      overall = ifelse(tolower(outcome) == "mortality", FALSE, TRUE),
      overall.hetstat = TRUE,
      print.byvar = TRUE,
      bylab = "",
      print.subgroup.name = FALSE,
      byseparator = "",
      het.subgroup = FALSE,
      col.diamond = "black",
      col.square = "black",
      col.predict = "black",
      smlab = "",
      fontsize = 9,
      cex = 0.85,
      colgap.forest.left = unit(0.4, "cm"),
      refline = ifelse(tolower(outcome) == "mortality", 0.06, NA),
      col.refline = "black",
      lty.refline = 2,
      lwd.refline = 1.2
    )
    grid::grid.text(paste0(outcome, " in Neural Tube Defects by Age Group"),
                    y = unit(0.97, "npc"),
                    gp = gpar(fontsize = 14, fontface = "bold"))
    if (tolower(outcome) == "mortality") {
      grid::grid.text("* Studies contribute data at multiple time points",
                      x = unit(0.02, "npc"), y = unit(0.02, "npc"),
                      just = "left", gp = gpar(fontsize = 8, fontface = "italic"))
    }
  }, error = function(e) {
    message("Forest plot error for ", outcome, ": ", e$message)
  }, finally = dev.off())
  message("Saved forest plot: ", forest_file)
  
  # ----- Publication bias decision (count unique studies after removing '*') -----
  # Robust Study id cleaning function
  clean_study_id <- function(x) {
    x <- as.character(x)
    x <- gsub("\\*", "", x)                     # remove asterisk markers
    x <- gsub("\\(.*?\\)", "", x)               # remove parenthetical notes
    x <- gsub("\\bT\\b|\\bT\\d+\\b", "", x, perl=TRUE)   # remove 'T' or 'T1' timepoint labels
    x <- gsub("\\b\\d+\\s?mo\\b|\\b\\d+\\s?months?\\b", "", x, perl=TRUE) # remove '12mo' etc
    x <- gsub("(?<=\\d)[a-z]+", "", x, perl=TRUE) # remove trailing letter tokens after numbers (if any)
    x <- gsub("[^A-Za-z0-9 ]+", " ", x)         # remove punctuation
    x <- tolower(x)                             # lowercase
    x <- gsub("\\s+", " ", x)                   # collapse whitespace
    x <- trimws(x)                              # trim
    return(x)
  }
  
  # Apply robust cleaning
  subset_data$Study_clean <- vapply(subset_data$`Study id`, clean_study_id, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  # Optional: show mapping for debugging (comment out after checking)
  mapping_debug <- subset_data %>%
    dplyr::select(`Study id`, Study_clean) %>%
    dplyr::distinct() %>%
    dplyr::arrange(Study_clean)
  print(mapping_debug)
  
  # Count unique studies
  unique_studies <- length(unique(subset_data$Study_clean))
  message("Unique independent studies (after robust cleaning): ", unique_studies)
  
  message("Unique independent studies (after removing '*'): ", unique_studies)
  
  funnel_file <- NA_character_
  egger_p <- NA_real_
  begg_p  <- NA_real_
  bias_assessed <- FALSE
  
  if (unique_studies >= 10) {
    bias_assessed <- TRUE
    message("Running publication bias checks (funnel, Egger, Begg)...")
    
    # Funnel
    funnel_file <- file.path(output_dir, paste0("Funnel_", safe_name, ".pdf"))
    pdf(funnel_file, width = 7, height = 7)
    tryCatch({
      funnel(meta_obj,
             xlab = "Effect size (Proportion)", ylab = "Standard Error",
             contour = c(0.9, 0.95, 0.99),
             col.contour = c("darkgray", "gray", "lightgray"))
      title(main = paste0("Funnel Plot for ", outcome))
    }, error = function(e) {
      message("Funnel plot error for ", outcome, ": ", e$message)
    }, finally = dev.off())
    message("Saved funnel plot: ", funnel_file)
    
    # Egger & Begg via metabias
    egger_obj <- tryCatch(metabias(meta_obj, method.bias = "linreg"), error = function(e) { message("Egger error: ", e$message); NULL })
    begg_obj  <- tryCatch(metabias(meta_obj, method.bias = "rank"), error = function(e) { message("Begg error: ", e$message); NULL })
    
    egger_p <- extract_pval(egger_obj)
    begg_p  <- extract_pval(begg_obj)
    message("Egger p = ", ifelse(is.na(egger_p), "NA", format(round(egger_p, 4), nsmall = 4)))
    message("Begg  p = ", ifelse(is.na(begg_p),  "NA", format(round(begg_p, 4), nsmall = 4)))
  } else {
    message("Publication bias NOT performed (need >=10 unique studies).")
  }
  
  # ----- Save summary info for this outcome -----
  summary_rows[[outcome]] <- list(
    Outcome = outcome,
    Raw_rows = raw_rows,
    Unique_studies = unique_studies,
    Bias_assessed = bias_assessed,
    Egger_p = egger_p,
    Begg_p = begg_p,
    Forest_file = forest_file,
    Funnel_file = ifelse(is.na(funnel_file), "", funnel_file)
  )
  
} # end outcomes loop

# ----- Combine summary and save CSV -----
summary_df <- do.call(rbind, lapply(summary_rows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
# ensure column types
summary_df$Raw_rows <- as.integer(summary_df$Raw_rows)
summary_df$Unique_studies <- as.integer(summary_df$Unique_studies)
summary_df$Egger_p <- as.numeric(summary_df$Egger_p)
summary_df$Begg_p <- as.numeric(summary_df$Begg_p)
summary_csv <- file.path(output_dir, "Meta_summary_publication_bias.csv")
write.csv(summary_df, summary_csv, row.names = FALSE)
message("\nSaved summary CSV: ", summary_csv)

message("\nAll done. Results in: ", output_dir)
