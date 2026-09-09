prepare_all_lab_boxplots <- function(
    final_data,
    diabetic_data,
    panel_vector,
    treatment_vector,
    
    y_labels         = NULL,
    panel_titles     = NULL,
    treatment_titles = NULL,
    
    dataset_name = "diabetic",
    subfolder    = "diabetic",
    
    save_plot    = TRUE
) {
  
  library(dplyr)
  library(purrr)
  library(stringr)
  
  # ─────────────────────────────────────────────
  # Automatic defaults
  # ─────────────────────────────────────────────
  
  if (is.null(panel_titles)) {
    
    panel_titles <- stats::setNames(
      object = stringr::str_to_title(panel_vector),
      nm     = panel_vector
    )
    
    # Optional custom replacements
    panel_titles[c("gpt", "got", "ggt", "ap", "ck")] <- c(
      "ALT",
      "AST",
      "GGT",
      "AP",
      "CK"
    )
  }
  
  if (is.null(y_labels)) {
    
    y_labels <- stats::setNames(
      object = paste0(
        stringr::str_to_upper(panel_vector),
        " (U/L)"
      ),
      nm = panel_vector
    )
    
    # Optional clinical replacements
    y_labels[c("hb")] <- "Hb (g/dL)"
  }
  
  if (is.null(treatment_titles)) {
    
    treatment_titles <- c(
      Liver_Diclo = "Liver Diclofenac",
      Liver_Ibu   = "Liver Ibuprofen",
      Liver_Para  = "Liver Paracetamol"
    )
  }
  
  # ─────────────────────────────────────────────
  # Store plots
  # ─────────────────────────────────────────────
  
  all_plots    <- list()
  skipped_log  <- character(0)   # track skipped combinations
  error_log    <- character(0)   # track errored combinations
  
  # ─────────────────────────────────────────────
  # Main loop
  # ─────────────────────────────────────────────
  
  for (panel_name in panel_vector) {
    
    message("\n── Processing panel: ", panel_name, " ──")
    
    # ── Prepare panel data ──────────────────────
    panel_data <- tryCatch({
      
      prepare_panel_data(
        final_data     = final_data,
        diabetic_data  = diabetic_data,
        panel_id_value = panel_name
      )
      
    }, error = function(e) {
      message("  ✖ prepare_panel_data failed for [", panel_name, "]: ", e$message)
      NULL
    })
    
    # skip entire panel if data prep failed
    if (is.null(panel_data)) {
      skipped_log <- c(skipped_log, paste0(panel_name, " — panel data prep failed"))
      next
    }
    
    # ── FIX 2: clean labreads at source ─────────
    panel_data <- panel_data %>%
      mutate(
        labreads = labreads %>%
          # European decimal comma → period
          stringr::str_replace_all(",", "\\.") %>%
          # Strip non-numeric characters like <, >, ~, spaces, units
          stringr::str_remove_all("[^0-9\\.\\-]") %>%
          as.numeric()
      )
    
    n_valid_panel <- sum(!is.na(panel_data$labreads))
    message("  ✔ ", panel_name, " — ", nrow(panel_data), " rows | ",
            n_valid_panel, " valid numeric reads")
    
    # ── Loop treatments ──────────────────────────
    for (treatment_name in treatment_vector) {
      
      current_plot_name <- paste0(panel_name, "_", tolower(treatment_name))
      
      # ── FIX 3: wrap entire block in tryCatch ──
      tryCatch({
        
        # treatment filter + recode
        final_plot_data <- panel_data %>%
          mutate(
            treatment = dplyr::recode(
              treatment,
              "Leber_Diclo" = "Liver_Diclo",
              "Leber_Ibu"   = "Liver_Ibu",
              "Leber_Para"  = "Liver_Para"
            )
          ) %>%
          filter(treatment == treatment_name)
        
        # ── FIX 1: skip if labreads all NA after filter ──
        valid_reads <- sum(!is.na(final_plot_data$labreads))
        
        if (nrow(final_plot_data) == 0) {
          message("  ⚠ Skipping [", current_plot_name, "] — no rows after treatment filter.")
          skipped_log <- c(skipped_log, paste0(current_plot_name, " — no rows after treatment filter"))
          next
        }
        
        if (valid_reads == 0) {
          message("  ⚠ Skipping [", current_plot_name, "] — labreads all NA for this treatment.")
          skipped_log <- c(skipped_log, paste0(current_plot_name, " — labreads all NA"))
          next
        }
        
        # dynamic title
        current_title <- paste0(
          panel_titles[[panel_name]],
          " in ",
          treatment_titles[[treatment_name]],
          "\nDiabetic vs Non-Diabetic"
        )
        
        # generate plot
        p <- plot_lab_comparison(
          data           = final_plot_data,
          group_var      = diabetes,
          y_var          = labreads,
          patient_id_var = bf_ar_m_nummer,
          
          comparisons    = list(
            c("Diabetic", "Non_Diabetic")
          ),
          
          plot_title = current_title,
          y_label    = y_labels[[panel_name]]
        )
        
        # store
        all_plots[[current_plot_name]] <- p
        message("  ✔ Plot ready: ", current_plot_name)
        
        # save
        if (save_plot) {
          save_plots(
            plots        = list(p),
            names        = current_plot_name,
            dataset_name = dataset_name,
            subfolder    = file.path(subfolder, panel_name)
          )
        }
        
      }, error = function(e) {
        message("  ✖ Error in [", current_plot_name, "]: ", e$message)
        error_log <<- c(error_log, paste0(current_plot_name, " — ", e$message))
      })
      
    } # end treatment loop
    
  } # end panel loop
  
  # ─────────────────────────────────────────────
  # Summary report
  # ─────────────────────────────────────────────
  
  message("\n══════════════════════════════════════")
  message("SUMMARY")
  message("══════════════════════════════════════")
  message("✔ Plots generated : ", length(all_plots))
  message("⚠ Skipped         : ", length(skipped_log))
  message("✖ Errors          : ", length(error_log))
  
  if (length(skipped_log) > 0) {
    message("\nSkipped combinations:")
    message(paste0("  • ", skipped_log, collapse = "\n"))
  }
  
  if (length(error_log) > 0) {
    message("\nErrored combinations:")
    message(paste0("  • ", error_log, collapse = "\n"))
  }
  
  message("══════════════════════════════════════\n")
  
  return(all_plots)
}