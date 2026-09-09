prepare_all_lab_gender_boxplots <- function(
    final_data,
    diabetic_data,
    gender_data = NULL,   # 👈 NEW (optional)
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
  # OPTIONAL: attach gender
  # ─────────────────────────────────────────────
  
  if (!is.null(gender_data)) {
    
    diabetic_data <- diabetic_data %>%
      left_join(
        gender_data %>%
          dplyr::select(bf_ar_m_nummer, gender) %>%
          dplyr::distinct(),
        by = "bf_ar_m_nummer"
      ) %>%
      tidyr::drop_na(gender)
  }
  
  # ─────────────────────────────────────────────
  # Automatic defaults
  # ─────────────────────────────────────────────
  
  if (is.null(panel_titles)) {
    panel_titles <- stats::setNames(
      stringr::str_to_title(panel_vector),
      panel_vector
    )
    
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
      paste0(stringr::str_to_upper(panel_vector), " (U/L)"),
      panel_vector
    )
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
  skipped_log  <- character(0)
  error_log    <- character(0)
  
  # ─────────────────────────────────────────────
  # Main loop
  # ─────────────────────────────────────────────
  
  for (panel_name in panel_vector) {
    
    message("\n── Processing panel: ", panel_name, " ──")
    
    panel_data <- tryCatch({
      prepare_panel_data(
        final_data     = final_data,
        diabetic_data  = diabetic_data,
        panel_id_value = panel_name
      )
    }, error = function(e) NULL)
    
    if (is.null(panel_data)) next
    
    # ── FIX: clean labreads ─────────────────────
    panel_data <- panel_data %>%
      mutate(
        labreads = labreads %>%
          stringr::str_replace_all(",", ".") %>%
          stringr::str_remove_all("[^0-9\\.\\-]") %>%
          as.numeric()
      )
    
    n_valid_panel <- sum(!is.na(panel_data$labreads))
    message("  ✔ ", panel_name, " — ", nrow(panel_data), " rows | ",
            n_valid_panel, " valid numeric reads")
    
    # ─────────────────────────────────────────────
    # treatment loop
    # ─────────────────────────────────────────────
    
    for (treatment_name in treatment_vector) {
      
      current_plot_name <- paste0(panel_name, "_", tolower(treatment_name))
      
      tryCatch({
        
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
        
        valid_reads <- sum(!is.na(final_plot_data$labreads))
        
        if (nrow(final_plot_data) == 0) next
        if (valid_reads == 0) next
        
        # ─────────────────────────────────────
        # UPDATED TITLE (adds gender automatically if present)
        # ─────────────────────────────────────
        
        gender_txt <- if ("gender" %in% names(final_plot_data)) {
          "\nStratified by Gender"
        } else {
          "\nDiabetic vs Non-Diabetic"
        }
        
        current_title <- paste0(
          panel_titles[[panel_name]],
          " in ",
          treatment_titles[[treatment_name]],
          gender_txt
        )
        
        # ─────────────────────────────────────
        # plot (UNCHANGED)
        # ─────────────────────────────────────
        
        p <- plot_lab_comparison(
          data           = final_plot_data,
          group_var      = diabetes,
          y_var          = labreads,
          patient_id_var = bf_ar_m_nummer,
          
          comparisons    = list(c("Diabetic", "Non_Diabetic")),
          
          plot_title     = current_title,
          y_label        = y_labels[[panel_name]]
        )
        
        all_plots[[current_plot_name]] <- p
        
        if (save_plot) {
          save_plots(
            plots        = list(p),
            names        = current_plot_name,
            dataset_name = dataset_name,
            subfolder    = file.path(subfolder, panel_name)
          )
        }
        
        message("  ✔ Plot ready: ", current_plot_name)
        
      }, error = function(e) {
        message("  ✖ Error in [", current_plot_name, "]: ", e$message)
        error_log <<- c(error_log, paste0(current_plot_name, " — ", e$message))
      })
      
    }
  }
  
  message("\n════════════════════════════")
  message("✔ Plots:", length(all_plots))
  message("════════════════════════════")
  
  return(all_plots)
}