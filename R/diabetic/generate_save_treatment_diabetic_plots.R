generate_and_save_panel_plots <- function(
    panel_ids,
    all_data,
    diabetic_data,
    gender_data,
    save_plots_flag     = TRUE,
    export_dataset_name = "lab_plots",
    export_base_dir     = "output",
    export_subfolder    = "panel_comparisons",
    export_width        = 8.5,
    export_height       = 6.5,
    export_dpi          = 600,
    export_formats      = c("svg", "png")
) {
  
  unit_map <- get_lab_unit_map()
  
  plots_list <- list()
  plot_names <- character()
  
  for (pid in panel_ids) {
    
    # ───────────────────────────────────────────────────────────
    # Data Preparation
    # ───────────────────────────────────────────────────────────
    
    panel_data <- all_data %>%
      left_join(
        diabetic_data,
        by = "bf_ar_m_nummer",
        relationship = "many-to-many"
      ) %>%
      left_join(
        gender_data,
        by = "bf_ar_m_nummer",
        relationship = "many-to-many"
      ) %>%
      filter(panel_id == pid) %>%
      filter(grepl("Leber_", treatment.x)) %>%
      filter(
        !str_detect(
          attribute,
          "_krankheitsbeginn|dbs"
        )
      ) %>%
      filter(
        !is.na(diabetes),
        !is.na(treatment.x)
      ) %>%
      mutate(
        treatment.x = recode(
          treatment.x,
          "Leber_Diclo" = "Liver_Diclo",
          "Leber_Ibu"   = "Liver_Ibu",
          "Leber_Para"  = "Liver_Para"
        )
      ) %>%
      mutate(
        treatment.x = factor(
          treatment.x,
          levels = c(
            "Liver_Diclo",
            "Liver_Ibu",
            "Liver_Para"
          )
        ),
        diabetes = factor(diabetes),
        gender   = factor(gender)
      ) %>%
      filter(labreads < 15000) %>%
      drop_na(
        labreads,
        treatment.x,
        diabetes
      )
    
    # Skip empty panels
    if (nrow(panel_data) == 0) {
      message(
        paste(
          "No valid data available for panel:",
          toupper(pid),
          "- skipping."
        )
      )
      next
    }
    
    # Plot labels
    plot_title <- paste(
      "Comparison of",
      toupper(pid),
      "in Liver"
    )
    
    y_label <- if (pid %in% names(unit_map)) {
      unit_map[[pid]]
    } else {
      toupper(pid)
    }
    
    # Generate plot
    p <- plot_lab_comparison(
      data           = panel_data,
      group_var      = treatment.x,
      y_var          = labreads,
      patient_id_var = bf_ar_m_nummer,
      comparisons    = make_comparisons(
        panel_data,
        "treatment.x"
      ),
      plot_title   = plot_title,
      x_label      = "Treatment",
      y_label      = y_label,
      y_pos_top    = y_pos_top,
      y_pos_bottom = y_pos_bottom
    )
    
    plots_list[[length(plots_list) + 1]] <- p
    
    plot_names <- c(
      plot_names,
      paste0("Plot_", toupper(pid))
    )
    
    message(
      paste(
        "Successfully generated plot for:",
        toupper(pid)
      )
    )
  }
  
  # Save plots
  if (save_plots_flag && length(plots_list) > 0) {
    
    message("\nSaving generated plots...")
    
    save_plots(
      plots        = plots_list,
      names        = plot_names,
      dataset_name = export_dataset_name,
      base_dir     = export_base_dir,
      subfolder    = export_subfolder,
      width        = export_width,
      height       = export_height,
      dpi          = export_dpi,
      formats      = export_formats
    )
  }
  
  names(plots_list) <- plot_names
  
  return(plots_list)
}