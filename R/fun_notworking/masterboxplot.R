# ═══════════════════════════════════════════════════════════════════════
# MASTER FUNCTION
# ═══════════════════════════════════════════════════════════════════════

generate_panel_plots <- function(
    final_data,
    diabetic_data,
    panel_vector,
    comparisons,
    metadata_table       = NULL,
    
    group_var            = diabetes,
    y_var                = labreads,
    patient_id_var       = bf_ar_m_nummer,
    
    save_output          = FALSE,
    dataset_name         = "diabetes",
    save_subfolder       = NULL,
    
    width                = 8,
    height               = 6,
    dpi                  = 600
) {
  
  library(dplyr)
  library(purrr)
  
  plot_list <- list()
  
  for (panel_name in panel_vector) {
    
    message("Processing panel: ", panel_name)
    
    # ─────────────────────────────────────────────────────────────
    # Prepare data
    # ─────────────────────────────────────────────────────────────
    
    panel_data <- prepare_panel_data(
      final_data      = final_data,
      diabetic_data   = diabetic_data,
      panel_id_value  = panel_name
    )
    
    # ─────────────────────────────────────────────────────────────
    # Metadata lookup
    # ─────────────────────────────────────────────────────────────
    
    if (!is.null(metadata_table)) {
      
      meta_row <- metadata_table %>%
        filter(panel_id == panel_name)
      
      plot_title <- meta_row$plot_title
      y_label    <- meta_row$y_label
      
    } else {
      
      plot_title <- paste0(toupper(panel_name), " Comparison")
      y_label    <- toupper(panel_name)
    }
    
    # ─────────────────────────────────────────────────────────────
    # Generate plot
    # ─────────────────────────────────────────────────────────────
    
    p <- plot_lab_comparison(
      data           = panel_data,
      group_var      = {{ group_var }},
      y_var          = {{ y_var }},
      patient_id_var = {{ patient_id_var }},
      comparisons    = comparisons,
      
      plot_title     = plot_title,
      y_label        = y_label
    )
    
    # store plot
    plot_list[[panel_name]] <- p
  }
  
  # ─────────────────────────────────────────────────────────────
  # Save plots
  # ─────────────────────────────────────────────────────────────
  
  if (save_output) {
    
    save_plots(
      plots        = plot_list,
      names        = names(plot_list),
      dataset_name = dataset_name,
      subfolder    = save_subfolder,
      width        = width,
      height       = height,
      dpi          = dpi
    )
  }
  
  return(plot_list)
}