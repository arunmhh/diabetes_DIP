make_condition_plot <- function(
    
  # datasets
  all_data,
  diabetic_data,
  gender_data,
  
  # panel
  panel_id_value,
  
  # filters
  diabetes_status = NULL,
  gender_filter = NULL,
  
  # treatment
  treatment_prefix = "Leber_",
  
  # plotting
  comparisons,
  plot_title,
  y_label,
  
  # cleaning
  remove_patterns = "_From Start|_From start|DBS"
) {
  
  # ==========================================
  # PREPARE PANEL DATA
  # ==========================================
  
  panel_data <- prepare_panel_data(
    all_data = all_data,
    diabetic_data = diabetic_data,
    panel_id_value = panel_id_value
  )
  
  # ==========================================
  # JOIN GENDER
  # ==========================================
  
  plot_data <- left_join(
    gender_data,
    panel_data,
    by = "bf_ar_m_nummer"
  )
  
  # ==========================================
  # REMOVE UNWANTED ATTRIBUTES
  # ==========================================
  
  plot_data <- plot_data %>%
    filter(
      !grepl(remove_patterns, attribute)
    )
  
  # ==========================================
  # DIABETES FILTER
  # ==========================================
  
  if (!is.null(diabetes_status)) {
    
    plot_data <- plot_data %>%
      filter(diabetes == diabetes_status)
  }
  
  # ==========================================
  # GENDER FILTER
  # ==========================================
  
  if (!is.null(gender_filter)) {
    
    plot_data <- plot_data %>%
      filter(gender == gender_filter)
  }
  
  # ==========================================
  # TREATMENT FILTER
  # ==========================================
  
  plot_data <- plot_data %>%
    filter(
      str_starts(treatment.x, treatment_prefix)
    )
  
  # ==========================================
  # CLEAN
  # ==========================================
  
  plot_data <- plot_data %>%
    drop_na() %>%
    mutate(
      treatment.x = as.factor(treatment.x),
      gender = as.factor(gender)
    )
  
  # ==========================================
  # CALL YOUR CUSTOM PLOT FUNCTION
  # ==========================================
  
  p <- gender_plot_lab_comparison(
    data           = plot_data,
    group_var      = treatment.x,
    y_var          = labreads,
    patient_id_var = bf_ar_m_nummer,
    comparisons    = comparisons,
    plot_title     = plot_title,
    y_label        = y_label
  )
  
  return(p)
}