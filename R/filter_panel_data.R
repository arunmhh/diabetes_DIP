# ══════════════════════════════════════════════════════════════════════════════
# Helper: filter panel and join with diabetic data
# ══════════════════════════════════════════════════════════════════════════════

prepare_panel_data <- function(
    final_data,
    diabetic_data,
    panel_id_value,                          # e.g. "got", "crp", "alt"
    final_data_id_col      = "bf_ar_m_nummer", # join key in final_data
    diabetic_id_col      = "bf_ar_m_nummer", # join key in diabetic_data
    exclude_pattern      = "_From Start|_From start|DBS", # regex to exclude
    group_col            = "diabetes"
) {
  
  library(dplyr)
  
  # ── Validation ---------------------------------------------------------------
  if (!panel_id_value %in% unique(final_data$panel_id)) {
    stop("panel_id '", panel_id_value, "' not found in final_data. ",
         "Available: ", paste(unique(final_data$panel_id), collapse = ", "))
  }
  
  # ── Step 1: Filter panel and convert labreads to numeric --------------------
  panel_filtered <- final_data %>%
    filter(panel_id == panel_id_value) %>%
    mutate(labreads = as.numeric(labreads))
  
  n_lost <- sum(is.na(panel_filtered$labreads))
  if (n_lost > 0) {
    message("Warning: ", n_lost, " value(s) in 'labreads' became NA after conversion.")
  }
  
  # ── Step 2: Join with diabetic data -----------------------------------------
  panel_diabetic <- left_join(
    panel_filtered,
    diabetic_data,
    by           = setNames(diabetic_id_col, final_data_id_col),
    relationship = "many-to-many"
  ) %>%
    drop_na() %>%
    filter(!grepl(exclude_pattern, attribute)) %>%
    group_by(.data[[group_col]])
  
  message("✔ '", panel_id_value, "' ready — ",
          nrow(panel_diabetic), " rows | ",
          n_distinct(panel_diabetic[[final_data_id_col]]), " patients")
  
  return(panel_diabetic)
}