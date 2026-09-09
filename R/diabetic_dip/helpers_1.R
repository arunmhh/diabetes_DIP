# ==============================================================================
# Generate all pairwise group comparisons
#
# Args:
#   data      : dataframe
#   group_col : grouping variable name (character)
#
# Returns:
#   List of pairwise comparisons for stat_compare_means()
#   Example:
#   list(
#     c("Liver_Diclo", "Liver_Ibu"),
#     c("Liver_Diclo", "Liver_Para"),
#     c("Liver_Ibu",   "Liver_Para")
#   )
# ==============================================================================

library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)
library(stringr)
library(tidyr)

# ══════════════════════════════════════════════════════════════════════════════
# Helper 1: Build label data frames (Corrected N vs n convention)
# ══════════════════════════════════════════════════════════════════════════════

make_label_dfs <- function(data, group_var, patient_id_var) {
  label_df <- data %>%
    group_by({{ group_var }}) %>%
    summarise(
      n_patients = n_distinct({{ patient_id_var }}),
      n_labreads = n(),
      .groups = "drop"
    )
  
  # Top label: Lowercase 'n' for total lab reads (observations)
  label_df_top <- label_df %>%
    mutate(label = paste0("n = ", n_labreads)) 
  
  # Bottom label: Capital 'N' for unique patients (used in x-axis)
  label_df_bottom <- label_df %>%
    mutate(label = paste0("(N = ", n_patients, ")")) 
  
  list(top = label_df_top, bottom = label_df_bottom)
}



# ══════════════════════════════════════════════════════════════════════════════
# Helper 2: Build comparisons dynamically from factor levels
# ══════════════════════════════════════════════════════════════════════════════

make_comparisons <- function(data, group_col) {
  lvls <- levels(factor(data[[group_col]]))
  
  if (length(lvls) < 2) {
    return(NULL)
  }
  
  utils::combn(lvls, 2, simplify = FALSE)
}




