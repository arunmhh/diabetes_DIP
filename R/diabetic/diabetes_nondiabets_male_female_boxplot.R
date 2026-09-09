library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)
library(utils)
library(tidyr)

# ══════════════════════════════════════════════════════════════════════════════
# Helper 1: Build label data frames (Now grouped by Sex and Diabetes Status)
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# Helper 1: Build label data frames (Fixed string evaluation)
# ══════════════════════════════════════════════════════════════════════════════

make_label_dfs_gender <- function(data, group_col_str, facet_col_str, id_col_str) {
  # Standardize grouping using strings and !!sym() to prevent scoping errors
  label_df <- data %>%
    group_by(!!sym(facet_col_str), !!sym(group_col_str)) %>%
    summarise(
      n_patients = n_distinct(!!sym(id_col_str)),
      n_labreads = n(),
      .groups = "drop"
    )
  
  label_df_top <- label_df %>%
    mutate(label = paste0("n = ", n_labreads)) 
  
  label_df_bottom <- label_df %>%
    mutate(label = paste0("(N = ", n_patients, ")")) 
  
  list(top = label_df_top, bottom = label_df_bottom)
}

# ══════════════════════════════════════════════════════════════════════════════
# Helper 2: Build comparisons dynamically
# ══════════════════════════════════════════════════════════════════════════════

make_comparisons <- function(data, group_col_str) {
  lvls <- levels(factor(data[[group_col_str]]))
  if (length(lvls) < 2) {
    return(NULL)
  }
  utils::combn(lvls, 2, simplify = FALSE)
}

# ══════════════════════════════════════════════════════════════════════════════
# Main: Plot lab comparison faceted by Gender
# ══════════════════════════════════════════════════════════════════════════════

plot_lab_comparison_gender <- function(
    data,
    group_var,     # e.g., diabetes
    facet_var,     # e.g., your gender column name
    y_var,         # e.g., labreads
    patient_id_var,
    comparisons,
    plot_title   = "Comparison of Lab Values",
    y_label      = "Lab Value",
    jitter_width = 0.20,
    jitter_alpha = 0.5,
    jitter_size  = 4,
    text_size    = 6, 
    n_breaks     = 10
) {
  
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(scales)
  
  # ── Convert inputs directly to strings to stabilize sub-helpers ──
  group_col <- as.character(substitute(group_var))
  facet_col <- as.character(substitute(facet_var))
  y_col     <- as.character(substitute(y_var))
  id_col    <- as.character(substitute(patient_id_var))
  
  if (!is.numeric(data[[y_col]])) {
    data[[y_col]] <- as.numeric(data[[y_col]])
  }
  
  # Generate labels passing the string column targets safely
  labels          <- make_label_dfs_gender(data, group_col, facet_col, id_col)
  label_df_top    <- labels$top
  label_df_bottom <- labels$bottom
  
  # Map custom X-axis labels: "Diabetic \n (N = 45)"
  custom_x_labels <- setNames(
    paste0(label_df_bottom[[group_col]], "\n", label_df_bottom$label),
    label_df_bottom[[group_col]]
  )
  
  y_max    <- max(data[[y_col]], na.rm = TRUE)
  y_top    <- y_max * 1.50  
  
  # Base Plot Construction
  p <- ggplot(data, aes(x = .data[[group_col]], y = .data[[y_col]], fill = .data[[group_col]])) +
    geom_boxplot(width = 0.30, outlier.shape = NA, alpha = 0.5, color = "black") +
    geom_jitter(width = jitter_width, alpha = jitter_alpha, size = jitter_size, color = "#CC79A7") +
    stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "brown") +
    
    # Render sample size counts at the top of each box
    geom_text(
      data        = label_df_top,
      aes(x = !!sym(group_col), y = y_top, label = label), # Fixed evaluation
      inherit.aes = FALSE, fontface = "bold", size = text_size
    )
  
  # Draw separate comparison statistics per facet if possible
  if (!is.null(comparisons) && length(comparisons) > 0) {
    p <- p + stat_compare_means(
      comparisons   = comparisons, method = "wilcox.test", label = "p.format",
      label.y       = y_max * 1.05, step.increase = 0.08, bracket.size = 0.8,
      tip.length    = 0.02, size = text_size
    )
  }
  
  # Apply faceting, labeling rules, and presentation themes
  p <- p + 
    facet_wrap(vars(!!sym(facet_col)), scales = "fixed") + # Fixed evaluation
    labs(title = plot_title, x = NULL, y = y_label) +
    coord_cartesian(ylim = c(0, y_max * 1.60), clip = "off") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = n_breaks)) +
    scale_x_discrete(labels = custom_x_labels) + 
    theme_classic() +
    theme(
      legend.position  = "none",
      plot.title       = element_text(hjust = 0.5, size = 26, face = "bold", margin = margin(b = 15), lineheight = 1.2),
      strip.text       = element_text(size = 22, face = "bold"), 
      strip.background = element_rect(fill = "gray95", color = "black", linewidth = 0.5),
      axis.text.x      = element_text(size = 18, face = "bold", margin = margin(t = 10), hjust = 0.5, vjust = 1),
      axis.text.y      = element_text(size = 20, face = "bold"),
      axis.title       = element_text(size = 18, face = "bold"),
      axis.line        = element_line(linewidth = 0.5),
      panel.spacing    = unit(2, "lines"), 
      plot.margin      = margin(t = 15, r = 30, b = 40, l = 20)
    )
  
  return(p)
}