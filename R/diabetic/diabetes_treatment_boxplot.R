# ══════════════════════════════════════════════════════════════════════════════
# Main: Plot lab comparison between groups (With Custom GeomText Labels)
# ══════════════════════════════════════════════════════════════════════════════

plot_lab_comparison <- function(
    data,
    group_var,
    y_var,
    patient_id_var,
    comparisons,
    plot_title   = "Comparison of Lab Values",
    x_label      = NULL,
    y_label      = "Lab Value",
    jitter_width = 0.18,          
    jitter_alpha = 0.4,           
    jitter_size  = 4,             
    text_size    = 8,
    n_breaks     = 10,
    # ── NEW: Custom Y-positions for labels ──
    y_pos_top    = NULL,          
    y_pos_bottom = NULL           
) {
  
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(scales)
  
  # ── Resolve column names ────────────────────────────────────────────────────
  group_col <- deparse(substitute(group_var))
  y_col     <- deparse(substitute(y_var))
  id_col    <- deparse(substitute(patient_id_var))
  
  # ── Validation ──────────────────────────────────────────────────────────────
  missing_cols <- setdiff(c(group_col, y_col, id_col), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.numeric(data[[y_col]])) {
    message("Note: Converting '", y_col, "' from ", class(data[[y_col]]), " to numeric.")
    data[[y_col]] <- as.numeric(data[[y_col]])
    
    n_lost <- sum(is.na(data[[y_col]]))
    if (n_lost > 0) {
      message("Warning: ", n_lost, " value(s) became NA after conversion — check raw data.")
    }
  }
  
  if (all(is.na(data[[y_col]]))) {
    stop("y_var '", y_col, "' is all NA after conversion — check for non-numeric values.")
  }
  
  # ── Label data frames ───────────────────────────────────────────────────────
  labels          <- make_label_dfs(data, {{ group_var }}, {{ patient_id_var }})
  label_df_top    <- labels$top
  label_df_bottom <- labels$bottom
  
  # ── Y-axis positions (Customizable) ─────────────────────────────────────────
  # Y-axis positions
  y_max <- max(data[[y_col]], na.rm = TRUE)
  
  actual_y_top    <- y_max * 1.05
  actual_y_bottom <- -(y_max * 0.22)
  
  # ── Base Plot ───────────────────────────────────────────────────────────────
  p <- ggplot(data,
              aes(x    = .data[[group_col]],
                  y    = .data[[y_col]],
                  fill = .data[[group_col]])) +
    
    geom_boxplot(width         = 0.55,
                 outlier.shape = NA,
                 alpha         = 0.6,
                 color         = "black") +
    
    geom_jitter(width = jitter_width,
                alpha = jitter_alpha,
                size  = jitter_size,
                color = "#CC79A7") +
    
    stat_summary(fun   = median,
                 geom  = "point",
                 shape = 23,
                 size  = 4.5,     
                 fill  = "brown") +
    
    # ── FIX: Top Labels (n = ...) ──
    geom_text(
      data        = label_df_top,
      aes(x = .data[[group_col]], y = actual_y_top, label = label),
      inherit.aes = FALSE,
      fontface    = "bold",
      size        = text_size
    ) +
    
    # ── FIX: Bottom Labels (N = ...) ──
    geom_text(
      data        = label_df_bottom,
      aes(x = .data[[group_col]], y = actual_y_bottom, label = label),
      inherit.aes = FALSE,
      fontface    = "bold",
      size        = text_size
    )
  
  # Conditionally add the Wilcoxon bracket ONLY if comparisons exist
  if (!is.null(comparisons) && length(comparisons) > 0) {
    p <- p + stat_compare_means(
      comparisons   = comparisons,
      method        = "wilcox.test",
      label         = "p.format",
      label.y       = y_max * 1.08, 
      step.increase = 0.10,          
      bracket.size  = 0.45,
      tip.length    = 0.010,
      size          = text_size
    )
  }
  
  # Continue adding labels and themes to 'p'
  p <- p + 
    labs(
      title = plot_title,
      x     = x_label,
      y     = y_label
    ) +
    
    # Dynamically scale the y-limit so brackets and text don't get cut off
    coord_cartesian(ylim = c(min(0, actual_y_bottom), actual_y_top * 1.05), clip = "off") +
    
    scale_y_continuous(breaks = scales::breaks_pretty(n = n_breaks)) +
    
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title       = element_text(hjust = 0.5,
                                      size  = 28,
                                      face  = "bold",
                                      margin = margin(b = 15),
                                      lineheight = 1.2),
      
      axis.text.x      = element_text(size = 24, face = "bold", 
                                      margin = margin(t = 12), 
                                      hjust = 0.5, vjust = 1),
      axis.text.y      = element_text(size = 24, face = "bold"),
      axis.title       = element_text(size = 26, face = "bold"),
      axis.line        = element_line(linewidth = 0.5),
      
      aspect.ratio     = 0.75,
      
      plot.margin      = margin(t = 10, r = 30, b = 20, l = 20)
    )
  
  return(p)
}











