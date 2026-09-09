# ══════════════════════════════════════════════════════════════════════════════
# Main: Plot lab comparison between groups (Sans Font Enforced Everywhere)
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
    jitter_width = 0.15,          
    jitter_alpha = 0.4,           
    jitter_size  = 4,             
    text_size    = 6.5,           
    n_breaks     = 10
) {
  
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
  
  # ── Create custom X-axis labels (Group Name \n (N=XX)) ──────────────────────
  custom_x_labels <- setNames(
    paste0(label_df_bottom[[group_col]], "\n", label_df_bottom$label),
    label_df_bottom[[group_col]]
  )
  
  # ── Y-axis positions ────────────────────────────────────────────────────────
  y_max    <- max(data[[y_col]], na.rm = TRUE)
  y_top    <- y_max * 1.48  
  
  # ── Base Plot ───────────────────────────────────────────────────────────────
  p <- ggplot(data,
              aes(x    = .data[[group_col]],
                  y    = .data[[y_col]],
                  fill = .data[[group_col]])) +
    
    geom_boxplot(width         = 0.50, 
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
    
    # ── FIXED: Added family = "sans" to the top sample size markers ──
    geom_text(
      data        = label_df_top,
      aes(x = .data[[group_col]], y = y_top, label = label),
      inherit.aes = FALSE,
      fontface    = "bold",
      family      = "sans",
      size        = text_size
    )
  
  # Significance Brackets layer
  if (!is.null(comparisons) && length(comparisons) > 0) {
    p <- p + stat_compare_means(
      comparisons   = comparisons,
      method        = "wilcox.test",
      label         = "p.format",
      label.y       = y_max * 1.12,    
      step.increase = 0.08,          
      bracket.size  = 0.45,          
      tip.length    = 0.010,         
      # ── FIXED: Passed family = "sans" straight into the p-value label text generator ──
      family        = "sans",
      size          = text_size * 0.95
    )
  }
  
  # Continue adding labels and themes to 'p'
  p <- p + 
    labs(
      title = plot_title,
      x     = NULL,
      y     = y_label
    ) +
    
    coord_cartesian(ylim = c(0, y_max * 1.60), clip = "off") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = n_breaks)) +
    scale_x_discrete(labels = custom_x_labels) +
    
    theme_classic() +
    # ── FIXED: Enforced family = "sans" globally inside the theme element components ──
    theme(
      text             = element_text(family = "sans"),
      legend.position  = "none",
      plot.title       = element_text(hjust = 0.5,
                                      size  = 26, 
                                      face  = "bold",
                                      family = "sans",
                                      margin = margin(b = 15),
                                      lineheight = 1.2),
      
      axis.text.x      = element_text(size = 22, face = "bold", family = "sans",
                                      margin = margin(t = 12), 
                                      hjust = 0.5, vjust = 1),
      axis.text.y      = element_text(size = 22, face = "bold", family = "sans"),
      axis.title       = element_text(size = 24, face = "bold", family = "sans"),
      axis.line        = element_line(linewidth = 0.5),
      
      aspect.ratio     = 0.80, 
      plot.margin      = margin(t = 15, r = 25, b = 45, l = 20)
    )
  
  return(p)
}