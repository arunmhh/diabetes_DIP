gender_plot_lab_comparison <- function(
    data,
    group_var,
    y_var,
    patient_id_var,
    comparisons,
    plot_title    = "Comparison of Lab Values",
    x_label       = NULL,
    y_label       = "Lab Value",
    jitter_width  = 0.15,
    jitter_alpha  = 0.5,
    jitter_size   = 3,
    text_size     = 4.5,
    n_breaks      = 10,
    step_increase = 0.11   # ← new parameter: controls bracket staggering
) {
  
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(scales)
  
  # ── Resolve column names -----------------------------------------------------
  group_col <- deparse(substitute(group_var))
  y_col     <- deparse(substitute(y_var))
  id_col    <- deparse(substitute(patient_id_var))
  
  # ── Validation ---------------------------------------------------------------
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
  
  # ── Label data frames --------------------------------------------------------
  labels          <- make_label_dfs(data, {{ group_var }}, {{ patient_id_var }})
  label_df_top    <- labels$top
  label_df_bottom <- labels$bottom
  
  # ── Y-axis positions (dynamic based on number of comparisons) ---------------
  y_max       <- max(data[[y_col]], na.rm = TRUE)
  n_comp      <- length(comparisons)
  
  # extra headroom: 0.15 per comparison bracket
  y_expansion <- 1.22 + (n_comp - 1) * 0.22
  y_top       <- y_max * y_expansion
  y_bottom    <- y_max * -0.40
  ylim_top    <- y_max * (y_expansion + 0.10)
  
  # ── Plot ---------------------------------------------------------------------
  ggplot(data,
         aes(x    = .data[[group_col]],
             y    = .data[[y_col]],
             fill = .data[[group_col]])) +
    
    # geom_violin(trim = FALSE, alpha = 0.5, color = "black") +   # violin commented out
    
    geom_boxplot(width         = 0.20,
                 outlier.shape = NA,
                 alpha         = 0.5,
                 color         = "black") +
    
    geom_jitter(width = jitter_width,
                alpha = jitter_alpha,
                size  = jitter_size,
                color = "#CC79A7") +
    
    stat_summary(fun   = median,
                 geom  = "point",
                 shape = 23,
                 size  = 4,
                 fill  = "brown") +
    
    geom_text(
      data        = label_df_top,
      aes(x = .data[[group_col]], y = y_top, label = label),
      inherit.aes = FALSE,
      fontface    = "plain",
      size        = text_size
    ) +
    
    geom_text(
      data        = label_df_bottom,
      aes(x = .data[[group_col]], y = y_bottom, label = label),
      inherit.aes = FALSE,
      fontface    = "plain",
      size        = text_size
    ) +
    
    stat_compare_means(
      comparisons   = comparisons,
      method        = "wilcox.test",
      label         = "p.format",
      label.y       = y_max * 1.02,   # starting height of first bracket
      step.increase = step_increase,  # ← each bracket steps up by this fraction
      bracket.size  = 1,
      tip.length    = 0.02,
      size          = text_size
    ) +
    
    labs(
      title = plot_title,
      x     = x_label,
      y     = y_label
    ) +
    
    coord_cartesian(ylim = c(0, ylim_top), clip = "off") +
    
    scale_y_continuous(breaks = scales::breaks_pretty(n = n_breaks)) +
    
    theme_classic() +
    theme(
      legend.position  = "none",
      plot.title       = element_text(hjust      = 0.5,size= 20,
                                      face       = "bold",
                                      margin     = margin(b = 10),
                                      lineheight = 1.2),
      axis.text.x      = element_text(size = 16, face = "bold"),
      axis.text.y      = element_text(size = 16, face = "bold"),
      axis.title       = element_text(size = 18, face = "bold"),
      axis.line        = element_line(linewidth = 1),
      plot.margin      = margin(t = 10, r = 10, b = 40, l = 10)
    )
}