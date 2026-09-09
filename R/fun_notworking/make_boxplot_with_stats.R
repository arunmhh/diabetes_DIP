make_boxplot_with_stats <- function(data,
                                    x,
                                    y,
                                    comparisons = NULL,
                                    title = "",
                                    ylab = "",
                                    add_mean = TRUE,
                                    jitter_color = "red") {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  
  x_name <- rlang::as_name(x)
  y_name <- rlang::as_name(y)
  
  # --- Data preparation ---
  df <- data %>%
    ungroup() %>%
    mutate(
      {{ y }} := as.numeric({{ y }}),
      {{ x }} := factor({{ x }})
    ) %>%
    drop_na({{ x }}, {{ y }})
  
  # --- Guard: need at least 2 groups with >= 3 obs ---
  group_counts <- df %>% count({{ x }})
  valid_groups <- group_counts %>%
    filter(n >= 3) %>%
    pull({{ x }}) %>%
    as.character()
  
  if (length(valid_groups) < 2) {
    warning("Not enough groups with sufficient observations. Returning plot without p-values.")
    stat_test <- NULL
  }
  
  # --- P-value calculations ---
  if (!is.null(comparisons) && length(valid_groups) >= 2) {
    
    # Drop comparisons where either group has too few observations
    comparisons_valid <- Filter(function(pair) all(pair %in% valid_groups), comparisons)
    
    if (length(comparisons_valid) == 0) {
      warning("No valid comparisons after filtering. Returning plot without p-values.")
      stat_test <- NULL
    } else {
      
      stat_test <- tryCatch({
        raw <- df %>%
          filter({{ x }} %in% valid_groups) %>%
          pairwise_wilcox_test(
            formula         = as.formula(paste(y_name, "~", x_name)),
            p.adjust.method = "BH"
          ) %>%
          add_significance("p.adj") %>%
          add_xy_position(x = x_name)
        
        # --- Robust pair matching (order-insensitive) ---
        # Normalise each comparison to sorted pair string: "A___B"
        comp_keys <- sapply(comparisons_valid, function(pair) {
          paste(sort(pair), collapse = "___")
        })
        
        raw %>%
          filter(
            paste(pmin(group1, group2), pmax(group1, group2), sep = "___") %in% comp_keys
          )
        
      }, error = function(e) {
        warning("Skipping statistics: ", conditionMessage(e))
        NULL
      })
    }
    
  } else {
    stat_test <- NULL
  }
  
  # --- Base plot ---
  p <- ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(color = jitter_color, width = 0.2, alpha = 0.6) +
    labs(title = title, x = NULL, y = ylab) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
    theme_classic() +
    theme(
      axis.text.x     = element_text(size = 16,face = "bold", angle = 0, hjust = 0.5),
      axis.text.y     = element_text(size = 16, face = "bold"),
      axis.title      = element_text(size = 18, face = "bold"),
      plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
      axis.line       = element_line(linewidth = 1),
      legend.position = "none"
    )
  
  # --- Mean point + CI + line ---
  if (add_mean) {
    p <- p +
      stat_summary(
        fun      = mean,
        geom     = "point",
        shape    = 23,
        size     = 3,
        fill     = "brown",
        color    = "brown"
      ) +
      stat_summary(
        fun.data = mean_cl_normal,
        geom     = "errorbar",
        width    = 0.2,
        linewidth = 0.8,
        color    = "brown"
      ) +
      stat_summary(
        fun      = mean,
        geom     = "line",
        aes(group = 1),
        color    = "brown",
        linetype = "dashed",
        linewidth = 1
      )
  }
  
  # --- Significance brackets ---
  if (!is.null(stat_test) && nrow(stat_test) > 0) {
    p <- p + stat_pvalue_manual(
      stat_test,
      label      = "p.adj.signif",
      tip.length = 0.01
    )
  }
  
  return(p)
}