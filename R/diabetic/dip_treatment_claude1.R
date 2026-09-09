# ── Libraries ──────────────────────────────────────────────────────────────
library(ggbreak)
library(ggpubr)
library(dplyr)
library(stringr)
library(ggprism)

# ── Function ───────────────────────────────────────────────────────────────
plot_leber_boxplot <- function(
    all_data,
    diabetic_data,
    gender_data1,
    panel         = "gpt",
    unit_map      = NULL,
    comparisons   = list(
      c("Liver_Diclo", "Liver_Ibu"),
      c("Liver_Diclo", "Liver_Para"),
      c("Liver_Ibu",   "Liver_Para")
    ),
    stat_method   = "wilcox.test",
    stat_label    = "p.format",
    tip_length    = 0.01,
    bracket_size  = 0.5,
    y_break       = c(3000, 4500),
    y_break_scale = 0.3,
    top_margin    = 80,
    colors        = c(
      "Liver_Diclo" = "#4E79A7",
      "Liver_Ibu"   = "#F28E2B",
      "Liver_Para"  = "#59A14F"
    )
) {
  
  # ── Dynamic title & y-label ───────────────────────────────────────────────
  plot_title <- paste(toupper(panel), "Levels by treatment (Liver Cohort)")
  y_label    <- if (!is.null(unit_map) && panel %in% names(unit_map)) {
    unit_map[[panel]]
  } else {
    toupper(panel)
  }
  
  # ── 1. Data wrangling ────────────────────────────────────────────────────
  plot_data <- all_data %>%
    left_join(diabetic_data,
              by = "bf_ar_m_nummer",
              relationship = "many-to-many") %>%
    left_join(gender_data1,
              by = join_by("bf_ar_m_nummer", "treatment"),
              relationship = "many-to-many") %>%
    mutate(kathegorie = recode(kathegorie,
                               "Leber_Diclo" = "Liver_Diclo", 
                               "Leber_Ibu"   = "Liver_Ibu",
                               "Leber_Para"  = "Liver_Para")) %>%
    filter(
      panel_id == .env$panel,
      grepl("Liver_", kathegorie),
      !str_detect(attribute, "_krankheitsbeginn|dbs"),
      !is.na(diabetes),
      !is.na(kathegorie),
      !is.na(labreads)
    )
  
  # ── 2. Guard ─────────────────────────────────────────────────────────────
  if (nrow(plot_data) == 0) {
    stop("No data remaining after filtering. Check panel, kathegorie, or labreads.")
  }
  
  # ── 3. Summary stats: N (patients) & n (lab reads) per group ────────────
  group_stats <- plot_data %>%
    group_by(kathegorie) %>%
    summarise(
      N     = n_distinct(bf_ar_m_nummer),
      n     = n(),
      y_top = max(labreads, na.rm = TRUE),
      .groups = "drop"
    )
  
  # x-axis labels: kathegorie + N underneath
  x_labels <- setNames(
    paste0(group_stats$kathegorie, "\nN = ", group_stats$N),
    group_stats$kathegorie
  )
  
  # n label position: just below the lower break boundary
  n_label_y <- max(group_stats$y_top) * 1.05
  
  # stat bracket positions: staggered above ymax
  y_max       <- max(plot_data$labreads, na.rm = TRUE)
  n_comp      <- length(comparisons)
  label_y_pos <- y_max * 1.14 * seq(1, by = 0.08, length.out = n_comp)
  
  # ── 4. Plot ──────────────────────────────────────────────────────────────
  p <- ggplot(
    plot_data,
    aes(
      x      = factor(kathegorie),
      y      = labreads,
      fill   = factor(kathegorie),
      colour = factor(kathegorie)
    )
  ) +
    geom_boxplot(
      width         = 0.6,
      alpha         = 0.35,
      linewidth     = 0.8,
      outlier.shape = NA
    ) +
    geom_jitter(
      aes(shape = factor(kathegorie)),
      width = 0.2,
      size  = 2,
      alpha = 0.8
    ) +
    # n = lab reads just below break per group
    geom_text(
      data        = group_stats,
      aes(x       = factor(kathegorie),
          y       = n_label_y,
          label   = paste0("n = ", n)),
      inherit.aes = FALSE,
      vjust       = 0,
      size        = 5,
      fontface    = "bold"
    ) +
    # pairwise statistical comparisons
    stat_compare_means(
      comparisons   = comparisons,
      method        = stat_method,
      label         = stat_label,
      label.y       = label_y_pos,
      tip.length    = tip_length,
      bracket.size  = bracket_size,
      step.increase = 0.10,
      size = 5,
      fontface = "bold") +
    
    
    scale_x_discrete(labels = x_labels) +
    scale_fill_manual(values   = colors) +
    scale_colour_manual(values = colors) +
    scale_shape_prism() +
    scale_y_break(y_break, scales = y_break_scale) +
    expand_limits(y = max(label_y_pos) * 1.45) +   # ← add this line
    guides(y = "prism_offset_minor") +
    labs(
      title = plot_title,             # ← auto title
      x     = NULL,
      y     = y_label                 # ← auto y-label from unit_map
    ) +
    theme_prism() +
    theme(
      text             = element_text(family = "sans"),
      legend.position  = "none",
      plot.title       = element_text(hjust = 0.5,
                                      size  = 26,
                                      face  = "bold",
                                      family = "sans",
                                      margin = margin(b = 2),
                                      lineheight = 1.2),
      
      axis.text.x      = element_text(size = 22, face = "bold", family = "sans",
                                      margin = margin(t = 6),
                                      hjust = 0.5, vjust = 1),
      axis.text.y      = element_text(size = 22, face = "bold", family = "sans"),
      axis.title       = element_text(size = 24, face = "bold", family = "sans"),
      axis.title.y     = element_text(margin = margin(r = -25, unit = "pt")),
      #axis.line        = element_line(linewidth = 0.5),
      axis.text.x.top  = element_blank(),
      axis.ticks.x.top = element_blank(),
      axis.line.x.top  = element_blank(),
      plot.margin      = margin(t = top_margin, r = 10, b = 10, l = 10, unit = "pt")
    )
  
  return(p)
}