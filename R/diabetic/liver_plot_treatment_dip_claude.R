# ── Libraries ──────────────────────────────────────────────────────────────
library(ggbreak)
library(ggpubr)
library(dplyr)
library(stringr)
library(ggprism)
library(purrr)
library(here)
library(svglite)

# ══════════════════════════════════════════════════════════════════════════
# FUNCTION 1: save_plots
# ══════════════════════════════════════════════════════════════════════════
save_plots <- function(
    plots,
    names,
    dataset_name = "plots",
    base_dir     = "output/figure/diabetes/plots_all_labvalues",
    subfolder    = NULL,
    width        = 8,
    height       = 6,
    dpi          = 600,
    formats      = c("svg", "png")
) {
  
  if (!is.null(subfolder)) {
    base_path <- here::here(base_dir, subfolder)
  } else {
    base_path <- here::here(base_dir)
  }
  
  for (fmt in formats) {
    
    dir_path <- file.path(base_path, paste0(dataset_name, "_plot_", fmt))
    
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    for (i in seq_along(plots)) {
      
      file_name <- paste0(names[i], ".", fmt)
      file_path <- file.path(dir_path, file_name)
      
      ggplot2::ggsave(
        filename = file_path,
        plot     = plots[[i]],
        width    = width,
        height   = height,
        dpi      = dpi,
        device   = fmt
      )
    }
    
    message("✔ Saved ", length(plots), " ", toupper(fmt), " plots to: ", dir_path)
  }
}

# ══════════════════════════════════════════════════════════════════════════
# FUNCTION 2: plot_leber_boxplot
# ══════════════════════════════════════════════════════════════════════════
plot_leber_boxplot <- function(
    all_data,
    diabetic_data,
    gender_data1,
    panel         = "gpt",
    unit_map      = NULL,
    comparisons   = list(
      c("Leber_Diclo", "Leber_Ibu"),
      c("Leber_Diclo", "Leber_Para"),
      c("Leber_Ibu",   "Leber_Para")
    ),
    stat_method   = "wilcox.test",
    stat_label    = "p.format",
    tip_length    = 0.01,
    bracket_size  = 0.5,
    y_break       = c(2000, 4500),
    y_break_scale = 0.3,
    top_margin    = 80,
    colors        = c(
      "Leber_Diclo" = "#4E79A7",
      "Leber_Ibu"   = "#F28E2B",
      "Leber_Para"  = "#59A14F"
    )
) {
  
  # ── Dynamic title & y-label ──────────────────────────────────────────────
  plot_title <- paste(toupper(panel), "Levels by Treatment\n(Liver Cohort)")
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
    filter(
      panel_id == .env$panel,
      grepl("Leber_", kathegorie),
      !str_detect(attribute, "_krankheitsbeginn|dbs"),
      !is.na(diabetes),
      !is.na(kathegorie),
      !is.na(labreads)
    )
  
  # ── 2. Guard ─────────────────────────────────────────────────────────────
  if (nrow(plot_data) == 0) {
    stop("No data remaining after filtering. Check panel, kathegorie, or labreads.")
  }
  
  # ── 3. Summary stats ─────────────────────────────────────────────────────
  group_stats <- plot_data %>%
    group_by(kathegorie) %>%
    summarise(
      N     = n_distinct(bf_ar_m_nummer),
      n     = n(),
      y_top = max(labreads, na.rm = TRUE),
      .groups = "drop"
    )
  
  x_labels    <- setNames(
    paste0(group_stats$kathegorie, "\nN = ", group_stats$N),
    group_stats$kathegorie
  )
  n_label_y   <- y_break[1] * 0.90
  y_max       <- max(plot_data$labreads, na.rm = TRUE)
  n_comp      <- length(comparisons)
  label_y_pos <- y_max * 1.05 * seq(1, by = 0.08, length.out = n_comp)
  
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
    geom_text(
      data        = group_stats,
      aes(x       = factor(kathegorie),
          y       = n_label_y,
          label   = paste0("n = ", n)),
      inherit.aes = FALSE,
      vjust       = 0,
      size        = 3.5,
      fontface    = "bold"
    ) +
    stat_compare_means(
      comparisons   = comparisons,
      method        = stat_method,
      label         = stat_label,
      label.y       = label_y_pos,
      tip.length    = tip_length,
      bracket.size  = bracket_size,
      step.increase = 0.10
    ) +
    scale_x_discrete(labels = x_labels) +
    scale_fill_manual(values   = colors) +
    scale_colour_manual(values = colors) +
    scale_shape_prism() +
    scale_y_break(y_break, scales = y_break_scale) +
    expand_limits(y = max(label_y_pos) * 1.15) +
    guides(y = "prism_offset_minor") +
    labs(
      title = plot_title,
      x     = NULL,
      y     = y_label
    ) +
    theme_prism() +
    theme(
      legend.position  = "none",
      axis.text.x.top  = element_blank(),
      axis.ticks.x.top = element_blank(),
      axis.line.x.top  = element_blank(),
      plot.margin      = margin(t = top_margin, r = 10, b = 10, l = 10, unit = "pt"),
      axis.title.y     = element_text(margin = margin(r = -15, unit = "pt")),
      axis.text.y      = element_text(margin = margin(r = 1,   unit = "pt"))
    )
  
  return(p)
}

# ══════════════════════════════════════════════════════════════════════════
# FUNCTION 3: auto y_break per panel
# ══════════════════════════════════════════════════════════════════════════
get_y_break <- function(data, pid) {
  
  vals <- data %>%
    filter(panel_id == pid, grepl("Leber_", kathegorie), !is.na(labreads)) %>%
    pull(labreads)
  
  if (length(vals) == 0) return(NULL)
  
  p95 <- quantile(vals, 0.95, na.rm = TRUE)
  p99 <- quantile(vals, 0.99, na.rm = TRUE)
  
  if (p99 > p95 * 2) {
    return(c(p95, p99))
  } else {
    return(NULL)
  }
}