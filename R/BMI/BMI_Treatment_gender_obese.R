library(tidyverse)
library(rstatix)
library(ggpubr)
library(purrr)
library(glue)

# ══════════════════════════════════════════════════════════════════════════════
# FUNCTION
# ══════════════════════════════════════════════════════════════════════════════

plot_liver_treatment_sex_obesity <- function(
    all_data,
    gender_bmi_data,
    panel,
    y_var       = "labreads",
    y_label     = "GPT Activity (U/L)",
    title       = "GPT Activity by Treatment, Sex, and Obesity Status",
    max_filter  = 4000,
    y_limits    = c(0, 6000),
    y_breaks    = seq(0, 6000, by = 1000),
    pval_steps  = c(600, 1300, 2000),
    colors      = c("#0072B2", "#E69F00", "#009E73"),
    comparisons = list(
      c("Liver_Diclo", "Liver_Ibu"),
      c("Liver_Diclo", "Liver_Para"),
      c("Liver_Ibu",   "Liver_Para")
    )
) {
  
  # ── 1. Prepare data ──────────────────────────────────────────────────────────
  plot_data <- all_data %>%
    left_join(
      gender_bmi_data,
      by           = join_by("bf_ar_m_nummer", "treatment"),
      relationship = "many-to-many"
    ) %>%
    filter(
      panel_id == .env$panel,
      grepl("Leber_", treatment),
      !str_detect(attribute, "_from_start|dbs|_dbs"),
      !is.na(bmi),
      !is.na(treatment),
      !is.na(.data[[y_var]]),
      !is.na(days)
    ) %>%
    mutate(
      treatment = recode(treatment,
                         "Leber_Diclo" = "Liver_Diclo",
                         "Leber_Ibu"   = "Liver_Ibu",
                         "Leber_Para"  = "Liver_Para"
      ),
      gender = factor(gender, levels = c("Female", "Male")),
      obese  = factor(
        recode(obese, "yes" = "obese", "no" = "no_obese"),
        levels = c("obese", "no_obese")
      ),
      facet_group = paste(gender, obese, sep = " "),
      facet_group = recode(facet_group,
                           "Female no_obese" = "Female / Non-obese",
                           "Female obese"    = "Female / Obese",
                           "Male no_obese"   = "Male / Non-obese",
                           "Male obese"      = "Male / Obese"
      )
    ) %>%
    distinct(bf_ar_m_nummer, .data[[y_var]], .keep_all = TRUE) %>%
    filter(.data[[y_var]] <= max_filter)
  
  # ── Guard 1: no data at all ──────────────────────────────────────────────────
  if (nrow(plot_data) == 0) {
    warning("Panel '", panel, "': no data after filtering. Skipping.")
    return(NULL)
  }
  
  # ── Guard 2: facet groups with all 3 treatments ──────────────────────────────
  valid_groups <- plot_data %>%
    group_by(facet_group) %>%
    summarise(n_treatments = n_distinct(treatment), .groups = "drop") %>%
    filter(n_treatments == 3) %>%
    pull(facet_group)
  
  skipped <- setdiff(unique(plot_data$facet_group), valid_groups)
  if (length(skipped) > 0) {
    warning("Panel '", panel, "': skipping group(s) with < 3 treatments: ",
            paste(skipped, collapse = ", "))
  }
  
  # ── Guard 3: no valid groups remain ─────────────────────────────────────────
  if (length(valid_groups) == 0) {
    warning("Panel '", panel, "': no facet groups with all 3 treatments. Skipping.")
    return(NULL)
  }
  
  plot_data <- filter(plot_data, facet_group %in% valid_groups)
  
  # ── 2. Statistical tests ─────────────────────────────────────────────────────
  test_formula <- as.formula(paste(y_var, "~ treatment"))
  
  stat_test <- plot_data %>%
    group_by(facet_group) %>%
    group_split() %>%
    map(\(df) {
      fg    <- unique(df$facet_group)
      max_y <- max(df[[y_var]], na.rm = TRUE)
      
      tryCatch({
        df %>%
          wilcox_test(test_formula, comparisons = comparisons) %>%
          adjust_pvalue(method = "BH") %>%
          add_significance("p.adj") %>%
          add_xy_position(x = "treatment") %>%
          mutate(
            facet_group = fg,
            max_y       = max_y
          )
      }, error = function(e) {
        warning("Panel '", panel, "', group '", fg, "': test failed — ", e$message)
        NULL
      })
    }) %>%
    compact() %>%
    bind_rows()
  
  # ── Guard 4: all tests failed ────────────────────────────────────────────────
  if (nrow(stat_test) == 0) {
    warning("Panel '", panel, "': all statistical tests failed. Skipping.")
    return(NULL)
  }
  
  # ── Filter significant and assign compact y.positions ────────────────────────
  step_size <- pval_steps[2] - pval_steps[1]
  
  sig_tests <- stat_test %>%
    #filter(p.adj < 0.05) %>%
    group_by(facet_group) %>%
    mutate(
      y.position = first(max_y) + pval_steps[1] + (row_number() - 1) * step_size
    ) %>%
    ungroup() %>%
    select(-max_y)
  
  # ── 3. Plot ──────────────────────────────────────────────────────────────────
  ggplot(plot_data, aes(x = treatment, y = .data[[y_var]], color = treatment)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, alpha = 0.6) +
    facet_wrap(~facet_group) +
    { if (nrow(sig_tests) > 0) stat_pvalue_manual(sig_tests, label = "p {p.adj}") } +
     scale_x_discrete(labels = c(
     "Liver_Diclo" = "Diclofenac",
     "Liver_Ibu"   = "Ibuprofen",
     "Liver_Para"  = "Paracetamol"
     )) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_color_manual(values = colors) +
    labs(y = y_label, title = title) +
    theme_classic() +
    theme(
      strip.text       = element_text(size = 16, face = "bold", margin = margin(b = 5)),
      strip.background = element_blank(),
      text             = element_text(family = "sans"),
      legend.position  = "none",
      plot.title       = element_text(hjust = 0.5, size = 25, face = "bold",
                                      margin = margin(b = 10)),
      axis.text.x      = element_text(size = 14, face = "bold", margin = margin(t = 1)),
      axis.text.y      = element_text(size = 18, face = "bold"),
      axis.title       = element_text(size = 24, face = "bold"),
      plot.margin      = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )
}