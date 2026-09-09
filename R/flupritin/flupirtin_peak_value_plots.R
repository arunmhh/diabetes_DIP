## flupirtin peak valuepots##

library(ggplot2)
library(dplyr)
library(ggpubr)
library(stringr)

# ==========================================
# 1. PLOT FUNCTION (WITH UNIT MAPPING)
# ==========================================
plot_flupirtin_comparison <- function(
    data,
    group_var,
    y_var,
    patient_id_var,
    panel_id,
    unit_map,
    plot_title = "",
    text_size = 4
) {
  
  group_col <- deparse(substitute(group_var))
  y_col     <- deparse(substitute(y_var))
  id_col    <- deparse(substitute(patient_id_var))
  
  # ------------------------------
  # y-axis label from unit_map
  # ------------------------------
  y_label <- if (panel_id %in% names(unit_map)) {
    unit_map[[panel_id]]
  } else {
    toupper(panel_id)
  }
  
  # ------------------------------
  # Unique patients (N)
  # ------------------------------
  df_top <- data %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      unique_patients = n_distinct(.data[[id_col]]),
      .groups = "drop"
    ) %>%
    mutate(label = paste0("N=", unique_patients))
  
  # ------------------------------
  # Total observations (n)
  # ------------------------------
  df_bottom <- data %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      total_reads = n(),
      .groups = "drop"
    ) %>%
    mutate(label = paste0("n=", total_reads))
  
  y_max <- max(data[[y_col]], na.rm = TRUE)
  
  p <- ggplot(
    data,
    aes(
      x = .data[[group_col]],
      y = .data[[y_col]],
      fill = .data[[group_col]]
    )
  ) +
    
    geom_violin(
      trim = TRUE,
      alpha = 0.35,
      colour = "black",
      linewidth = 0.3
    ) +
    
    geom_boxplot(
      width = 0.15,
      alpha = 0.7,
      outlier.shape = NA,
      linewidth = 0.3
    ) +
    
    geom_jitter(
      width = 0.08,
      alpha = 0.4,
      size = 1.5
    ) +
    
    stat_summary(
      fun = median,
      geom = "point",
      shape = 23,
      size = 2.5,
      fill = "brown"
    ) +
    
    geom_text(
      data = df_bottom,
      aes(
        x = .data[[group_col]],
        y = y_max * 1.10,
        label = label
      ),
      inherit.aes = FALSE,
      fontface = "bold",
      size = text_size
    ) +
    
    geom_text(
      data = df_top,
      aes(
        x = .data[[group_col]],
        y = -(y_max * 0.25),
        label = paste0("(", label, ")")
      ),
      inherit.aes = FALSE,
      fontface = "bold",
      size = text_size
    ) +
    
    stat_compare_means(
      method = "wilcox.test",
      comparisons = list(c("No_Flupirtin", "Flupirtin")),
      label = "p.format",
      label.y = y_max * 1.12,
      bracket.size = 0.4,
      tip.length = 0.01,
      size = text_size
    ) +
    
    labs(
      title = plot_title,
      x = NULL,
      y = y_label
    ) +
    
    coord_cartesian(
      ylim = c(0, y_max * 1.25),
      clip = "off"
    ) +
    scale_fill_manual(
      values = c(
        "No_Flupirtin" = "#E69F00",
        "Flupirtin"    = "#009E73"
      )
    )+
    
    theme_classic() +
    
    theme(
      text              = element_text(family = "sans"),
      legend.position = "none",
      legend.background = element_blank(),
      legend.box        = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(size = 14, face = "bold"), 
      legend.title      = element_blank(),
      plot.title        = element_text(hjust = 0.5, size = 16, face = "bold",
                                       margin = margin(b = 10)),
      axis.text.x       = element_text(size = 14, face = "bold", 
                                       margin = margin(t = 3)), 
      axis.text.y       = element_text(size = 14, face = "bold", 
                                       margin = margin(r = 6)),
      axis.title        = element_text(size = 15, face = "bold"), 
      axis.line         = element_line(linewidth = 0.40),
      plot.margin       = margin(t = 15, r = 15, b = 20, l = 15) 
    )
  
  return(p)
}

# ==========================================
# 2. LOOP FUNCTION (ALL PANELS)
# ==========================================
generate_flupirtin_plots <- function(
    panel_ids,
    all_data,
    flupirtin_data,
    unit_map
) {
  
  plots <- list()
  
  for (pid in panel_ids) {
    
    message("Processing: ", pid)
    
    panel_data <- all_data %>%
      left_join(flupirtin_data,
                by = join_by("bf_ar_m_nummer","treatment"),
                relationship = "many-to-many") %>%
      filter(
        panel_id == pid,
        treatment == "Leber_Diclo"
      ) %>%
      filter(
        !str_detect(attribute, "_from_start|dbs")
      ) %>%
      drop_na(labreads, flupirtin, bf_ar_m_nummer) %>%
      mutate(
        flupirtin = case_when(
          flupirtin %in% c("Yes", "Flupirtin", "flupirtin", 1) ~ "Flupirtin",
          TRUE ~ "No_Flupirtin"
        ),
        flupirtin = factor(
          flupirtin,
          levels = c("No_Flupirtin", "Flupirtin")
        )
      )
    
    if (nrow(panel_data) == 0) next
    if (length(unique(panel_data$flupirtin)) < 2) next
    
    p <- plot_flupirtin_comparison(
      data = panel_data,
      group_var = flupirtin,
      y_var = labreads,
      patient_id_var = bf_ar_m_nummer,
      panel_id = pid,
      unit_map = unit_map,
      plot_title <- paste("Liver_Diclo:", toupper(pid), "\n(Flupirtin vs Non Flupirtin peak values)")
    )
    
    plots[[pid]] <- p
  }
  
  return(plots)
}

# UNIT MAP (YOUR VERSION)

  unit_map <- c(
    "got" = "AST/GOT (U/L)", 
    "gpt" = "ALT/GPT (U/L)", 
    "ggt" = "GGT (U/L)",
    "ap" = "Alkaline Phosphatase (U/L)", 
    "gldh" = "GLDH (U/L)", 
    "ldh" = "LDH (U/L)",
    "cholinesterase" = "Cholinesterase (U/L)", "bilirubin" = "Total Bilirubin (mg/dL)",
    "total_bilirubin" = "Total Bilirubin (mg/dL)", "direktes" = "Direct Bilirubin (mg/dL)",
    "direct_bilirubin" = "Direct Bilirubin (mg/dL)", "indirektes" = "Indirect Bilirubin (mg/dL)",
    "indirect_bilirubin" = "Indirect Bilirubin (mg/dL)", "ammoniak" = "Ammonia (µg/dL)",
    "ck" = "CK (U/L)", "keratinkinase" = "Creatine Kinase (U/L)", "ck_mb" = "CK-MB (U/L)",
    "troponin" = "Troponin (ng/L)", "serummyoglobin" = "Myoglobin (µg/L)",
    "kreatinin" = "Creatinine (mg/dL)", "harnstoff" = "Urea (mg/dL)",
    "harnsaure" = "Uric Acid (mg/dL)", "gfr" = "eGFR (mL/min/1.73m²)",
    "grf" = "eGFR (mL/min/1.73m²)", "glomarulare" = "eGFR (mL/min/1.73m²)",
    "glucose" = "Glucose (mg/dL)", "lactat" = "Lactate (mmol/L)",
    "cholesterin" = "Cholesterol (mg/dL)", "triglyceride" = "Triglycerides (mg/dL)",
    "hdl" = "HDL Cholesterol (mg/dL)", "ldl" = "LDL Cholesterol (mg/dL)",
    "gesamteiweiss" = "Total Protein (g/dL)", "albumin" = "Albumin (g/dL)"
  )