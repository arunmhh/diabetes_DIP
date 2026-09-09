library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)

# ══════════════════════════════════════════════════════════════════════════════
# Helper 1: Build label data frames (Corrected N vs n convention)
# ══════════════════════════════════════════════════════════════════════════════

make_label_dfs_faceted <- function(data, group_var, facet_var, patient_id_var) {
  label_df <- data %>%
    group_by({{ facet_var }}, {{ group_var }}) %>%
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

# ══════════════════════════════════════════════════════════════════════════════
# Core Engine: Faceted Lab Comparison Plot (Enforced Sans Font & Tight Aspect)
# ══════════════════════════════════════════════════════════════════════════════

plot_lab_comparison_gender <- function(
    data,
    group_var,
    facet_var,
    y_var,
    patient_id_var,
    comparisons,
    plot_title   = "Comparison of Lab Values",
    x_label      = NULL,
    y_label      = "Lab Value",
    jitter_width = 0.15,          
    jitter_alpha = 0.4,           
    jitter_size  = 3.5,             
    text_size    = 6.0,           # Scaled proportionally for crisp facet outputs
    n_breaks     = 10
) {
  
  # ── Resolve column names ────────────────────────────────────────────────────
  group_col  <- deparse(substitute(group_var))
  facet_col  <- deparse(substitute(facet_var))
  y_col      <- deparse(substitute(y_var))
  id_col     <- deparse(substitute(patient_id_var))
  
  # ── Validation ──────────────────────────────────────────────────────────────
  missing_cols <- setdiff(c(group_col, facet_col, y_col, id_col), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.numeric(data[[y_col]])) {
    data[[y_col]] <- as.numeric(data[[y_col]])
  }
  
  # ── Label data frames ───────────────────────────────────────────────────────
  labels          <- make_label_dfs_faceted(data, {{ group_var }}, {{ facet_var }}, {{ patient_id_var }})
  label_df_top    <- labels$top
  label_df_bottom <- labels$bottom
  
  # ── Create custom X-axis labels ─────────────────────────────────────────────
  custom_x_labels <- setNames(
    paste0(label_df_bottom[[group_col]], "\n", label_df_bottom$label),
    label_df_bottom[[group_col]]
  )
  
  # ── Y-axis positions ────────────────────────────────────────────────────────
  y_max    <- max(data[[y_col]], na.rm = TRUE)
  y_top    <- y_max * 1.48  
  
  # ── Base Plot Layer ──
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
                 size  = 4.0,     
                 fill  = "brown") +
    
    geom_text(
      data        = label_df_top,
      aes(x = .data[[group_col]], y = y_top, label = label),
      inherit.aes = FALSE,
      fontface    = "bold",
      family      = "sans",   # Enforced crisp sans-serif mapping
      size        = text_size
    ) +
    
    # Generate clear panels for your facets side-by-side
    facet_wrap(vars(.data[[facet_col]]), scales = "free_x")
  
  # Wilcoxon Signficance Lines
  if (!is.null(comparisons) && length(comparisons) > 0) {
    p <- p + stat_compare_means(
      comparisons   = comparisons,
      method        = "wilcox.test",
      label         = "p.format",
      label.y       = y_max * 1.12,    
      step.increase = 0.08,          
      bracket.size  = 0.45,          
      tip.length    = 0.010,         
      family        = "sans", # Fixed shape distortion on export
      size          = text_size * 0.95
    )
  }
  
  # Frame layout rules
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
    theme(
      text             = element_text(family = "sans"),
      legend.position  = "none",
      plot.title       = element_text(hjust = 0.5,
                                      size  = 24, 
                                      face  = "bold",
                                      family = "sans",
                                      margin = margin(b = 15),
                                      lineheight = 1.2),
      
      axis.text.x      = element_text(size = 20, face = "bold", family = "sans",
                                      margin = margin(t = 12), 
                                      hjust = 0.5, vjust = 1),
      axis.text.y      = element_text(size = 20, face = "bold", family = "sans"),
      axis.title       = element_text(size = 22, face = "bold", family = "sans"),
      axis.line        = element_line(linewidth = 0.5),
      
      # Rules for the strip labels (Faceted Panel Titles: Male / Female)
      strip.background = element_blank(),
      strip.text       = element_text(size = 22, face = "bold", family = "sans", 
                                      margin = margin(b = 10)),
      
      aspect.ratio     = 0.85, # Compacts the wide landscape empty spaces
      plot.margin      = margin(t = 15, r = 25, b = 45, l = 20)
    )
  
  return(p)
}


# ══════════════════════════════════════════════════════════════════════════════
# Master Loop Function: DIABETES VS NON-DIABETES BY GENDER (LIVER COHORT)
# ══════════════════════════════════════════════════════════════════════════════

generate_and_save_gender_faceted_plots <- function(
    panel_ids,
    all_data,
    diabetic_data,
    gender_data,
    
    save_plots_flag     = TRUE,
    export_dataset_name = "lab_plots",
    export_base_dir     = "output",
    export_subfolder    = "gender_faceted_comparisons",
    # ── THE FIX: Balanced, professional dimensions for clean side-by-side panels ──
    export_width        = 10.5, 
    export_height       = 6.5,
    export_dpi          = 600,
    export_formats      = c("svg", "png")
) {
  
  unit_map <- c(
    "got" = "AST/GOT (U/L)", "gpt" = "ALT/GPT (U/L)", "ggt" = "GGT (U/L)",
    "ap" = "Alkaline Phosphatase (U/L)", "gldh" = "GLDH (U/L)", "ldh" = "LDH (U/L)",
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
    "gesamteiweiss" = "Total Protein (g/dL)", "albumin" = "Albumin (g/dL)",
    "alpha" = "Alpha-Globulin (%)", "beta" = "Beta-Globulin (%)", "gamma" = "Gamma-Globulin (%)",
    "amylase" = "Amylase (U/L)", "lipase" = "Lipase (U/L)",
    "crp" = "CRP (mg/L)", "bsg" = "ESR (mm/h)", "leukozyten" = "Leukocytes (G/L)", "fieber" = "Temperature (°C)",
    "quick" = "Quick (%)", "inr" = "INR", "ptt" = "PTT (s)", "prothrombin" = "Prothrombin Time (s)", 
    "tz" = "Thrombin Time (s)", "antithrombin" = "Antithrombin III (%)", "antithrombin_3" = "Antithrombin III (%)",
    "fibrinogen" = "Fibrinogen (mg/dL)", "d" = "D-Dimer (mg/L)",
    "erythrozyten" = "Erythrocytes (T/L)", "hb" = "Hemoglobin (g/dL)", "hamatokrit" = "Hematocrit (%)", 
    "mcv" = "MCV (fL)", "mch" = "MCH (pg)", "mchc" = "MCHC (g/dL)", 
    "thrombozyten" = "Thrombocytes (G/L)", "trhombozyten" = "Thrombocytes (G/L)",
    "lymphozyten" = "Lymphocytes (%)", "monozyten" = "Monocytes (%)", "eosinophile" = "Eosinophils (%)", 
    "basophile" = "Basophils (%)", "neutrophile" = "Neutrophils (%)", 
    "stabkernige" = "Band Neutrophils (%)", "segmentkernige" = "Segmented Neutrophils (%)",
    "natrium" = "Sodium (mmol/L)", "kalium" = "Potassium (mmol/L)", "kalzium" = "Calcium (mmol/L)", 
    "magnesium" = "Magnesium (mmol/L)", "chlorid" = "Chloride (mmol/L)", "phosphat" = "Phosphate (mg/dL)",
    "serum_eisen" = "Serum Iron (µg/dL)", "transferrin" = "Transferrin (mg/dL)",
    "ferritin" = "Ferritin (µg/L)", "haptoglobin" = "Haptoglobin (mg/dL)",
    "total" = "Total Value", "totales" = "Total Value", "blut" = "Blood Value", "serum" = "Serum Value"
  )
  
  plots_list <- list()
  plot_names <- c()
  
  for (pid in panel_ids) {
    
    # Data Processing pipeline
    panel_data <- all_data %>%
      left_join(diabetic_data, by = "bf_ar_m_nummer", relationship = "many-to-many") %>%
      left_join(gender_data, by = c("bf_ar_m_nummer", "treatment"), relationship = "many-to-many") %>%
      filter(panel_id == pid) %>%
      filter(grepl("Leber_", treatment)) %>%
      dplyr::filter(!str_detect(attribute, "_krankheitsbeginn|DBS")) %>%
      filter(!is.na(gender), !is.na(diabetes), !is.na(treatment)) %>% 
      mutate(
        diabetes = factor(diabetes),
        gender   = factor(gender)
      ) %>%
      filter(labreads < 15000) %>%
      drop_na(labreads, diabetes, gender)
    
    if (nrow(panel_data) == 0) {
      message(paste("Skipping", toupper(pid), "- No data available after filtering."))
      next
    }
    
    num_groups <- length(unique(panel_data$diabetes))
    if (num_groups < 2) {
      message(paste("Skipping", toupper(pid), "- Fewer than 2 diabetes groups present."))
      next
    }
    
    plot_title <- paste(toupper(pid), "by Diabetes Status & Gender\n(Liver Treatment Cohort)")
    y_label    <- if (pid %in% names(unit_map)) unit_map[[pid]] else toupper(pid)
    
    # ── Call updated engine with explicit facet configurations ──
    p <- plot_lab_comparison_gender(
      data           = panel_data,
      group_var      = diabetes,
      facet_var      = gender, 
      y_var          = labreads,
      patient_id_var = bf_ar_m_nummer,
      comparisons    = make_comparisons(panel_data, "diabetes"), 
      plot_title     = plot_title,
      y_label        = y_label
    )
    
    plots_list[[length(plots_list) + 1]] <- p
    plot_names <- c(plot_names, paste0("Plot_", toupper(pid), "_Diabetes_ByGender"))
    
    message(paste("Successfully generated faceted plot for:", toupper(pid)))
  }
  
  if (save_plots_flag && length(plots_list) > 0) {
    message("\nSaving generated plots...")
    save_plots(
      plots        = plots_list,
      names        = plot_names,
      dataset_name = export_dataset_name,
      base_dir     = export_base_dir,
      subfolder    = export_subfolder,
      width        = export_width,   
      height       = export_height,  
      dpi          = export_dpi,
      formats      = export_formats
    )
  }
  
  names(plots_list) <- plot_names
  return(plots_list)
}