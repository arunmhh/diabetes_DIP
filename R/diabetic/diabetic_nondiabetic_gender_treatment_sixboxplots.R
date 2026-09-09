library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)

# ══════════════════════════════════════════════════════════════════════════════
# Helper Function: Generate N and n labels for 3-way faceted plots
# ══════════════════════════════════════════════════════════════════════════════

make_label_dfs_3way <- function(data, group_col, fill_col, facet_col, id_col) {
  group_sym <- sym(group_col)
  fill_sym  <- sym(fill_col)
  facet_sym <- sym(facet_col)
  id_sym    <- sym(id_col)
  
  df_top <- data %>%
    group_by(!!facet_sym, !!group_sym, !!fill_sym) %>%
    summarise(unique_patients = n_distinct(!!id_sym), .groups = "drop") %>%
    mutate(label = paste0("N = ", unique_patients)) %>%
    select(-unique_patients)
  
  df_bottom <- data %>%
    group_by(!!facet_sym, !!group_sym, !!fill_sym) %>%
    summarise(total_reads = n(), .groups = "drop") %>%
    mutate(label = paste0("n = ", total_reads)) %>%
    select(-total_reads)
  
  return(list(top = df_top, bottom = df_bottom))
}

# ══════════════════════════════════════════════════════════════════════════════
# Main: Plot 6 Boxplots (SVG Cross-Format Text Compatibility)
# ══════════════════════════════════════════════════════════════════════════════

plot_lab_6box_layout <- function(
    data, group_var, fill_var, facet_var, y_var, patient_id_var,  
    plot_title = "Comparison of Lab Values", y_label = "Lab Value",
    text_size = 3, n_breaks = 10
) {
  
  group_col <- as.character(substitute(group_var))
  fill_col  <- as.character(substitute(fill_var))
  facet_col <- as.character(substitute(facet_var))
  y_col     <- as.character(substitute(y_var))
  id_col    <- as.character(substitute(patient_id_var))
  
  if (!is.numeric(data[[y_col]])) data[[y_col]] <- as.numeric(data[[y_col]])
  
  data[[group_col]] <- factor(gsub("Leber_", "", as.character(data[[group_col]])),
                              levels = c("Diclo", "Ibu", "Para"))
  
  labels          <- make_label_dfs_3way(data, group_col, fill_col, facet_col, id_col)
  label_df_top    <- labels$top      
  label_df_bottom <- labels$bottom   
  
  y_max <- max(data[[y_col]], na.rm = TRUE)
  
  p <- ggplot(data, aes(x = .data[[group_col]], 
                        y = .data[[y_col]], 
                        fill = .data[[fill_col]])) +
    
    geom_boxplot(width = 0.50, outlier.shape = NA, 
                 alpha = 0.6, color = "black", linewidth = 0.35,
                 position = position_dodge(0.60)) +
    
    geom_jitter(aes(color = .data[[fill_col]]), 
                alpha = 0.35, size = 1.2, 
                position = position_jitterdodge(jitter.width = 0.12,
                                                dodge.width = 0.60)) +
    
    stat_summary(fun = median, geom = "point", shape = 23, 
                 size = 2, fill = "brown", 
                 position = position_dodge(0.60)) +
    
    # ── FIXED: Position dodge matched perfectly to 0.60 for SVG text rendering ──
    geom_text(data = label_df_bottom, aes(x = .data[[group_col]], 
                                          y = y_max * 1.05, 
                                          label = paste0("(", label, ")"), 
                                          group = .data[[fill_col]]), 
              inherit.aes = FALSE, fontface = "bold", 
              family = "sans",
              size = text_size * 1.05, 
              position = position_dodge(0.90), vjust = 0) +
    
    stat_compare_means(aes(group = .data[[fill_col]]), 
                       method = "wilcox.test", label = "p.format", 
                       label.y = y_max * 1.20, 
                       bracket.size = 0.40, 
                       tip.length = 0.01, 
                       family = "sans",
                       size = text_size * 0.9) +
    
    # ── FIXED: Shifted N values to -0.22 and matched dodge metrics to solve vector overlaps ──
    geom_text(data = label_df_top, aes(x = .data[[group_col]], 
                                       y = -(y_max * 0.22), 
                                       label = paste0("(", label, ")"), 
                                       group = .data[[fill_col]]), 
              inherit.aes = FALSE, fontface = "bold", 
              family = "sans",
              size = text_size * 1.05, 
              position = position_dodge(0.90), 
              vjust = 1.0, color = "black") +
    
    facet_wrap(vars(!!sym(facet_col)), scales = "fixed", drop = FALSE) + 
    labs(title = plot_title, x = NULL, 
         y = y_label, 
         fill = "Gender", 
         color = "Gender") +
    
    # Expanded bounding coordinates to avoid layout clipping inside SVG
    coord_cartesian(ylim = c(0, y_max * 1.35), clip = "off") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = n_breaks)) +
    scale_x_discrete(drop = FALSE) + 
    scale_fill_manual(values = c("Male" = "#56B4E9", "Female" = "#CC79A7"), drop = FALSE) +
    scale_color_manual(values = c("Male" = "#56B4E9", "Female" = "#CC79A7"), drop = FALSE) +
    theme_classic() +
    
    theme(
      text             = element_text(family = "sans"),
      
      legend.position  = "bottom", 
      legend.background = element_blank(),
      legend.box        = element_blank(),
      legend.key        = element_blank(),
      legend.text      = element_text(size = 16, face = "bold", family = "sans"), 
      legend.title     = element_text(size = 16, face = "bold", family = "sans"),
      
      plot.title       = element_text(hjust = 0.5, size = 22, face = "bold", family = "sans", margin = margin(b = 15)),
      
      strip.text       = element_text(size = 20, face = "bold", family = "sans"), 
      strip.background = element_rect(fill = "gray95", color = "black", linewidth = 0.40),
      
      # ── FIXED: Pushed top axis text margin out to avoid sample label collisions ──
      axis.text.x      = element_text(size = 18, face = "bold", family = "sans", 
                                      margin = margin(t = 4)), 
      
      # ── FIXED: Corrected Y-axis margin assignment from top (t) to right side (r) ──
      axis.text.y      = element_text(size = 18, face = "bold", family = "sans", 
                                      margin = margin(r = 2)),
      axis.ticks       = element_line(linewidth = 0.40),
      
      axis.title       = element_text(size = 20, face = "bold", family = "sans"), 
      axis.line        = element_line(linewidth = 0.40),
      
      panel.spacing    = unit(1.5, "lines"), 
      aspect.ratio     = 0.85,
      # Added clear padding around structural bounds
      plot.margin      = margin(t = 15, r = 25, b = 75, l = 25) 
    )
  
  return(p)
}

# ══════════════════════════════════════════════════════════════════════════════
# Master Loop Function: Runs calling sequence
# ══════════════════════════════════════════════════════════════════════════════

generate_6boxplot_panel_runs <- function(
    panel_ids, all_data, diabetic_data, gender_data,
    save_plots_flag = TRUE, export_dataset_name = "six_boxplot_profiles",
    export_base_dir = "output", export_subfolder = "treatment_gender_by_diabetes",
    export_width = 11.5, export_height = 7.5, export_dpi = 600, export_formats = c("svg", "png")
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
    
    panel_data <- all_data %>%
      left_join(diabetic_data, by = "bf_ar_m_nummer", relationship = "many-to-many") %>%
      left_join(gender_data, by = "bf_ar_m_nummer", relationship = "many-to-many") %>%
      dplyr::filter(panel_id == pid) %>%
      dplyr::filter(grepl("Leber_", treatment.x)) %>%
      dplyr::select(bf_ar_m_nummer, panel_id, labreads, diabetes, treatment.x, gender) %>%
      dplyr::rename("treatment" = "treatment.x") %>%
      drop_na() %>%
      mutate(
        treatment = factor(treatment, levels = c("Leber_Diclo", "Leber_Ibu", "Leber_Para")),
        gender    = factor(gender, levels = c("Male", "Female")),
        diabetes_status = factor(
          ifelse(diabetes %in% c(1, "1", "Diabetic", "Yes"), "Diabetic", "Non_Diabetic"), 
          levels = c("Non_Diabetic", "Diabetic")
        )
      ) %>%
      filter(labreads < 15000)
    
    if (nrow(panel_data) == 0) {
      message(paste("Skipping", toupper(pid), "- No data available."))
      next
    }
    
    plot_title <- paste(toupper(pid), "Levels across Treatments & Gender\n(Faceted by Diabetes Status)")
    y_label    <- if (pid %in% names(unit_map)) unit_map[[pid]] else toupper(pid)
    
    p <- plot_lab_6box_layout(
      data           = panel_data,
      group_var      = treatment,       
      fill_var       = gender,          
      facet_var      = diabetes_status, 
      y_var          = labreads,        
      patient_id_var = bf_ar_m_nummer,
      plot_title     = plot_title,
      y_label        = y_label
    )
    
    plots_list[[length(plots_list) + 1]] <- p
    plot_names <- c(plot_names, paste0("Plot_", toupper(pid), "_Treatment_Gender_6Box"))
  }
  
  if (save_plots_flag && length(plots_list) > 0) {
    save_plots(plots = plots_list, names = plot_names, dataset_name = export_dataset_name, base_dir = export_base_dir, subfolder = export_subfolder, width = export_width, height = export_height, dpi = export_dpi, formats = export_formats)
  }
  
  names(plots_list) <- plot_names
  return(plots_list)
}