library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(scales)

# ==========================================
# 1. DOWN-SCALED CANVA LAYOUT FUNCTION
# ==========================================
plot_diabetic_flupirtin_compact <- function(
    data, 
    group_var, 
    fill_var, 
    y_var, 
    patient_id_var,  
    plot_title = "Diabetic Cohort", 
    y_label = "Lab Value",
    text_size = 5, 
    n_breaks = 8  # Reduced label sizes and grid-breaks for small canvas
) {
  
  group_col <- as.character(substitute(group_var))
  fill_col  <- as.character(substitute(fill_var))
  y_col     <- as.character(substitute(y_var))
  id_col    <- as.character(substitute(patient_id_var))
  
  data[[group_col]] <- factor(data[[group_col]], 
                              levels = c("Diclo", "Ibu", "Para"))
  data[[fill_col]]  <- factor(data[[fill_col]], 
                              levels = c("No_Flupirtin", "Flupirtin"))
  
  df_top <- data %>%
    group_by(!!sym(group_col), !!sym(fill_col)) %>%
    summarise(unique_patients = n_distinct(!!sym(id_col)),
              .groups = "drop") %>%
    mutate(label = paste0("N=", unique_patients)) # Removed spaces to save horizontal space
  
  df_bottom <- data %>%
    group_by(!!sym(group_col), !!sym(fill_col)) %>%
    summarise(total_reads = n(), .groups = "drop") %>%
    mutate(label = paste0("n=", total_reads))
  
  y_max <- max(data[[y_col]], na.rm = TRUE)
  
  valid_comparisons <- data %>%
    group_by(!!sym(group_col)) %>%
    summarise(has_both = all(c("No_Flupirtin", "Flupirtin") %in% 
                               !!sym(fill_col)), .groups = "drop") %>%
    filter(has_both == TRUE) %>%
    pull(!!sym(group_col)) %>%
    as.character()
  
  p <- ggplot(data, aes(x = .data[[group_col]], 
                        y = .data[[y_col]], 
                        fill = .data[[fill_col]])) +
    
    # Thinned out geometries to match the 5x4 layout bounds
    geom_boxplot(aes(group = interaction(.data[[group_col]], 
                                         .data[[fill_col]])),
                 width = 0.55, outlier.shape = NA, 
                 alpha = 0.6, color = "black", linewidth = 0.30,
                 position = position_dodge(0.65)) +
    
    geom_jitter(aes(color = .data[[fill_col]], 
                    group = interaction(.data[[group_col]], 
                                        .data[[fill_col]])), 
                alpha = 0.30, size = 0.8, # Smaller dots prevent crowded patterns
                position = position_jitterdodge(jitter.width = 0.10, 
                                                dodge.width = 0.65)) +
    
    stat_summary(aes(group = interaction(.data[[group_col]], 
                                         .data[[fill_col]])),
                 fun = median, 
                 geom = "point", 
                 shape = 23, 
                 size = 1.2, 
                 fill = "brown", 
                 position = position_dodge(0.65)) +
    
    # Shifted 'n' numbers down closer to box heights
    geom_text(data = df_bottom, aes(x = .data[[group_col]], 
                                    y = y_max * 1.03, 
                                    label = label, 
                                    group = .data[[fill_col]]), 
              inherit.aes = FALSE, 
              fontface = "bold", 
              family = "sans",
              size = text_size * 1.2, 
              position = position_dodge(0.65), 
              vjust = 0) +
    
    # Tightened 'N' layout adjustments beneath the horizontal baseline axis line
    geom_text(data = df_top, aes(x = .data[[group_col]], 
                                 y = -(y_max * 0.25), 
                                 label = paste0("(", label, ")"), 
                                 group = .data[[fill_col]]), 
              inherit.aes = FALSE, 
              fontface = "bold", 
              family = "sans",
              size = text_size * 1.2, 
              position = position_dodge(0.85), 
              vjust = 1.0, 
              color = "black") +
    
    labs(title = plot_title, 
         x = NULL, 
         y = y_label, 
         fill = "Co-Medication", 
         color = "Co-Medication") +
    coord_cartesian(ylim = c(0, y_max * 1.25), clip = "off") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = n_breaks)) +
    scale_x_discrete(drop = FALSE) + 
    scale_fill_manual(
      values = c("No_Flupirtin" = "#E69F00", 
                 "Flupirtin" = "#009E73"), 
      drop = FALSE) +
    scale_color_manual(
      values = c("No_Flupirtin" = "#E69F00", 
                 "Flupirtin" = "#009E73"), 
      drop = FALSE) +
    theme_prism() +
    # ── FIXED: Scaled font indices down so text fits inside the 5x4 in window ──
    theme(
      text              = element_text(family = "sans"),
      legend.position   = "bottom", 
      legend.background = element_blank(),
      legend.box        = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(size = 16, face = "bold"), 
      legend.title      = element_blank(),
      legend.margin     = margin(t = 5), # Pull legend up slightly
      plot.title        = element_text(hjust = 0.5, 
                                       size = 25, 
                                       face = "bold", 
                                       margin = margin(b = 10)),
      axis.text.x       = element_text(size = 22, 
                                       face = "bold", 
                                       margin = margin(b = 3)), 
      axis.text.y       = element_text(size = 22, 
                                       face = "bold",
                                       margin = margin(r = 4)),
      axis.title        = element_text(size = 24, 
                                       face = "bold"), 
      #axis.line         = element_line(linewidth = 0.30),
      axis.ticks        = element_line(linewidth = 0.30),
      plot.margin       = margin(t = 10, r = 15,
                                 b = 25, l = 15))
  
  if (length(valid_comparisons) > 0) {
    p <- p + stat_compare_means(
      data    = filter(data, !!sym(group_col) %in% valid_comparisons),
      aes(group = .data[[fill_col]]), 
      method  = "wilcox.test", label = "p.format", 
      label.y = y_max * 1.14, bracket.size = 0.30, 
      tip.length = 0.01, family = "sans", size = text_size * 0.95
    )
  }
  
  return(p)
}

# ==========================================
# 2. MASTER LOOP FUNCTION (COMPACT ARGS)
# ==========================================
generate_diabetic_flupirtin_compact_runs <- function(
    panel_ids, 
    all_data, 
    diabetic_data, 
    flupirtin_data,
    save_plots_flag = TRUE, 
    export_dataset_name = "Diabetic_Flupirtin_Compact",
    export_base_dir = "output/figure/diabetes/plots_all_labvalues/Flupirtin", 
    export_subfolder = "Diabetic_Flupirtin_6x4",
    # ── FIXED: Configured target specifications here ──
    export_width = 8, 
    export_height = 5, 
    export_dpi = 600, 
    export_formats = c("svg", "png")
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
      left_join(flupirtin_data, by = "bf_ar_m_nummer", relationship = "many-to-many") %>%
      
      dplyr::filter(panel_id == pid) %>%
      dplyr::filter(grepl("Leber_", treatment.x)) %>%
      dplyr::filter(diabetes %in% c("Diabetic", "Yes", 1, "1")) %>%
      
      dplyr::select(bf_ar_m_nummer, panel_id, labreads, treatment.x, flupirtin) %>%
      dplyr::rename("treatment" = "treatment.x") %>%
      drop_na(treatment, flupirtin, labreads) %>%
      
      mutate(
        treatment = factor(gsub("Leber_", "", as.character(treatment)), levels = c("Diclo", "Ibu", "Para")),
        flupirtin = factor(ifelse(flupirtin %in% c("Flupirtin", "Yes", "flupritin", "flupirtin"), "Flupirtin", "No_Flupirtin"),
                           levels = c("No_Flupirtin", "Flupirtin"))
      ) %>%
      filter(labreads < 15000)
    
    if (nrow(panel_data) == 0) next
    
    plot_title <- paste("Diabetic:", toupper(pid),"(Flupirtin Medication Effect)")
    y_label    <- if (pid %in% names(unit_map)) unit_map[[pid]] else toupper(pid)
    
    # Pipe cleaned entries to our new compact engine
    p <- plot_diabetic_flupirtin_compact(
      data           = panel_data,
      group_var      = treatment,       
      fill_var       = flupirtin,          
      y_var          = labreads,        
      patient_id_var = bf_ar_m_nummer,
      plot_title     = plot_title,
      y_label        = y_label
    )
    
    plots_list[[length(plots_list) + 1]] <- p
    plot_names <- c(plot_names, paste0("Plot_", toupper(pid), "_Diabetic_Flupirtin_5x4"))
  }
  
  if (save_plots_flag && length(plots_list) > 0) {
    save_plots(plots = plots_list, 
               names = plot_names, 
               dataset_name = export_dataset_name, 
               base_dir = export_base_dir, 
               subfolder = export_subfolder, 
               width = export_width, 
               height = export_height, 
               dpi = export_dpi, 
               formats = export_formats)
  }
  
  names(plots_list) <- plot_names
  return(plots_list)
}