######
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(scales)

# ========================================================
# 1. CORE LAYOUT FUNCTION
# ========================================================
plot_diabetic_flupirtin_fixed <- function(
    data, group_var, fill_var, y_var, patient_id_var,  
    plot_title = "Diabetic Cohort Analysis", y_label = "Lab Value",
    text_size = 5, n_breaks = 8
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
    mutate(label = paste0("N = ", unique_patients))
  
  df_bottom <- data %>%
    group_by(!!sym(group_col), !!sym(fill_col)) %>%
    summarise(total_reads = n(), .groups = "drop") %>%
    mutate(label = paste0("n = ", total_reads))
  
  y_max <- max(data[[y_col]], na.rm = TRUE)
  
  valid_comparisons <- data %>%
    group_by(!!sym(group_col)) %>%
    summarise(has_both = all(c("No_Flupirtin", "Flupirtin") %in% 
                               !!sym(fill_col)),
              .groups = "drop") %>%
    filter(has_both == TRUE) %>%
    pull(!!sym(group_col)) %>%
    as.character()
  
  p <- ggplot(data, aes(x = .data[[group_col]], 
                        y = .data[[y_col]], 
                        fill = .data[[fill_col]])) +
    
    geom_boxplot(aes(group = interaction(.data[[group_col]], 
                                         .data[[fill_col]])),
                 width = 0.50, outlier.shape = NA, 
                 alpha = 0.6, color = "black", linewidth = 0.35,
                 position = position_dodge(0.60)) +
    
    geom_jitter(aes(color = .data[[fill_col]], group = interaction(.data[[group_col]], .data[[fill_col]])), 
                alpha = 0.35, size = 1.2, 
                position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.60)) +
    
    stat_summary(aes(group = interaction(.data[[group_col]], .data[[fill_col]])),
                 fun = median, geom = "point", shape = 23, 
                 size = 2, fill = "brown", 
                 position = position_dodge(0.60)) +
    
    geom_text(data = df_bottom, aes(x = .data[[group_col]], 
                                    y = y_max * 1.05, 
                                    label = label, 
                                    group = .data[[fill_col]]), 
              inherit.aes = FALSE, fontface = "bold", family = "sans",
              size = text_size * 1.05, position = position_dodge(0.75), vjust = 0) +
    
    geom_text(data = df_top, aes(x = .data[[group_col]], 
                                 y = -(y_max * 0.25), 
                                 label = paste0("(", label, ")"), 
                                 group = .data[[fill_col]]), 
              inherit.aes = FALSE, fontface = "bold", family = "sans",
              size = text_size * 1.05, 
              position = position_dodge(0.99), vjust = 1.0, color = "black") +
    
    labs(title = plot_title, x = NULL, y = y_label, fill = "Co-Medication", color = "Co-Medication") +
    
    coord_cartesian(ylim = c(0, y_max * 1.30), clip = "off") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = n_breaks)) +
    scale_x_discrete(drop = FALSE) + 
    
    scale_fill_manual(values = c("No_Flupirtin" = "#E69F00", "Flupirtin" = "#009E73"), drop = FALSE) +
    scale_color_manual(values = c("No_Flupirtin" = "#E69F00", "Flupirtin" = "#009E73"), drop = FALSE) +
    theme_classic() +
    
    theme(
      text              = element_text(family = "sans"),
      legend.position   = "bottom", 
      legend.background = element_blank(),
      legend.box        = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(size = 14, face = "bold"), 
      legend.title      = element_blank(),
      plot.title        = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      axis.text.x       = element_text(size = 14, face = "bold", margin = margin(t = 3)), 
      axis.text.y       = element_text(size = 14, face = "bold", margin = margin(r = 6)),
      axis.title        = element_text(size = 15, face = "bold"), 
      axis.line         = element_line(linewidth = 0.40),
      plot.margin       = margin(t = 15, r = 15, b = 20, l = 15) 
    )
  
  if (length(valid_comparisons) > 0) {
    p <- p + stat_compare_means(
      data    = filter(data, !!sym(group_col) %in% valid_comparisons),
      aes(group = .data[[fill_col]]), 
      method  = "wilcox.test", label = "p.format", 
      label.y = y_max * 1.20, bracket.size = 0.40, 
      tip.length = 0.01, family = "sans", size = text_size * 0.9
    )
  }
  
  return(p)
}

# ========================================================
# 2. THE MASTER PIPELINE (WITH INTEGRATED FILTER ADJUSTMENTS)
# ========================================================
generate_diabetic_flupirtin_compact_runs <- function(
    panel_ids, 
    all_data, 
    flupirtin_data,
    save_plots_flag = TRUE, 
    export_dataset_name = "Diabetic_Flupirtin_Compact",
    export_base_dir = "output/figure/diabetes/plots_all_labvalues", 
    export_subfolder = "Diabetic_Flupirtin_DIP",
    export_width = 6, 
    export_height = 4, 
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
    
    # Adjusted filtering logic block embedded seamlessly into the loop execution
    panel_data <- all_data %>%
      dplyr::filter(panel_id == pid) %>%
      left_join(flupirtin_data, by = join_by("bf_ar_m_nummer","treatment"),
                relationship = "many-to-many") %>% 
      dplyr::filter(grepl("Leber_", treatment)) %>% 
      
      # Drops records missing key values including 'days' matching your requirements
      drop_na(labreads, days, treatment, flupirtin) %>% 
      
      mutate(
        treatment = factor(gsub("Leber_", "", as.character(treatment)), 
                           levels = c("Diclo", "Ibu", "Para")),
        flupirtin = factor(ifelse(flupirtin %in% c("Flupirtin", "Yes", "flupritin", "flupirtin"), 
                                  "Flupirtin", "No_Flupirtin"), 
                           levels = c("No_Flupirtin", "Flupirtin"))
      ) %>%
      dplyr::filter(treatment %in% c("Diclo", "Ibu")) %>% 
      dplyr::filter(labreads < 15000) %>%
      dplyr::select(bf_ar_m_nummer, panel_id, labreads, treatment, flupirtin)
    
    if (nrow(panel_data) == 0) next
    
    plot_title <- paste("Liver_Diclo:", toupper(pid), "\n(Flupirtin Medication Effect)")
    y_label    <- if (pid %in% names(unit_map)) unit_map[[pid]] else toupper(pid)
    
    p <- plot_diabetic_flupirtin_fixed(
      data           = panel_data,
      group_var      = treatment,       
      fill_var       = flupirtin,          
      y_var          = labreads,        
      patient_id_var = bf_ar_m_nummer,
      plot_title     = plot_title,
      y_label        = y_label
    )
    
    plots_list[[length(plots_list) + 1]] <- p
    plot_names <- c(plot_names, paste0("Plot_", toupper(pid), "Flupirtin"))
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
