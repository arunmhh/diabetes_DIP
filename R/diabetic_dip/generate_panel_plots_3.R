# ==============================================================================
# Main pipeline:
#   - prepares panel data
#   - creates plots
#   - optionally saves them
# ==============================================================================

# ══════════════════════════════════════════════════════════════════════════════
# 4. Master Loop Function: TREATMENT COMPARISON (LIVER COHORT)
# ══════════════════════════════════════════════════════════════════════════════

generate_and_save_panel_plots_treatment_liver <- function(
    panel_ids,
    all_data,
    gender_data1,
    diabetic_data       = NULL, # Set as optional if needed for other covariates
    
    # Optional parameters for saving
    save_plots_flag     = TRUE,
    export_dataset_name = "lab_plots",
    export_base_dir     = "output",
    export_subfolder    = "treatment_liver_comparisons",
    export_width        = 7.5,
    export_height       = 6.0,
    export_dpi          = 600,
    export_formats      = c("svg", "png")
) {
  
  # Dictionary mapping raw panel IDs to their explicit display names and units
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
    
    # ── Data Processing Pipeline ──
    panel_data <- all_data %>%
      left_join(gender_data1, by = c("bf_ar_m_nummer", "treatment"), relationship = "many-to-many") %>%
      dplyr::filter(panel_id == pid) %>%
      
      # Filter for Liver treatments, specifically Diclo, Ibu, and Para
      dplyr::filter(grepl("Leber_", treatment)) %>%
      dplyr::filter(grepl("(?i)diclo|ibu|para", treatment)) %>% # Matches diclo, ibu, or para
      
      dplyr::filter(!str_detect(attribute, "_krankheitsbeginn|DBS")) %>%
      dplyr::filter(!is.na(treatment)) %>% 
      mutate(treatment = factor(treatment)) %>%
      dplyr::filter(labreads < 15000) %>%
      drop_na(labreads, treatment)
    
    # Optional join to keep diabetes info if needed, uncomment if required:
    # if (!is.null(diabetic_data)) {
    #   panel_data <- panel_data %>% left_join(diabetic_data, by = "bf_ar_m_nummer", relationship = "many-to-many")
    # }
    
    # Safety Failsafes
    if (nrow(panel_data) == 0) {
      message(paste("Skipping", toupper(pid), "- No data available after filtering."))
      next
    }
    
    num_groups <- length(unique(panel_data$treatment))
    if (num_groups < 2) {
      message(paste("Skipping", toupper(pid), "- Less than 2 treatment groups present."))
      next
    }
    
    # Dynamic Titles & Units
    plot_title <- paste(toupper(pid), "Levels by Treatment\n(Liver Cohort)")
    y_label    <- if (pid %in% names(unit_map)) unit_map[[pid]] else toupper(pid)
    
    # Render Single Plot Layer
    p <- plot_lab_comparison(
      data           = panel_data,
      group_var      = treatment,                       
      y_var          = labreads,
      patient_id_var = bf_ar_m_nummer,
      comparisons    = make_comparisons(panel_data, "treatment"), 
      plot_title     = plot_title,
      x_label        = "Treatment Group",               
      y_label        = y_label
    )
    
    plots_list[[length(plots_list) + 1]] <- p
    plot_names <- c(plot_names, paste0("Plot_", toupper(pid), "_Treatment_LiverCohort"))
    
    message(paste("Successfully generated plot for:", toupper(pid)))
  }
  
  # ── File Export Routine ──
  if (save_plots_flag && length(plots_list) > 0) {
    message("\nSaving generated plots...")
    # Make sure you have your save_plots helper defined elsewhere in your script!
    tryCatch({
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
    }, error = function(e) {
      message("Note: 'save_plots' function not found or failed. Plots generated but not saved to disk.")
    })
  }
  
  names(plots_list) <- plot_names
  return(plots_list)
}