library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

prepare_gender_diabetes_plots <- function(
    
  # datasets
  all_data,
  diabetic_data,
  gender_data,
  
  # vectors
  panel_vector,
  
  gender_vector = c("Male", "Female"),
  
  diabetes_vector = c(
    "Diabetic",
    "Non_Diabetic"
  ),
  
  # comparisons
  comparisons = list(
    c("Leber_Diclo", "Leber_Ibu"),
    c("Leber_Diclo", "Leber_Para"),
    c("Leber_Ibu", "Leber_Para")
  ),
  
  # save
  dataset_name = "diabetes_gender",
  main_folder = "diabetes_gender"
) {
  
  # =====================================
  # PANEL LABELS
  # =====================================
  
  panel_labels <- c(
    gpt = "ALT",
    got = "AST",
    ggt = "GGT",
    ap  = "AP",
    ck  = "CK",
    hb  = "Hb"
  )
  
  # =====================================
  # Y LABELS
  # =====================================
  
  y_labels <- c(
    gpt = "ALT (U/l)",
    got = "AST (U/l)",
    ggt = "GGT (U/l)",
    ap  = "AP (U/l)",
    ck  = "CK (U/l)",
    hb  = "Hb (g/dL)"
  )
  
  # =====================================
  # STORE ALL PLOTS
  # =====================================
  
  all_plots <- list()
  
  # =====================================
  # LOOP THROUGH PANELS
  # =====================================
  
  for (panel_name in panel_vector) {
    
    # -----------------------------------
    # PREPARE PANEL DATA
    # -----------------------------------
    
    panel_data <- prepare_panel_data(
      all_data = all_data,
      diabetic_data = diabetic_data,
      panel_id_value = panel_name
    )
    
    # -----------------------------------
    # JOIN GENDER
    # -----------------------------------
    
    panel_gender_data <- left_join(
      gender_data,
      panel_data,
      by = "bf_ar_m_nummer"
    ) %>%
      
      filter(
        !grepl(
          "_From Start|_From start|DBS",
          attribute
        )
      ) %>%
      
      drop_na() %>%
      
      filter(
        str_starts(
          treatment.x,
          "Leber_"
        )
      ) %>%
      
      mutate(
        treatment.x = as.factor(treatment.x),
        gender = as.factor(gender)
      )
    
    # ===================================
    # LOOP GENDER
    # ===================================
    
    for (gender_name in gender_vector) {
      
      # =================================
      # LOOP DIABETES
      # =================================
      
      for (diabetes_name in diabetes_vector) {
        
        # -------------------------------
        # FILTER
        # -------------------------------
        
        plot_data <- panel_gender_data %>%
          filter(
            gender == gender_name,
            diabetes == diabetes_name
          )
        
        # -------------------------------
        # TITLE
        # -------------------------------
        
        current_title <- paste0(
          "Comparison of ",
          panel_labels[[panel_name]],
          " ",
          diabetes_name,
          " ",
          tolower(gender_name),
          "s"
        )
        
        # -------------------------------
        # PLOT NAME
        # -------------------------------
        
        current_plot_name <- paste0(
          panel_name, "_",
          tolower(diabetes_name), "_",
          tolower(gender_name)
        )
        
        # -------------------------------
        # PLOT
        # -------------------------------
        
        p <- gender_plot_lab_comparison(
          data           = plot_data,
          group_var      = treatment.x,
          y_var          = labreads,
          patient_id_var = bf_ar_m_nummer,
          comparisons    = comparisons,
          plot_title     = current_title,
          y_label        = y_labels[[panel_name]]
        )
        
        # -------------------------------
        # SAVE
        # -------------------------------
        
        save_plots(
          plots = list(p),
          names = current_plot_name,
          dataset_name = dataset_name,
          
          subfolder = file.path(
            main_folder,
            panel_name,
            diabetes_name,
            gender_name
          )
        )
        
        # -------------------------------
        # STORE
        # -------------------------------
        
        all_plots[[current_plot_name]] <- p
      }
    }
  }
  
  return(all_plots)
}