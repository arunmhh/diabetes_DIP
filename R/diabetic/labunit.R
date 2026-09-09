get_lab_unit_map <- function() {
  
  c(
    
    # ─────────────────────────────────────────────────────────────
    # Liver & Enzymes
    # ─────────────────────────────────────────────────────────────
    "got"                = "AST/GOT (U/L)",
    "gpt"                = "ALT/GPT (U/L)",
    "ggt"                = "GGT (U/L)",
    "ap"                 = "Alkaline Phosphatase (U/L)",
    "gldh"               = "GLDH (U/L)",
    "ldh"                = "LDH (U/L)",
    "cholinesterase"     = "Cholinesterase (U/L)",
    "bilirubin"          = "Total Bilirubin (mg/dL)",
    "total_bilirubin"    = "Total Bilirubin (mg/dL)",
    "direktes"           = "Direct Bilirubin (mg/dL)",
    "direct_bilirubin"   = "Direct Bilirubin (mg/dL)",
    "indirektes"         = "Indirect Bilirubin (mg/dL)",
    "indirect_bilirubin" = "Indirect Bilirubin (mg/dL)",
    "ammoniak"           = "Ammonia (µg/dL)",
    
    # ─────────────────────────────────────────────────────────────
    # Cardiac & Muscle
    # ─────────────────────────────────────────────────────────────
    "ck"             = "CK (U/L)",
    "keratinkinase"  = "Creatine Kinase (U/L)",
    "ck_mb"          = "CK-MB (U/L)",
    "troponin"       = "Troponin (ng/L)",
    "serummyoglobin" = "Myoglobin (µg/L)",
    
    # ─────────────────────────────────────────────────────────────
    # Kidney & Metabolic
    # ─────────────────────────────────────────────────────────────
    "kreatinin"   = "Creatinine (mg/dL)",
    "harnstoff"   = "Urea (mg/dL)",
    "harnsaure"   = "Uric Acid (mg/dL)",
    "gfr"         = "eGFR (mL/min/1.73m²)",
    "grf"         = "eGFR (mL/min/1.73m²)",
    "glomarulare" = "eGFR (mL/min/1.73m²)",
    "glucose"     = "Glucose (mg/dL)",
    "lactat"      = "Lactate (mmol/L)",
    
    # ─────────────────────────────────────────────────────────────
    # Lipids & Proteins
    # ─────────────────────────────────────────────────────────────
    "cholesterin"   = "Cholesterol (mg/dL)",
    "triglyceride"  = "Triglycerides (mg/dL)",
    "hdl"           = "HDL Cholesterol (mg/dL)",
    "ldl"           = "LDL Cholesterol (mg/dL)",
    "gesamteiweiss" = "Total Protein (g/dL)",
    "albumin"       = "Albumin (g/dL)",
    "alpha"         = "Alpha-Globulin (%)",
    "beta"          = "Beta-Globulin (%)",
    "gamma"         = "Gamma-Globulin (%)",
    
    # ─────────────────────────────────────────────────────────────
    # Pancreas
    # ─────────────────────────────────────────────────────────────
    "amylase" = "Amylase (U/L)",
    "lipase"  = "Lipase (U/L)",
    
    # ─────────────────────────────────────────────────────────────
    # Inflammation & Infection
    # ─────────────────────────────────────────────────────────────
    "crp"         = "CRP (mg/L)",
    "bsg"         = "ESR (mm/h)",
    "leukozyten"  = "Leukocytes (G/L)",
    "fieber"      = "Temperature (°C)",
    
    # ─────────────────────────────────────────────────────────────
    # Coagulation
    # ─────────────────────────────────────────────────────────────
    "quick"          = "Quick (%)",
    "inr"            = "INR",
    "ptt"            = "PTT (s)",
    "prothrombin"    = "Prothrombin Time (s)",
    "tz"             = "Thrombin Time (s)",
    "antithrombin"   = "Antithrombin III (%)",
    "antithrombin_3" = "Antithrombin III (%)",
    "fibrinogen"     = "Fibrinogen (mg/dL)",
    "d"              = "D-Dimer (mg/L)",
    
    # ─────────────────────────────────────────────────────────────
    # Full Blood Count
    # ─────────────────────────────────────────────────────────────
    "erythrozyten" = "Erythrocytes (T/L)",
    "hb"           = "Hemoglobin (g/dL)",
    "hamatokrit"   = "Hematocrit (%)",
    "mcv"          = "MCV (fL)",
    "mch"          = "MCH (pg)",
    "mchc"         = "MCHC (g/dL)",
    "thrombozyten" = "Thrombocytes (G/L)",
    "trhombozyten" = "Thrombocytes (G/L)",
    
    # ─────────────────────────────────────────────────────────────
    # Differential Blood Count
    # ─────────────────────────────────────────────────────────────
    "lymphozyten"    = "Lymphocytes (%)",
    "monozyten"      = "Monocytes (%)",
    "eosinophile"    = "Eosinophils (%)",
    "basophile"      = "Basophils (%)",
    "neutrophile"    = "Neutrophils (%)",
    "stabkernige"    = "Band Neutrophils (%)",
    "segmentkernige" = "Segmented Neutrophils (%)",
    
    # ─────────────────────────────────────────────────────────────
    # Electrolytes & Minerals
    # ─────────────────────────────────────────────────────────────
    "natrium"   = "Sodium (mmol/L)",
    "kalium"    = "Potassium (mmol/L)",
    "kalzium"   = "Calcium (mmol/L)",
    "magnesium" = "Magnesium (mmol/L)",
    "chlorid"   = "Chloride (mmol/L)",
    "phosphat"  = "Phosphate (mg/dL)",
    
    # ─────────────────────────────────────────────────────────────
    # Iron Metabolism
    # ─────────────────────────────────────────────────────────────
    "serum_eisen" = "Serum Iron (µg/dL)",
    "transferrin" = "Transferrin (mg/dL)",
    "ferritin"    = "Ferritin (µg/L)",
    "haptoglobin" = "Haptoglobin (mg/dL)",
    
    # ─────────────────────────────────────────────────────────────
    # Generic
    # ─────────────────────────────────────────────────────────────
    "total"    = "Total Value",
    "totales"  = "Total Value",
    "blut"     = "Blood Value",
    "serum"    = "Serum Value"
  )
}