library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

process_comedication_sheet <- function(file, sheet, skip_headers = 3,range = NULL) {
  
  # Read header rows
  headers <- read_excel(
    path = file,
    sheet = sheet,
    n_max = skip_headers,
    range = range,
    col_names = FALSE
  )
  
  # Create combined column names
  combined_names <- paste(headers[1, ],
                          headers[2, ],
                          headers[3, ],
                          sep = "_") %>%
    str_replace_all("_NA", "")
  
  # Read actual data
  raw_data <- read_excel(
    path = file,
    sheet = sheet,
    skip = skip_headers,
    col_names = combined_names
  )
  
  # Clean and reshape
  cleaned_data <- raw_data %>%
    pivot_longer(
      cols = 3:ncol(raw_data),
      names_to = c("Medication", "kathegorie", "drug_group"),
      names_pattern = "([^_]+)_([^_]+)_(.*)",
      values_to = "labreads"
    ) %>%
    rename(
      bfarm_nummer = `BfArM Nummer_BfArM Nummer_Edited_BfArM Nummer2`,
      treatment = `Kathegorie_Kathegorie_Treatment`
    ) %>%
    drop_na(labreads)
  
  return(cleaned_data)
}
























