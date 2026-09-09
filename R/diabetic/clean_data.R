process_sheet <- function(file, sheet_name) {
  
  read_excel(file, skip = 2, sheet = sheet_name) %>% 
    #slice(-1) %>% 
    janitor::clean_names() %>% 
    pivot_longer(
      cols = -c(1:2),
      names_to = "attribute",
      values_to = "labreads"
    )%>% 
    mutate(
      panel = sheet_name,
      
      panel_id = str_extract(attribute, "^[^_]+"),
      
      day_raw = str_extract(
        attribute,
        "d\\d+w\\d+m\\d+|d\\d+w\\d+|d\\d+m\\d+|d\\d+|day\\d+"
      ),
      
      days = case_when(
        
        # d5w3m1
        str_detect(day_raw, "^d\\d+w\\d+m\\d+$") ~
          as.numeric(str_extract(day_raw, "(?<=d)\\d+")) +
          7 * as.numeric(str_extract(day_raw, "(?<=w)\\d+")) +
          30 * as.numeric(str_extract(day_raw, "(?<=m)\\d+")),
        
        # d2w1
        str_detect(day_raw, "^d\\d+w\\d+$") ~
          as.numeric(str_extract(day_raw, "(?<=d)\\d+")) +
          7 * as.numeric(str_extract(day_raw, "(?<=w)\\d+")),
        
        # d5m1
        str_detect(day_raw, "^d\\d+m\\d+$") ~
          as.numeric(str_extract(day_raw, "(?<=d)\\d+")) +
          30 * as.numeric(str_extract(day_raw, "(?<=m)\\d+")),
        
        # day14
        str_detect(day_raw, "^day\\d+$") ~
          as.numeric(str_extract(day_raw, "\\d+")),
        
        # d14
        str_detect(day_raw, "^d\\d+$") ~
          as.numeric(str_extract(day_raw, "\\d+")),
        
        TRUE ~ NA_real_
      )
    ) %>% 
    relocate(panel, 
      panel_id, .before = 1)
}
##################################
## function for reading excel data file##

read_data <- function(file, sheet = NULL, range = NULL, skip = 0) {
  readxl::read_excel(
    path = file,
    sheet = sheet,
    range = range,
    skip = skip
  )
}
