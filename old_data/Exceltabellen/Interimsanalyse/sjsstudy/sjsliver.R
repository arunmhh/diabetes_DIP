library(readxl)
library(tidyr)
library(dplyr)

# df <- read_excel("Raw Data_From_Leenart/Rechenversion Laborparameter.xlsx", sheet = 1) %>%
#   pivot_longer(cols = starts_with("dic"), names_to = "Attribute", values_to = "Value") %>%
#   filter(Value != 0) %>%
#   select(-c(starts_with("dic"))) %>%
#   arrange(`BfArM Nummer`, Attribute)
# 
# dfibu <- read_excel("Raw Data_From_Leenart/Rechenversion Laborparameter.xlsx", sheet = 3) %>%
#   pivot_longer(cols = starts_with("Ibu"), names_to = "Attribute", values_to = "Value") %>%
#   filter(Value != 0) %>%
#   select(-c(starts_with("dic"))) %>%
#   arrange(`BfArM Nummer`, Attribute)
# 
# unique_df <- unique(df[, c("BfArM Nummer", "Attribute", "Value")])
# df_value <- df$Value
# t.test(df_value)


rawdata <- read_excel("data_19June/Übersicht Laborwerte (Rohdaten für Interimsanalyse 03.05.2023).xlsx",
                      sheet = 1,skip = 1)
rawdf1 <- as.data.frame(rawdata)
# Remove leading and trailing spaces from column names
names(rawdf1) <- trimws(names(rawdf1))
rawdf1 <- as.data.frame(rawdf1)




rawdf1 %>%
  select(1:2,starts_with("CK"))->raw_ck


raw_ck%>% 
  pivot_longer(cols = 3:43,
               names_to = "Attribute",
               values_to = "lab_reads")->data_ck
 

