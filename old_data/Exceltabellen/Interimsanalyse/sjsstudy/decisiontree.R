library(tidyverse)
####Reading the SJS labwerte data in R from excel
library(readxl)
lab_SJSDiclo <- read_excel("Rechenversion Laborparameter.xlsx", sheet = 1)
View(lab_SJSDiclo)
lab_SJSIbu<- read_excel("Rechenversion Laborparameter.xlsx", sheet = 2)
lab_SJSPara<- read_excel("Rechenversion Laborparameter.xlsx", sheet = 3)
lab_HautDiclo <- read_excel("Rechenversion Laborparameter.xlsx", sheet = 4)
lab_HautIbu<- read_excel("Rechenversion Laborparameter.xlsx", sheet = 5)
lab_HautPara<- read_excel("Rechenversion Laborparameter.xlsx", sheet = 6)
lab_Leberdiclo<- read_excel("Rechenversion Laborparameter.xlsx", sheet = 14)
lab_LeberIbu<- read_excel("Rechenversion Laborparameter.xlsx", sheet = 15)
lab_Leberpara<- read_excel("Rechenversion Laborparameter.xlsx", sheet = 18)

##  Data read from Rechenversion Medikamente excel file.
med_SJSdiclo<- read_excel("Rechenversion Medikamente.xlsx", sheet = 1)
#SJSIbu[is.na(SJSIbu)] <- 0
View(med_SJSdiclo)
##
med_SJSIbu<- read_excel("Rechenversion Medikamente.xlsx", sheet = 2)
med_SJSPara<- read_excel("Rechenversion Medikamente.xlsx", sheet = 3)
med_Hautdiclo<- read_excel("Rechenversion Medikamente.xlsx", sheet = 4)
med_HautIbu<- read_excel("Rechenversion Medikamente.xlsx", sheet = 5)
med_Hautpara<- read_excel("Rechenversion Medikamente.xlsx", sheet = 6)
med_Leberdiclo<- read_excel("Rechenversion Medikamente.xlsx", sheet = 7)
med_LeberIbu<- read_excel("Rechenversion Medikamente.xlsx", sheet = 8)
med_LeberPara<- read_excel("Rechenversion Medikamente.xlsx", sheet = 9)


#### Rechenversion_anamnese_vorerkrankungen excke file

Anase_SJSdiclo<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 1)
#SJSIbu[is.na(SJSIbu)] <- 0
View(Anase_SJSdiclo)
##
Anase_SJSIbu<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 2)
Anase_SJSPara<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 3)
Anase_Hautdiclo<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 4)
Anase_HautIbu<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 5)
Anase_Hautpara<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 6)
Anase_Leberdiclo<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 7)
Anase_LeberIbu<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 8)
Anase_LeberPara<- read_excel("Rechenversion_anamnese_vorerkrankungen.xlsx", sheet = 9)


#### Rechenversion_symptome_biopsie_histo excle file.

SBH_SJSdiclo<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 1)
#SJSIbu[is.na(SJSIbu)] <- 0
View(SBH_SJSdiclo)
##
SBH_SJSIbu<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 2)
SBH_SJSPara<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 3)
SBH_Hautdiclo<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 4)
SBH_HautIbu<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 5)
SBH_Hautpara<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 6)
SBH_Leberdiclo<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 7)
SBH_LeberIbu<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 8)
SBH_LeberPara<- read_excel("Rechenversion_symptome_biopsie_histo.xlsx", sheet = 9)

##########################################################################################

# filtering data base on observations.
library(dplyr)

lab_SJSDiclo %>% select(`BfArM Nummer`,dic_4900261,dic_7072184) %>%  
arrange(desc(dic_4900261)) %>% head(30)

























