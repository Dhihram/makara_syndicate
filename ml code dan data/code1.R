setwd("C:/Users/dhihr/OneDrive/riset bu yeni")
library(haven)
dataset = read_sav('Klaim Lansia fix [clean].sav')
library(tidyverse)
library(labelled)
library(dplyr, warn.conflicts = FALSE)
dataset <- dataset |> select(-survival, -stat2, -stat4, -stat3, -stat4, -stat5, -PSTV02, -PSTV15)
#write.csv(dataset, "klaim_lansia.csv", row.names = FALSE)
#write_sav(dataset, 'Klaim Lansia fix [clean].sav')
dataset$no <- as.numeric(row.names(dataset)) 
dataset <- dataset %>%
  select(no,PSTV01,  everything())
dataset <- to_factor(dataset)
#dataset <- dataset %>% filter(demografi1 >= 65)

# variabel : cost
#utama: diagnosis penyakit, usia, sex, status sosek, disabilitas, dll

#machine learning (random forest atau KNN)
#sensitivity analisis (regresi linear)
#akurasi

hist(dataset$cost)

table(dataset$rs1)
table(dataset$rs2)
table(dataset$akses2)

hist(dataset$cost, breaks = 700)

#ICD9CM Grouping

dat2 <- dataset %>% select(no, PSTV01, stat5)

#separate stat5
# Use separate_rows to split 'b' into multiple rows
#dat2 <- dat2 %>%
#  separate_rows(stat5, sep = ";")
dataset <- dataset %>%
  mutate(stat1 = as.character(stat1)) %>% # Ensure stat1 is a character column
  separate(stat1, into = c("letters", "numbers"), sep = "(?<=\\D)(?=\\d)", remove = FALSE)

dat2$icd9 = as.character(sub(" -.*", "", dat2$stat5))
dat2$icd9 <- paste0(substr(as.character(dat2$icd9), 1, 2), '.', substr(as.character(dat2$icd9), 3, 4))
dat2 <- dat2 %>% filter(!str_detect(icd9, "^NA\\.NA$"))
dat2 <- dat2 %>%
  separate(icd9, into = c("icd9_code", "icd9_code2"), sep = "\\.") %>%
  mutate(icd9_code = as.character(icd9_code),
         icd9_code2 = as.numeric(icd9_code2))
dat2$chap_00 <- ifelse(dat2$icd9_code == '00', 1, NA)
dat2$icd9_code <- as.numeric(dat2$icd9_code)
dat2 <- dat2 %>% mutate(
  chap_1 = case_when(icd9_code %in% 1:5 ~ 1),
  chap_2 = case_when(icd9_code %in% 6:7 ~ 1),
  chap_3 = case_when(icd9_code %in% 8:16 ~ 1),
  chap_3A = case_when(icd9_code %in% 17:17 ~ 1),
  chap_4 = case_when(icd9_code %in% 18:20 ~ 1),
  chap_5 = case_when(icd9_code %in% 21:29 ~ 1),
  chap_6 = case_when(icd9_code %in% 30:34 ~ 1),
  chap_7 = case_when(icd9_code %in% 35:39 ~ 1),
  chap_8 = case_when(icd9_code %in% 40:41 ~ 1),
  chap_9 = case_when(icd9_code %in% 42:54 ~ 1),
  chap_10 = case_when(icd9_code %in% 55:59 ~ 1),
  chap_11 = case_when(icd9_code %in% 60:64 ~ 1),
  chap_12 = case_when(icd9_code %in% 65:71 ~ 1),
  chap_13 = case_when(icd9_code %in% 72:75 ~ 1),
  chap_14 = case_when(icd9_code %in% 76:84 ~ 1),
  chap_15 = case_when(icd9_code %in% 85:86 ~ 1),
  chap_16 = case_when(icd9_code %in% 87:99 ~ 1))
dat2 <- dat2 %>% select(-stat5, -icd9_code2, -icd9_code)
dat2 <- dat2 %>%
  group_by(no, PSTV01) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
