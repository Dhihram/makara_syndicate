setwd("C:/Users/dhihr/OneDrive/riset bu yeni")
library(haven)
#dataset = read_sav('Klaim Lansia fix [clean].sav')
dataset = read_dta('Klaim Lansia fix [clean usia 60+].dta')
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
dataset <- dataset %>% filter(stat1 != "")


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

dat2 <- dataset %>% select(PSTV01, FKL02, stat1)

#separate stat5
# Use separate_rows to split 'b' into multiple rows
#dat2 <- dat2 %>%
#  separate_rows(stat5, sep = ";")
dat2 <- dat2 %>%
  mutate(stat1 = as.character(stat1)) %>% # Ensure stat1 is a character column
  separate(stat1, into = c("icd10_let", "Icd10_num"), sep = "(?<=\\D)(?=\\d)", remove = FALSE)
dat2 <- dat2 %>%
  mutate(
    Icd10_num = paste0(
      substr(Icd10_num, 1, 2), ".",    # First two characters and a period
      substr(Icd10_num, 3, nchar(Icd10_num)) # The rest of the string
    )
  )
dat2 <- dat2 %>%
  mutate(
    Icd10_num = case_when(
      Icd10_num == '00.'  ~ '0.00001',
      Icd10_num == '00.0' ~ '0.0000001',
      TRUE ~ Icd10_num  # Default case to retain original Icd10_num if no conditions are met
    )
  )

dat2$Icd10_num <- as.numeric(dat2$Icd10_num)
dat2$Icd10_num <- floor(dat2$Icd10_num)
dat2_s <- dat2 %>%
  mutate(
    chap_1 = case_when(
      icd10_let %in% c('A') & Icd10_num %in% 0:99 ~ 1,
      icd10_let %in% c('B') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_2 = case_when(
      icd10_let %in% c('C') ~ 1,
      icd10_let %in% c('D') & Icd10_num %in% 0:48 ~ 1, TRUE ~ 0
    ),
    chap_3 = case_when(
      icd10_let %in% c('D') & Icd10_num %in% 50:89 ~ 1, TRUE ~ 0
    ),
    chap_4 = case_when(
      icd10_let %in% c('E') & Icd10_num %in% 0:90 ~ 1, TRUE ~ 0
    ),
    chap_5 = case_when(
      icd10_let %in% c('F') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_6 = case_when(
      icd10_let %in% c('G') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_7 = case_when(
      icd10_let %in% c('H') & Icd10_num %in% 0:59 ~ 1, TRUE ~ 0
    ),
    chap_8 = case_when(
      icd10_let %in% c('H') & Icd10_num %in% 60:95 ~ 1, TRUE ~ 0
    ),
    chap_9 = case_when(
      icd10_let %in% c('I') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_10 = case_when(
      icd10_let %in% c('J') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_11 = case_when(
      icd10_let %in% c('K') & Icd10_num %in% 0:93 ~ 1, TRUE ~ 0
    ),
    chap_12 = case_when(
      icd10_let %in% c('L') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_13 = case_when(
      icd10_let %in% c('M') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_14 = case_when(
      icd10_let %in% c('N') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_15 = case_when(
      icd10_let %in% c('O') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_16 = case_when(
      icd10_let %in% c('P') & Icd10_num %in% 0:96 ~ 1, TRUE ~ 0
    ),
    chap_17 = case_when(
      icd10_let %in% c('Q') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_18 = case_when(
      icd10_let %in% c('R') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_19 = case_when(
      icd10_let %in% c('S') & Icd10_num %in% 0:99 ~ 1,
      icd10_let %in% c('T') & Icd10_num %in% 0:98 ~ 1, TRUE ~ 0
    ),
    chap_20 = case_when(
      icd10_let %in% c('V', 'X', 'Y') ~ 1, TRUE ~ 0
    ),
    chap_21 = case_when(
      icd10_let %in% c('Z') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    ),
    chap_22 = case_when(
      icd10_let %in% c('U') & Icd10_num %in% 0:99 ~ 1, TRUE ~ 0
    )
  )
dat2 %>%
  filter(str_detect(stat1, "V"))
##save.image(file='progress1.RData')
## mulai dari sini
#load("progress1.RData")
dat2 <- dat2 %>% select(-icd10_let, -Icd10_num)
dat2_s <- dat2_s %>%
  group_by(PSTV01) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
dat2_s <- dat2_s %>% select(-Icd10_num)
obs_with_all_zeros <- dat2_s$PSTV01[apply(dat2_s[, 2:22], 1, function(x) all(x == 0))]

#cek icd kosong
obs_zero <- dat2 %>% filter(PSTV01 %in% obs_with_all_zeros)

#deteksi pindah kelas
switch <- dataset %>%
  group_by(PSTV01) %>%
  summarise(distinct_count = n_distinct(segmen1)) %>%
  filter(distinct_count > 1)
switch


dataset <- dataset %>%
  mutate(com1 = case_when(
    str_detect(com1, pattern = "Ringan") ~ "ringan",
    str_detect(com1, pattern = "Sedang") ~ "sedang",
    str_detect(com1, pattern = "Berat") ~ "berat",
    str_detect(com1, pattern = "Jalan") ~ "rjalan"
  ))
dataset$com1 <- as.factor(dataset$com1)

#select dataset
dataset %>% select(PSTV01, cost, demografi1, demografi2, demografi3, demografi4, stat6, klaim1,
                   klaim2, akses1, akses2, rs1, rs2)

#breakdown sum stat6, cost
#breakdown last demografi1, demografi2, demograif3, demografi4
#breakdown dan count: rs1, rs2, klaim1, klaim2, com1 

#pivot count
rs1 <- dataset %>% 
  count(PSTV01,rs1) %>% 
  pivot_wider(names_from = rs1,
              values_from = n) 
  
rs2 <- dataset %>%
  count(PSTV01,rs2) %>% 
  pivot_wider(names_from = rs2,
              values_from = n)
klaim1 <- dataset %>%
  count(PSTV01,klaim1) %>% 
  pivot_wider(names_from = klaim1,
              values_from = n)
klaim2 <- dataset %>% 
  count(PSTV01,klaim2) %>% 
  pivot_wider(names_from = klaim2,
              values_from = n)
com1 <- dataset %>% 
  count(PSTV01,com1) %>% 
  pivot_wider(names_from = com1,
              values_from = n)

count <- rs1 %>%
  left_join(rs2, by = "PSTV01") %>%
  left_join(klaim1, by = "PSTV01") %>%
  left_join(klaim2, by = "PSTV01") %>%
  left_join(com1, by = "PSTV01")
count[is.na(count)] <- 0
#rm(rs1, rs2, klaim1, klaim2, com1)

# pivot sum and last
sum <- dataset %>% group_by(PSTV01) %>%
  summarise(usia = last(demografi1),
            gender = last(demografi2),
            pernikahan = last(demografi3),
            peserta = last(segmen1),
            los = sum(stat6),
            cost = sum(cost)) %>% 
    ungroup()  # remove the grouping structure

#join all dataset
final_dataset <-  sum %>%
  left_join(count, by = "PSTV01") %>%
  left_join(dat2_s, by = "PSTV01") 
summary(final_dataset)
#rm(sum, count, dat2_s)
final_dataset <- filter(final_dataset, MISSING == 0)
final_dataset <- final_dataset %>% select(-MISSING) 
final_dataset <- final_dataset %>% select(-chap_22, -chap_20)
final_dataset <- na.omit(final_dataset)
#write.csv(final_dataset, "final_dataset.csv", row.names = FALSE)

summary(final_dataset)
