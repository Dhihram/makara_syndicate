library(tidyverse)
library(caret)
library(ggplot2)
setwd("C:/Users/dhihr/OneDrive/riset bu yeni")
dat = read.csv('final_dataset.csv', header = TRUE)


dat <- dat %>%
  mutate_if(is.character, factor)
summary(dat)
dat <- dat %>% select(-PSTV01)
dat <- dat %>% select (-chap_16)
dat <- na.omit(dat)

#running rf model
model_list <- readRDS("rforest_rfe_list.RData")

#Partition
#  80% training, 20% testing
library(caret)
set.seed(1234)
bagi <- createDataPartition(dat$cost, p = 0.8, list=F) 
training<- dat[bagi,]
testing<- dat[-bagi,]

## METODE VALIDASI ##
# cross-validasi 5 lipat
fit.control <- trainControl(method = "cv", number = 5)

#generate performance to data frame
results <- resamples(model_list)
tree50 <- data.frame(mtry = model_list$rf_ntree_50$results$mtry, RMSE = model_list$rf_ntree_50$results$RMSE)
tree100 <- data.frame(mtry = model_list$rf_ntree_100$results$mtry, RMSE = model_list$rf_ntree_100$results$RMSE)
tree200 <- data.frame(mtry = model_list$rf_ntree_200$results$mtry, RMSE = model_list$rf_ntree_200$results$RMSE)

# prediction
testing$prediksiForest50 <- predict(model_list$rf_ntree_50, testing)
testing$prediksiForest100 <- predict(model_list$rf_ntree_100, testing)
testing$prediksiForest200 <- predict(model_list$rf_ntree_200, testing)
View(testing)

#check every fold
res_ntree_50 <- model_list$rf_ntree_50$resample
res_wide_50 <- res_ntree_50 %>%
  pivot_longer(cols = c(RMSE, Rsquared, MAE),
               names_to = "Metric",
               values_to = "Value") %>%
  pivot_wider(names_from = Resample, values_from = Value) %>%
  rowwise() %>%
  mutate(
    Mean = mean(c_across(starts_with("Fold"))),
    SD   = sd(c_across(starts_with("Fold"))),
    Algorithm = "RF ntree 50" 
  ) %>%
  ungroup() %>%
  relocate(Algorithm, .before = Metric)

res_ntree_100 <- model_list$rf_ntree_100$resample
res_wide_100 <- res_ntree_100 %>%
  pivot_longer(cols = c(RMSE, Rsquared, MAE),
               names_to = "Metric",
               values_to = "Value") %>%
  pivot_wider(names_from = Resample, values_from = Value) %>%
  rowwise() %>%
  mutate(
    Mean = mean(c_across(starts_with("Fold"))),
    SD   = sd(c_across(starts_with("Fold"))),
    Algorithm = "RF ntree 100" 
  ) %>%
  ungroup() %>%
  relocate(Algorithm, .before = Metric)

res_ntree_200 <- model_list$rf_ntree_200$resample
res_wide_200 <- res_ntree_200 %>%
  pivot_longer(cols = c(RMSE, Rsquared, MAE),
               names_to = "Metric",
               values_to = "Value") %>%
  pivot_wider(names_from = Resample, values_from = Value) %>%
  rowwise() %>%
  mutate(
    Mean = mean(c_across(starts_with("Fold"))),
    SD   = sd(c_across(starts_with("Fold"))),
    Algorithm = "RF ntree 100" 
  ) %>%
  ungroup() %>%
  relocate(Algorithm, .before = Metric)
rf_wide <- rbind(res_wide_50,res_wide_100,res_wide_200)

# error and performance evaluation
postResample(testing$prediksiForest50, testing$cost)
postResample(testing$prediksiForest100, testing$cost)
postResample(testing$prediksiForest200, testing$cost)

#best tune
rf_besttune <- tree200$mtry[which.min(tree200$RMSE)]

# melihat variabel importance
varImp(model_list$rf_ntree_50)
varImp(model_list$rf_ntree_100)
varImp(model_list$rf_ntree_200)

#plot
# Assuming all data frames have the same columns 'mtry' and 'RMSE'
tree_data <- rbind(
  data.frame(mtry = tree50$mtry, RMSE = tree50$RMSE, TreeSize = factor("50", levels = c("50", "100", "200"))),
  data.frame(mtry = tree100$mtry, RMSE = tree100$RMSE, TreeSize = factor("100", levels = c("50", "100", "200"))),
  data.frame(mtry = tree200$mtry, RMSE = tree200$RMSE, TreeSize = factor("200", levels = c("50", "100", "200")))
)

color_palette <- c("50" = "darkcyan",  # Green
                   "100" = "#0000FF",  # Blue
                   "200" = "firebrick"  
)

# Assuming rf_besttune is defined
max_rmse <- max(tree_data$RMSE)  # Calculate max RMSE outside of ggplot call

library(ggplot2)

ggplot(tree_data, aes(x = mtry, y = RMSE, color = TreeSize)) +
  geom_line(aes(size = ifelse(TreeSize == "1000", 0.75, 0.5))) +  # Conditional line size
  labs(title = 'Random Forest Performance', x = 'Mtry', y = 'RMSE') +
  geom_vline(xintercept = rf_besttune, linetype="dashed", color="tomato", size=0.7) +
  annotate("label", x = rf_besttune - 0.75, y = max_rmse, label = paste0('Best Tune = mtry (', rf_besttune, ') & ntree (700)'), color="black") +
  theme_minimal() +
  scale_color_manual(values = color_palette,
                     name = "Tree Size",
                     breaks = c("50", "100", "200")) + 
  scale_size(range = c(0.5, 1.5), guide = "none")  # Use "none" instead of FALSE to remove the size guide


# REGRESI LINIER #
#running model
lr <- train(cost ~ ., data = training, method = "lm", trControl = fit.control)
summary(lr)
testing$prediksiLR <- predict(lr, testing)

#check every fold
res_lr <- lr$resample
lr_wide <- res_lr %>%
  pivot_longer(cols = c(RMSE, Rsquared, MAE),
               names_to = "Metric",
               values_to = "Value") %>%
  pivot_wider(names_from = Resample, values_from = Value) %>%
  rowwise() %>%
  mutate(
    Mean = mean(c_across(starts_with("Fold"))),
    SD   = sd(c_across(starts_with("Fold"))),
    Algorithm = "Linear Reg" 
  ) %>%
  ungroup() %>%
  relocate(Algorithm, .before = Metric)

#plot
linear_reg <- data.frame(coef = coef(lr))
linear_reg <- linear_reg %>% 
  rownames_to_column(var = "variable") %>%
  arrange(desc(coef)) %>% 
  filter(variable != "(Intercept)")

# Convert the variable column to a factor with levels based on the current order
linear_reg <- linear_reg %>%
  mutate(variable = recode(variable, "RJTL" = "Outpatients", 
                           'ringan' = 'Severity Mild',
                           'sedang' = 'Severity Moderate',
                           'pesertaPBI APBN' = 'Assistance by National Govt',
                           'pesertaPBI APBD' = 'Assistance by Regional Govt',
                           'genderPEREMPUAN' = 'Sex Female',
                           'pernikahanTIDAK TERDEFINISI' = 'Marital Status Undefined',
                           'RS.Kelas.D' =  'Hospital Type D',
                           'RS.Kelas.C' =  'Hospital Type C',
                           'pernikahanCERAI' = 'Marital Status Divorced',
                           'RS.Khusus' = 'Special Hospital',
                           'RS.Kelas.B' =  'Hospital Type B',
                           'usia' = 'Age',
                           'pernikahanKAWIN' = 'Marital Status Married',
                           'KELAS.I' = 'Class I Facility',
                           'pesertaPPU' = 'Wage-Recipient Workers',
                           'pesertaPBPU' = 'Non-Wage Recipient Worker',
                           "los" = "Length of Stay",
                           'RS.Kelas.A' =  'Hospital Type A',
                           "chap_1" = "ICD-10 Chapter 1",
                           "chap_2" = "ICD-10 Chapter 2",
                           "chap_3" = "ICD-10 Chapter 3",
                           "chap_4" = "ICD-10 Chapter 4",
                           "chap_5" = "ICD-10 Chapter 5",
                           "chap_6" = "ICD-10 Chapter 6",
                           "chap_7" = "ICD-10 Chapter 7",
                           "chap_8" = "ICD-10 Chapter 8",
                           "chap_9" = "ICD-10 Chapter 9",
                           "chap_10" = "ICD-10 Chapter 10",
                           "chap_11" = "ICD-10 Chapter 11",
                           "chap_12" = "ICD-10 Chapter 12",
                           "chap_13" = "ICD-10 Chapter 13",
                           "chap_14" = "ICD-10 Chapter 14",
                           "chap_15" = "ICD-10 Chapter 15",
                           "chap_17" = "ICD-10 Chapter 17",
                           "chap_18" = "ICD-10 Chapter 18",
                           "chap_19" = "ICD-10 Chapter 19",
                           "chap_21" = "ICD-10 Chapter 21",
                           'RS.Swasta' = 'Private Hospital',
                           'RS.Milik.Pemerintah' = 'Government Hospital'
  ))
linear_reg$variable <- factor(linear_reg$variable, levels = linear_reg$variable)

ggplot(linear_reg, aes(x = variable, y = coef)) +
  geom_bar(stat = "identity", fill = "gray", color = "black") +
  coord_flip() +
  labs(title = "Linear Regression Coefficient Against Cost",
       x = "Variable",
       y = "Coef") + theme_classic()

#XGBoost

#running xgb model
xgb100 <- readRDS("xgb100_rfe.RData")
xgb200 <- readRDS("xgb200_rfe.RData")
xgb500 <- readRDS("xgb500_rfe.RData")

#check every fold
res_xgb_100 <- xgb100$resample
res_wide_xgb100 <- res_xgb_100 %>%
  pivot_longer(cols = c(RMSE, Rsquared, MAE),
               names_to = "Metric",
               values_to = "Value") %>%
  pivot_wider(names_from = Resample, values_from = Value) %>%
  rowwise() %>%
  mutate(
    Mean = mean(c_across(starts_with("Fold"))),
    SD   = sd(c_across(starts_with("Fold"))),
    Algorithm = "XGB nround 50" 
  ) %>%
  ungroup() %>%
  relocate(Algorithm, .before = Metric)

res_xgb_200 <- xgb200$resample
res_wide_xgb200 <- res_xgb_200 %>%
  pivot_longer(cols = c(RMSE, Rsquared, MAE),
               names_to = "Metric",
               values_to = "Value") %>%
  pivot_wider(names_from = Resample, values_from = Value) %>%
  rowwise() %>%
  mutate(
    Mean = mean(c_across(starts_with("Fold"))),
    SD   = sd(c_across(starts_with("Fold"))),
    Algorithm = "XGB nround 200" 
  ) %>%
  ungroup() %>%
  relocate(Algorithm, .before = Metric)

res_xgb_500 <- xgb500$resample
res_wide_xgb500 <- res_xgb_500 %>%
  pivot_longer(cols = c(RMSE, Rsquared, MAE),
               names_to = "Metric",
               values_to = "Value") %>%
  pivot_wider(names_from = Resample, values_from = Value) %>%
  rowwise() %>%
  mutate(
    Mean = mean(c_across(starts_with("Fold"))),
    SD   = sd(c_across(starts_with("Fold"))),
    Algorithm = "XGB nround 500" 
  ) %>%
  ungroup() %>%
  relocate(Algorithm, .before = Metric)

xgb_wide <- rbind(res_wide_xgb100, res_wide_xgb200, res_wide_xgb500)

# plot tuning
par(mfrow = c(3, 1)) 
plot(xgb100, main = 'A. XG Boost 100 NRounds')
plot(xgb200, main = 'B. XG Boost 200 NRounds')
plot(xgb500, main = 'C. XG Boost 500 NRounds')
xgb100$bestTune
xgb200$bestTune
xgb500$bestTune

best_tune <- rbind(xgb100$bestTune, xgb200$bestTune, xgb500$bestTune)
best_tune$nrounds <- c(100, 200, 500)

# melakukan prediksi terhadap data testing xgb
testing$prediksixgb100 <- predict(xgb100, testing)
testing$prediksixgb200 <- predict(xgb200, testing)
testing$prediksixgb500 <- predict(xgb500, testing)

#model performance
postResample(testing$prediksixgb100, testing$cost)
postResample(testing$prediksixgb200, testing$cost)
postResample(testing$prediksixgb500, testing$cost)

# melihat variabel importance
varImp(xgb100)
varImp(xgb200)
varImp(xgb500)

#besst tune
xgb100$bestTune
xgb200$bestTune
xgb500$bestTune


#combine
comp <- data.frame(Model = c("LR", "RF50", "RF100", "RF200", "XGB100", "XGB200", "XGB500"),
                        round(rbind(postResample(testing$prediksiLR, testing$cost),
                                    postResample(testing$prediksiForest50, testing$cost),
                                    postResample(testing$prediksiForest100, testing$cost),
                                    postResample(testing$prediksiForest200, testing$cost),
                                    postResample(testing$prediksixgb100, testing$cost),
                                    postResample(testing$prediksixgb200, testing$cost),
                                    postResample(testing$prediksixgb500, testing$cost)), 2))
write.csv(comp, "komparasi_rfe.csv")

#combine fold
fold_wide <- rbind(lr_wide, rf_wide, xgb_wide)
fold_wide <- fold_wide %>%
  mutate(across(starts_with("Fold"), as.numeric)) %>% 
  mutate(across(c(Fold1:SD), ~ ifelse(Metric == "Rsquared",
                                      round(.x, 2),   
                                      round(.x, 0)))) 
write.csv(fold_wide, "fold_wide_rfe.csv")

#data frame vip
vip_xgb500 <- data.frame(type = 'XG Boost', name = '500 Nrounds', Variable = rownames(varImp(xgb500)$importance),
                         Importance = round(varImp(xgb500)$importance[,1],2)) %>% arrange(desc(Importance)) %>% 
  head(20)
vip_rf50 <- data.frame(type = 'Random Forest', name = '50 Trees', Variable = rownames(varImp(model_list$rf_ntree_50)$importance), 
                       Importance = round(varImp(model_list$rf_ntree_50)$importance[,1],2)) %>% arrange(desc(Importance)) %>%
  filter(Variable %in% vip_xgb500$Variable)
vip_rf100 <- data.frame(type = 'Random Forest', name = '100 Trees', Variable = rownames(varImp(model_list$rf_ntree_100)$importance), 
                        Importance = round(varImp(model_list$rf_ntree_100)$importance[,1],2)) %>% arrange(desc(Importance)) %>%
  filter(Variable %in% vip_xgb500$Variable)
vip_rf200 <- data.frame(type = 'Random Forest', name = '200 Trees', Variable = rownames(varImp(model_list$rf_ntree_200)$importance), 
                        Importance = round(varImp(model_list$rf_ntree_200)$importance[,1],2)) %>% arrange(desc(Importance)) %>%
  filter(Variable %in% vip_xgb500$Variable)
vip_xgb100 <- data.frame(type = 'XG Boost', name = '100 Nrounds', Variable = rownames(varImp(xgb100)$importance), 
                         Importance = round(varImp(xgb100)$importance[,1],2)) %>% arrange(desc(Importance)) %>%
  filter(Variable %in% vip_xgb500$Variable)
vip_xgb200 <- data.frame(type = 'XG Boost', name = '200 Nrounds', Variable = rownames(varImp(xgb200)$importance),
                         Importance = round(varImp(xgb200)$importance[,1],2)) %>% arrange(desc(Importance)) %>% 
  filter(Variable %in% vip_xgb500$Variable)
vip_lr <- data.frame(type = 'Linear Reg', name = 'Linear Reg', Variable = rownames(varImp(lr)),
                         Importance = round(varImp(lr)$Overall,2)) %>% arrange(desc(Importance)) %>% 
  filter(Variable %in% vip_xgb500$Variable)

head(vip_xgb500,7)
head(vip_rf100, 10)

vip <- rbind(vip_lr, vip_rf50, vip_rf100, vip_rf200, vip_xgb100, vip_xgb200, vip_xgb500) 
vip$name <- factor(vip$name, levels = c('Linear Reg', '50 Trees', '100 Trees', '200 Trees', '100 Nrounds', '200 Nrounds', '500 Nrounds'))
vip$Variable <- factor(vip$Variable, levels = vip_xgb500$Variable)
vip <- vip %>%
  mutate(Variable = recode(Variable, "RJTL" = "Outpatients", 
                           "los" = "Length of Stay",
                           "chap_14" = "ICD-10 Chapter 14",
                           'RS.Kelas.A' =  'Hospital Type A',
                           'chap_21' = 'ICD-10 Chapter 21',
                           'sedang' = 'Severity Moderate',
                           'KELAS.I' = 'Class I Facility',
                           'chap_9' = 'ICD-10 Chapter 9',
                           'chap_13' = 'ICD-10 Chapter 13',
                           'RS.Kelas.B' =  'Hospital Type B',
                           'RS.Khusus' = 'Special Hospital',
                           'chap_7' = 'ICD-10 Chapter 7',
                           'ringan' = 'Severity Mild',
                           'RS.Swasta' = 'Private Hospital',
                           'usia' = 'Age',
                           'RS.Kelas.C' =  'Hospital Type C',
                           'RS.Milik.Pemerintah' = 'Government Hospital',
                           'chap_19' = 'ICD-10 Chapter 19',
                           'chap_11' = 'ICD-10 Chapter 11',
                           'chap_2' = 'ICD-10 Chapter 2'
  ))

#VIP plot
vip_plot <- vip %>% ggplot() +
  geom_point(aes(x = name, y = Variable, size = Importance, color = type)) + xlab('Alghoritm') + ylab('Variable') +
  theme_minimal() +  ggtitle('A. Variable of Importance') + scale_color_manual(
    values = c('Linear Reg' = "#ffa47d", 'Random Forest' = "#7eded6", 'XG Boost' = "#7dafff")
  ) +
  theme(legend.position="top") 
vip$name

# xgb best plot
vip_best_xgb <- vip %>% filter(name == '500 Nrounds')
xgb_best_plot <- ggplot(vip_best_xgb, aes(x = Variable, y = Importance)) +
  geom_vline(xintercept = 1:20, col = "grey80", size = 0.005, linetype="dashed") +
  geom_point(size = 5, col = "#7eded6") +
  coord_flip() + 
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank()) +
  xlab('Variable') + ylab('Importance') + ggtitle('B. XG Boost 500 Nrounds Importance') 

#rf best plot
vip_rf200$Variable <- factor(vip_rf200$Variable, levels = vip_rf200$Variable)
vip_rf200 <- vip_rf200 %>%
  mutate(Variable = recode(Variable,"los" = "Length of Stay", 
                           "RJTL" = "Outpatients", 
                           "chap_21" = "ICD-10 Chapter 21",
                           'RS.Swasta' = 'Private Hospital',
                           'KELAS.I' = 'Class I Facility',
                           'sedang' = 'Severity Moderate',
                           'RS.Kelas.B' =  'Hospital Type B',
                           'RS.Kelas.A' =  'Hospital Type A',
                           'RS.Milik.Pemerintah' = 'Government Hospital',
                           'ringan' = 'Severity Mild',
                           'chap_14' = 'ICD-10 Chapter 14',
                           'RS.Kelas.C' =  'Hospital Type C',
                           'chap_9' = 'ICD-10 Chapter 9',
                           'usia' = 'Age',
                           'RS.Khusus' = 'Special Hospital',
                           'chap_7' = 'ICD-10 Chapter 7',
                           'chap_13' = 'ICD-10 Chapter 13',
                           'chap_2' = 'ICD-10 Chapter 2',
                           'chap_19' = 'ICD-10 Chapter 19',
                           'chap_11' = 'ICD-10 Chapter 11',
  ))
rf_best_plot <- ggplot(vip_rf200, aes(x = Variable, y = Importance)) +
  geom_vline(xintercept = 1:20, col = "grey80", size = 0.005, linetype="dashed") +
  geom_point(size = 6.5, col = "#7dafff") +
  coord_flip() + 
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank()) +
  xlab('Variable') + ylab('Importance') + ggtitle('C. Random Forest 200 Trees Importance') 

# lr best plot
lr_var <- varImp(lr)
lr_var$Variable <- rownames(lr_var)
rownames(lr_var) <- NULL
lr_var <- rename(lr_var,c('Importance'='Overall')) %>% select(Variable, Importance)
lr_var <- lr_var %>% arrange(desc(Importance))
lr_var$Variable <- factor(lr_var$Variable, levels = lr_var$Variable)
lr_var <- head(lr_var,20)
lr_var <- lr_var %>%
  mutate(Variable = recode(Variable,
                           'RJTL' = 'Outpatients',
                           'ringan' = 'Severity Mild',
                           'sedang' = 'Severity Moderate',
                           'los' = 'Length of Stay',
                           'RS.Kelas.D' =  'Hospital Type D',
                           'RS.Kelas.A' =  'Hospital Type A',
                           'KELAS.I' = 'Class I Facility',
                           'RS.Kelas.C' =  'Hospital Type C',
                           'pesertaPBI APBN' = 'Assistance by National Govt',
                           'genderPEREMPUAN' = 'Sex Female',
                           'RS.Khusus' = 'Special Hospital',
                           'usia' = 'Age',
                           'RS.Kelas.B' =  'Hospital Type B',
                           'pesertaPBI APBD' = 'Assistance by Regional Govt',
                           'pernikahanTIDAK TERDEFINISI' = 'Marital Status Undefined',
                           'RS.Swasta' = 'Private Hospital',
                           'RS.Milik.Pemerintah' = 'Government Hospital',
                           'pesertaPPU' = 'Wage-Recipient Workers',
                           'pesertaPBPU' = 'Non-Wage Recipient Worker',
                           'chap_19' = 'ICD-10 Chapter 19'
))
lr_best_plot <- ggplot(lr_var, aes(x = Variable, y = Importance)) +
  geom_vline(xintercept = 1:20, col = "grey80", size = 0.005, linetype="dashed") +
  geom_point(size = 6.5, col = "#ffa47d") +
  coord_flip() + 
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank()) +
  xlab('Variable') + ylab('Importance') + ggtitle('D. Linear Regression Importance')                           
                           
library(patchwork)  
vip_plot / xgb_best_plot / rf_best_plot / lr_best_plot

#VIP measurement

#data frame vip
vip_xgb500 <- data.frame(type = 'XG Boost', name = '500 Nrounds', Variable = rownames(varImp(xgb500)$importance),
                         Importance = round(varImp(xgb500)$importance[,1],2)) %>% arrange(desc(Importance))
vip_rf50 <- data.frame(type = 'Random Forest', name = '50 Trees', Variable = rownames(varImp(model_list$rf_ntree_50)$importance), 
                       Importance = round(varImp(model_list$rf_ntree_50)$importance[,1],2)) %>% arrange(desc(Importance)) 
vip_rf100 <- data.frame(type = 'Random Forest', name = '100 Trees', Variable = rownames(varImp(model_list$rf_ntree_100)$importance), 
                        Importance = round(varImp(model_list$rf_ntree_100)$importance[,1],2)) %>% arrange(desc(Importance)) 
vip_rf200 <- data.frame(type = 'Random Forest', name = '200 Trees', Variable = rownames(varImp(model_list$rf_ntree_200)$importance), 
                        Importance = round(varImp(model_list$rf_ntree_200)$importance[,1],2)) %>% arrange(desc(Importance)) 
vip_xgb100 <- data.frame(type = 'XG Boost', name = '100 Nrounds', Variable = rownames(varImp(xgb100)$importance), 
                         Importance = round(varImp(xgb100)$importance[,1],2)) %>% arrange(desc(Importance)) 
vip_xgb200 <- data.frame(type = 'XG Boost', name = '200 Nrounds', Variable = rownames(varImp(xgb200)$importance),
                         Importance = round(varImp(xgb200)$importance[,1],2)) %>% arrange(desc(Importance)) 
vip_lr <- data.frame(type = 'Linear Reg', name = 'Linear Reg', Variable = rownames(varImp(lr)),
                     Importance = round(varImp(lr)$Overall,2)) %>% arrange(desc(Importance)) 

vip <- rbind(vip_lr, vip_rf50, vip_rf100, vip_rf200, vip_xgb100, vip_xgb200, vip_xgb500) 

vip_outpatients <- vip %>% filter(Variable == 'RJTL')
mean(vip_outpatients$Importance)                           
sd(vip_outpatients$Importance)                         

vip_icd <- vip %>%
  filter(
    str_detect(Variable, "chap")
  )  
mean(vip_icd$Importance)
sd(vip_icd$Importance)                           

vip_icd %>% group_by(Variable) %>% summarise(mean = mean(Importance), sd = sd(Importance)) %>% arrange(desc(mean))                           
                           
#scatter plot testing
#LR
plotlr <- ggplot(testing, aes(x = cost, y = prediksiLR)) +
  geom_point(color = "steelblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Prediction vs Actual Cost (Linear Regression)",
    x = "Actual Cost",
    y = "Predicted Cost"
  ) +
  theme_minimal()
#xgb
plotxgb <- ggplot(testing, aes(x = cost, y = prediksixgb500)) +
  geom_point(color = "darkblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Prediction vs Actual Cost (XGBoost)",
    x = "Actual Cost",
    y = "Predicted Cost"
  ) +
  theme_minimal()
#rf
plotrf <- ggplot(testing, aes(x = cost, y = prediksiForest200)) +
  geom_point(color = "darkcyan", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Prediction vs Actual Cost (Random Forest)",
    x = "Actual Cost",
    y = "Predicted Cost"
  ) +
  theme_minimal()

library(patchwork)
scatplot <- plotlr + plotrf + plotxgb

setwd("C:/Users/dhihr/OneDrive/riset bu yeni/manuskrip")
ggsave(
  filename = "scatplot_highres.png",
  plot = scatplot,
  width = 12,         # in inches
  height = 5,         # adjust as needed
  dpi = 600           # high resolution
)

#test every fold
model_list$lr <- lr
model_list$xgb100  <- xgb100
model_list$xgb200  <- xgb200
model_list$xgb500  <- xgb500


# Paired t-tests
resamp <- resamples(model_list)
summary(resamp)


model_diffs <- diff(resamp)
summary(model_diffs)


# Quantile cost test
df <- testing %>%
  mutate(
    cost_decile = ntile(cost, 10),        # 10 groups
    high_cost   = ifelse(cost >= quantile(cost, 0.95), 1, 0) # top 5%
  )

# Evaluate error by decile
decile_eval <- df %>%
  group_by(cost_decile) %>%
  summarise(
    n = n(),
    MAE  = round(mean(abs(prediksixgb500 - cost)),0),
    RMSE = round(sqrt(mean((prediksixgb500 - cost)^2)),0),
    MedAE = round(median(abs(prediksixgb500 - cost)),0)
  )

write.csv(decile_eval, 'decile_evaluation.csv', row.names = FALSE)
