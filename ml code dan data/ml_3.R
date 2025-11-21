library(tidyverse)
setwd("C:/Users/dhihr/OneDrive/riset bu yeni")
dat = read.csv('final_dataset.csv', header = TRUE)


dat <- dat %>%
  mutate_if(is.character, factor)
summary(dat)
dat <- dat %>% select(-PSTV01)
dat <- dat %>% select (-chap_16)
dat <- na.omit(dat)
hist(dat$cost, breaks = 1000, xlim = c(0, 14190950))
ggplot(dat, aes(x=gender)) +
  geom_bar(fill="gray", color="black")
ggplot(dat, aes(x=peserta)) +
  geom_bar(fill="gray", color="black")
ggplot(dat, aes(x=pernikahan)) +
  geom_bar(fill="gray", color="black")
ggplot(dat, aes(x=pernikahan)) +
  geom_bar(fill="gray", color="black")

# category var
# Reshape the data
var_cat <- dat %>%
  select(where(is.factor), cost)
long_data <- var_cat %>%
  pivot_longer(cols = -cost, names_to = "variable", values_to = "value")

# Create the boxplot
ggplot(long_data, aes(x = value, y = cost)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x") +
  theme(axis_text_x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Cost Distribution by Category",
       x = "Category",
       y = "Cost")

#num var
dat_plus <- dat
#dat_plus$gender <- as.numeric(dat_plus$gender)
#dat_plus$pernikahan <- as.numeric(dat_plus$pernikahan)
#dat_plus$peserta <- as.numeric(dat_plus$peserta)
var_num <- dat_plus %>% select_if(is.numeric)
var_num <- var_num %>% rename(
                              Age = usia,
                              Cost = cost,
                              `Length of Stay` = los,
                              `Hospital Private ` = RS.Swasta,
                              `Hospital Govt` = RS.Milik.Pemerintah,
                              `Hosp Type D ` = RS.Kelas.D,
                              `Hosp Type C ` = RS.Kelas.C,
                              `Hosp Type B ` = RS.Kelas.B,
                              `Hosp Special ` = RS.Khusus,
                              `Hosp Type A ` = RS.Kelas.A,
                              `Hosp Other ` = Lainnya,
                              `Type Outpatient` = RJTL,
                              `Type Inpatient` = RITL,
                              `Class I` = KELAS.I,
                              `Class II` = KELAS.II,
                              `Class III` = KELAS.III,
                              `Sev Mild` = ringan,
                              `Sev Outpatient` = rjalan,
                              `Sev Severe` = berat,
                              `Sev moderate` = sedang,
                              `ICD Chap 1` = chap_1,
                              `ICD Chap 2` = chap_2,
                              `ICD Chap 3` = chap_3,
                              `ICD Chap 4` = chap_4,
                              `ICD Chap 5` = chap_5,
                              `ICD Chap 6` = chap_6,
                              `ICD Chap 7` = chap_7,
                              `ICD Chap 8` = chap_8,
                              `ICD Chap 9` = chap_9,
                              `ICD Chap 10` = chap_10,
                              `ICD Chap 11` = chap_11,
                              `ICD Chap 12` = chap_12,
                              `ICD Chap 13` = chap_13,
                              `ICD Chap 14` = chap_14,
                              `ICD Chap 15` = chap_15,
                              `ICD Chap 17` = chap_17,
                              `ICD Chap 18` = chap_18,
                              `ICD Chap 19` = chap_19,
                              `ICD Chap 21` = chap_21)
                              
head(var_num)
# Reshape wide -> long
var_num_long <- var_num %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = factor(variable, 
                           levels = c("Cost", setdiff(unique(variable), "Cost"))))

# Plot with Cost first
p<- ggplot(var_num_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 5) +
  theme_minimal() +
  labs(
    title = " ",
    x = "",
    y = " "
  ) + theme(
    axis.text.x = element_text(size = 6),  # decrease x-axis tick label size
    axis.text.y = element_text(size = 6)   # decrease y-axis tick label size
  ) + scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))

p

# Save with high resolution
setwd("C:/Users/dhihr/OneDrive/riset bu yeni/manuskrip")
ggsave(
  filename = "histograms_num.jpg", # file name
  plot = p,                    # plot object
  width = 14,                  # width in inches
  height = 16,                  # height in inches
  dpi = 300                    # resolution
)

#corplot
library(corrplot)
cor_matrix <- cor(var_num, use = "complete.obs")
# Visualize the correlation matrix
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)
corrplot(cor_matrix,title = " ", method = "square", outline = T, 
         addgrid.col = "darkgray", order="FPC", 
         rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", 
         tl.cex = 1.2, cl.cex = 1.2)
subset_cost <- cor_matrix[,"cost"]
subset_cost <- subset_cost[order((subset_cost), decreasing = TRUE)]
subset_cost <- subset_cost[-which(names(subset_cost) == "cost")]
subset_cost <- data.frame(variable = names(subset_cost), correlation = subset_cost)
subset_cost$variable <- factor(subset_cost$variable, levels = subset_cost$variable)
ggplot(subset_cost, aes(x = variable, y = correlation)) +
  geom_bar(stat = "identity", fill = "gray", color = "black") +
  coord_flip() +
  labs(title = "Correlation with Cost",
       x = "Variable",
       y = "Correlation") + theme_minimal()


#table
library(table1)
library(forcats)

table <- dat
render.categorical <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x)), function(y) with(y,
                                                                        sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT))))
}


render.strat <- function (label, n, ...) {
  sprintf("<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>", 
          label, prettyNum(n, big.mark=","))
}

render.continuous.sep <- function(x) {
  # Mean (SD)
  m_sd <- sprintf("%s (%s)",
                  formatC(mean(x, na.rm = TRUE), big.mark = ",", format = "f", digits = 0),
                  formatC(sd(x, na.rm = TRUE), big.mark = ",", format = "f", digits = 0))
  
  # Median (Q1–Q3)
  med_q <- sprintf("%s (%s–%s)",
                   formatC(median(x, na.rm = TRUE), big.mark = ",", format = "f", digits = 0),
                   formatC(quantile(x, 0.25, na.rm = TRUE), big.mark = ",", format = "f", digits = 0),
                   formatC(quantile(x, 0.75, na.rm = TRUE), big.mark = ",", format = "f", digits = 0))
  
  # Return both rows
  c("Mean (SD)" = m_sd, "Median (Q1–Q3)" = med_q)
}



#change to english categorical data
table <- table %>%
  mutate(
    gender = factor(gender,
                    levels = c("LAKI-LAKI", "PEREMPUAN"),   
                    labels = c("Male", "Female")),
    pernikahan = factor(pernikahan,
                        levels = c('BELUM KAWIN', 'CERAI', 'KAWIN', 'TIDAK TERDEFINISI'),
                        labels = c('Unmarried', 'Divorced', 'Married', 'Undefined')),
    peserta = factor(peserta,
                      levels = c('BUKAN PEKERJA', 'PBI APBD', 'PBI APBN', 'PBPU', 'PPU'),
                      labels = c('Non-Worker', 'PBI APBD', 'PBI APBN', 'PBPU', 'PPU'))
  )

#label
label(table$cost) <- "Cost (IDR)"
label(table$usia) <- "Age"
label(table$gender) <- "Sex"
label(table$pernikahan) <- "Marital Status"
label(table$peserta)           <- 'Participant Segmentation'
label(table$cost)              <- "Cost"
label(table$los)               <- "Length of Stay"
label(table$RS.Swasta)         <- "Hospital Private"
label(table$RS.Milik.Pemerintah) <- "Hospital Govt"
label(table$RS.Kelas.D)        <- "Hosp Type D"
label(table$RS.Kelas.C)        <- "Hosp Type C"
label(table$RS.Kelas.B)        <- "Hosp Type B"
label(table$RS.Khusus)         <- "Hosp Special"
label(table$RS.Kelas.A)        <- "Hosp Type A"
label(table$Lainnya)           <- "Hosp Other"
label(table$RJTL)              <- "Type Outpatient"
label(table$RITL)              <- "Type Inpatient"
label(table$KELAS.I)           <- "Class I"
label(table$KELAS.II)          <- "Class II"
label(table$KELAS.III)         <- "Class III"
label(table$ringan)            <- "Sev Mild"
label(table$rjalan)            <- "Sev Outpatient"
label(table$berat)             <- "Sev Severe"
label(table$sedang)            <- "Sev moderate"
label(table$chap_1)            <- "ICD Chap 1"
label(table$chap_2)            <- "ICD Chap 2"
label(table$chap_3)            <- "ICD Chap 3"
label(table$chap_4)            <- "ICD Chap 4"
label(table$chap_5)            <- "ICD Chap 5"
label(table$chap_6)            <- "ICD Chap 6"
label(table$chap_7)            <- "ICD Chap 7"
label(table$chap_8)            <- "ICD Chap 8"
label(table$chap_9)            <- "ICD Chap 9"
label(table$chap_10)           <- "ICD Chap 10"
label(table$chap_11)           <- "ICD Chap 11"
label(table$chap_12)           <- "ICD Chap 12"
label(table$chap_13)           <- "ICD Chap 13"
label(table$chap_14)           <- "ICD Chap 14"
label(table$chap_15)           <- "ICD Chap 15"
label(table$chap_17)           <- "ICD Chap 17"
label(table$chap_18)           <- "ICD Chap 18"
label(table$chap_19)           <- "ICD Chap 19"
label(table$chap_21)           <- "ICD Chap 21"

#table1
table1(~., data = table, render.categorical=render.categorical, overall = c(left="Total"),     
       render.strat=render.strat, render.continuous = render.continuous.sep)
table1(~cost + usia + gender + pernikahan + peserta, data = table, render.categorical=render.categorical, overall = c(left="Total"),     
       render.strat=render.strat, render.continuous = render.continuous.sep)
table1(~RS.Swasta + RS.Milik.Pemerintah + RS.Kelas.D + RS.Kelas.C + RS.Kelas.B + RS.Khusus + RS.Kelas.A + Lainnya, data = table, render.categorical=render.categorical, overall = c(left="Total"),     
       render.strat=render.strat, render.continuous =render.continuous.sep)
table1(~RJTL + RITL + KELAS.I + KELAS.II + KELAS.III + los, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = render.continuous.sep)
table1(~ringan + sedang + berat + rjalan, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = render.continuous.sep)
table1(~chap_1 + chap_2 + chap_3 + chap_4 + chap_5 + chap_6, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = render.continuous.sep)
table1(~chap_7 + chap_8 + chap_9 + chap_10 + chap_11 + chap_12, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = render.continuous.sep)
table1(~chap_13 + chap_14 + chap_15 + chap_17 + chap_18 + chap_19 + chap_21, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = render.continuous.sep)


#Partition
# membagi data 80% training, 20% testing
library(caret)
set.seed(1234)
bagi <- createDataPartition(dat$cost, p = 0.8, list=F) 
training<- dat[bagi,]
testing<- dat[-bagi,]

## METODE VALIDASI ##
# cross-validasi 5 lipat
fit.control <- trainControl(method = "cv", number = 5)

# REGRESI LINIER #
#running model
lr <- lm(cost ~ ., data = training)
summary(lr)
testing$prediksiLR <- predict(lr, testing)

# melihat tingkat error atau akurasi hasil prediksi
postResample(testing$prediksiLR, testing$cost)

# RANDOM FOREST #
# Define the different values of ntree you want to test
ntree_values <- c(500,700)
tunegrid <- expand.grid(.mtry = (1:5))

# Initialize a list to store models
model_list <- list()

# Loop over ntree values and train model for each, storing each in the list
for(ntree in ntree_values) {
  set.seed(123)  # for reproducibility
  model <- train(cost ~ ., dat = training, method = "rf", trControl = fit.control,
                 ntree = ntree, tuneGrid = tunegrid)
  model_list[[paste("rf_ntree", ntree, sep = "_")]] <- model
}

# Now model_list contains each model
model_list$rf_ntree_10
#save.image("ml_progress2.RData")
#saveRDS(model_list,'rforest_list1.RData')
#load("ml_progress3.RData")
model_list2 <- readRDS("rforest_list3.RData")
model_list <- readRDS("rforest_list1.RData")


#generate performance to data frame
results <- resamples(model_list)
tree20 <- data.frame(mtry = model_list$rf_ntree_20$results$mtry, RMSE = model_list$rf_ntree_20$results$RMSE)
tree50 <- data.frame(mtry = model_list$rf_ntree_50$results$mtry, RMSE = model_list$rf_ntree_50$results$RMSE)
tree100 <- data.frame(mtry = model_list$rf_ntree_100$results$mtry, RMSE = model_list$rf_ntree_100$results$RMSE)
tree200 <- data.frame(mtry = model_list$rf_ntree_200$results$mtry, RMSE = model_list$rf_ntree_200$results$RMSE)
tree500 <- data.frame(mtry = model_list2$rf_ntree_500$results$mtry, RMSE = model_list2$rf_ntree_500$results$RMSE)
tree700 <- data.frame(mtry = model_list2$rf_ntree_700$results$mtry, RMSE = model_list2$rf_ntree_700$results$RMSE)

#best tune
rf_besttune <- tree700$mtry[which.min(tree700$RMSE)]

# melakukan prediksi terhadap data testing forest 10
testing$prediksiForest20 <- predict(model_list$rf_ntree_20, testing)
testing$prediksiForest50 <- predict(model_list$rf_ntree_50, testing)
testing$prediksiForest100 <- predict(model_list$rf_ntree_100, testing)
testing$prediksiForest200 <- predict(model_list$rf_ntree_200, testing)
testing$prediksiForest500 <- predict(model_list2$rf_ntree_500, testing)
testing$prediksiForest700 <- predict(model_list2$rf_ntree_700, testing)
View(testing)

# melihat tingkat error atau akurasi hasil prediksi
postResample(testing$prediksiForest20, testing$cost)
postResample(testing$prediksiForest50, testing$cost)
postResample(testing$prediksiForest100, testing$cost)
postResample(testing$prediksiForest200, testing$cost)
postResample(testing$prediksiForest500, testing$cost)
postResample(testing$prediksiForest700, testing$cost)

# melihat variabel importance
varImp(model_list$rf_ntree_20)
varImp(model_list$rf_ntree_50)
varImp(model_list$rf_ntree_100)
varImp(model_list$rf_ntree_200)
varImp(model_list2$rf_ntree_500)
varImp(model_list2$rf_ntree_700)

#XGB100
nrounds <- 500
tune_grid <- expand.grid(
  nrounds = seq(from = 1, to = nrounds, by = 10),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 5, 7, 10),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(123)
xgb500 <- caret::train(
  cost ~ ., data = training,
  trControl = fit.control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

#save model
#saveRDS(xgb200,'xgb200.RData')
xgb500 <- readRDS("xgb500.RData")

# plot tuning
plot(xgb100)
xgb100$bestTune
plot(xgb200)
xgb200$bestTune
plot(xgb500)
xgb500$bestTune

best_tune <- rbind(xgb100$bestTune, xgb200$bestTune, xgb500$bestTune)
best_tune$nrounds <- c(100, 200, 500)
#write.csv(best_tune, "best_tune.csv")
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
komparasi <- data.frame(Model = c("LR", "RF20", "RF50", "RF100", "RF200", "RF500", "RF700", "XGB100", "XGB200", "XGB500"),
                        round(rbind(postResample(testing$prediksiLR, testing$cost),
                                    postResample(testing$prediksiForest20, testing$cost),
                                    postResample(testing$prediksiForest50, testing$cost),
                                    postResample(testing$prediksiForest100, testing$cost),
                                    postResample(testing$prediksiForest200, testing$cost),
                                    postResample(testing$prediksiForest500, testing$cost),
                                    postResample(testing$prediksiForest700, testing$cost),
                                    postResample(testing$prediksixgb100, testing$cost),
                                    postResample(testing$prediksixgb200, testing$cost),
                                    postResample(testing$prediksixgb500, testing$cost)), 2))
#write.csv(komparasi, "komparasi.csv")

tree20 <- data.frame(mtry = tree20$mtry, RMSE = tree20$RMSE)
tree50 <- data.frame(mtry = tree50$mtry, RMSE = tree50$RMSE)
tree100 <- data.frame(mtry = tree100$mtry, RMSE = tree100$RMSE)
tree200 <- data.frame(mtry = tree200$mtry, RMSE = tree200$RMSE)
tree500 <- data.frame(mtry = tree500$mtry, RMSE = tree500$RMSE)
tree700 <- data.frame(mtry = tree700$mtry, RMSE = tree700$RMSE)

#plot
# Assuming all data frames have the same columns 'mtry' and 'RMSE'
tree_data <- rbind(
  data.frame(mtry = tree20$mtry, RMSE = tree20$RMSE, TreeSize = factor("20", levels = c("20", "50", "100", "200", "500", "700"))),
  data.frame(mtry = tree50$mtry, RMSE = tree50$RMSE, TreeSize = factor("50", levels = c("20", "50", "100", "200", "500", "700"))),
  data.frame(mtry = tree100$mtry, RMSE = tree100$RMSE, TreeSize = factor("100", levels = c("20", "50", "100", "200", "500", "700"))),
  data.frame(mtry = tree200$mtry, RMSE = tree200$RMSE, TreeSize = factor("200", levels = c("20", "50", "100", "200", "500", "700"))),
  data.frame(mtry = tree500$mtry, RMSE = tree500$RMSE, TreeSize = factor("500", levels = c("20", "50", "100", "200", "500", "700"))),
  data.frame(mtry = tree700$mtry, RMSE = tree700$RMSE, TreeSize = factor("700", levels = c("20", "50", "100", "200", "500", "700")))
)


library(ggplot2)

# Assuming tree_data is already created and rf_besttune and color_palette are set
# Specifying colors by name or hexadecimal code
color_palette <- c("20" = "#FF0550",  # Red
                   "50" = "#00FF00",  # Green
                   "100" = "#0000FF",  # Blue
                   "200" = "gold",  # Yellow
                   "500" = "#FF00FF",  # Magenta
                   "700" = "darkcyan")  # Cyan


library(ggplot2)
library(dplyr)

# Assuming rf_besttune is defined
max_rmse <- max(tree_data$RMSE)  # Calculate max RMSE outside of ggplot call

library(ggplot2)

ggplot(tree_data, aes(x = mtry, y = RMSE, color = TreeSize)) +
  geom_line(aes(size = ifelse(TreeSize == "1000", 1.5, 0.5))) +  # Conditional line size
  labs(title = 'Random Forest Performance', x = 'Mtry', y = 'RMSE') +
  geom_vline(xintercept = rf_besttune, linetype="dashed", color="tomato", size=0.7) +
  annotate("label", x = rf_besttune - 2, y = max_rmse, label = paste0('Best Tune = mtry (', rf_besttune, ') & ntree (700)'), color="black") +
  theme_minimal() +
  scale_color_manual(values = color_palette,
                     name = "Tree Size",
                     breaks = c("20", "50", "100", "200", "500", "700")) +
  scale_size(range = c(0.5, 1.5), guide = "none")  # Use "none" instead of FALSE to remove the size guide

#data frame vip
vip_xgb500 <- data.frame(type = 'XG Boost', name = '500 Nrounds', Variable = rownames(varImp(xgb500)$importance),
                         Importance = round(varImp(xgb500)$importance[,1],2)) %>% arrange(desc(Importance)) %>% 
                         head(20)
vip_rf20 <- data.frame(type = 'Random Forest', name = '20 Trees', Variable = rownames(varImp(model_list$rf_ntree_20)$importance), 
                       Importance = round(varImp(model_list$rf_ntree_20)$importance[,1],2)) %>% arrange(desc(Importance)) %>%
                       filter(Variable %in% vip_xgb500$Variable)
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

head(vip_xgb500,7)
head(vip_rf100, 10)

vip <- rbind(vip_rf20, vip_rf50, vip_rf100, vip_rf200, vip_xgb100, vip_xgb200, vip_xgb500) 
vip$name <- factor(vip$name, levels = c('20 Trees', '50 Trees', '100 Trees', '200 Trees', '100 Nrounds', '200 Nrounds', '500 Nrounds'))
vip$Variable <- factor(vip$Variable, levels = vip_xgb500$Variable)
vip <- vip %>%
  mutate(Variable = recode(Variable, "RJTL" = "Outpatients", 
                         "RITL" = "Inpatients",
                         "los" = "Length of Stay",
                         'RS.Kelas.A' =  'Hospital Type A',
                         'sedang' = 'Severity Mild',
                         'berat' = 'Severity Severe',
                         'usia' = 'Age',
                         'RS.Kelas.B' =  'Hospital Type B'
                         ))

vip %>% ggplot() +
  geom_point(aes(x = name, y = Variable, size = Importance, color = type)) + theme_minimal()
vip$name

#feature selection
#rf
vp_rf200 <- varImp(model_list$rf_ntree_200, scale = TRUE)
print(rf_importance)
selected_features_rf <- rownames(rf_importance$importance)[
  rf_importance$importance$Overall > quantile(rf_importance$importance$Overall, 0.75)
]

#xgb
print(selected_features_rf)
xgb_importance <- varImp(xgb500, scale = TRUE)
selected_features_xgb <- rownames(xgb_importance$importance)[
  xgb_importance$importance$Overall > quantile(xgb_importance$importance$Overall, 0.75)
]
print(selected_features_xgb)
