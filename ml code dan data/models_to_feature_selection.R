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
var_num <- dat %>% select_if(is.numeric)
head(var_num)
ggplot(var_num, aes(x = usia, y = cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
ggplot(var_num, aes(x = los, y = cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 
ggplot(var_num, aes(x = RS.Swasta, y = cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
ggplot(var_num, aes(x = RS.Milik.Pemerintah, y = cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 
ggplot(var_num, aes(x = RJTL, y = cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 
ggplot(var_num, aes(x = RITL, y = cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 
pairs(cost ~usia + RS.Swasta + RS.Milik.Pemerintah + RS.Kelas.C + RS.Kelas.D + RS.Kelas.B + RS.Khusus +
        RS.Kelas.A + Lainnya, data=var_num, lower.panel=NULL)
pairs(cost ~RJTL+RITL+KELAS.I+KELAS.II+KELAS.II, data=var_num, lower.panel=NULL)


#corplot
library(corrplot)
cor_matrix <- cor(var_num, use = "complete.obs")
# Visualize the correlation matrix
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)
subset_cost <- cor_matrix[,"cost"]
subset_cost <- subset_cost[order(abs(subset_cost), decreasing = TRUE)]
subset_cost <- subset_cost[-which(names(subset_cost) == "cost")]
subset_cost <- data.frame(variable = names(subset_cost), correlation = subset_cost)
subset_cost$variable <- factor(subset_cost$variable, levels = subset_cost$variable)
ggplot(subset_cost, aes(x = variable, y = correlation)) +
  geom_bar(stat = "identity", fill = "gray", color = "black") +
  coord_flip() +
  labs(title = "Correlation with Cost",
       x = "Variable",
       y = "Correlation")


#table
library(table1)
library(forcats)

table <- dat
table$cost_cat <- as.factor(ifelse(table$cost > 2622737, "High", "Low"))
table <- table %>% select(-cost)
render.categorical <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x)), function(y) with(y,
                                                                        sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT))))
}


render.strat <- function (label, n, ...) {
  sprintf("<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>", 
          label, prettyNum(n, big.mark=","))
}



label(table$cost_cat) <- "Biaya Perawatan Rata-Rata Per Kapita (IDR)"
tab1 <- table1(~.| cost_cat
               , data=table, render.categorical=render.categorical, 
               overall=c(left="Total"), render.strat=render.strat, 
               caption = ' ')
tab1
table1(~usia + gender + pernikahan + peserta| cost_cat, data = table, render.categorical=render.categorical, overall = c(left="Total"),     
       render.strat=render.strat, render.continuous = c(.="Mean(SD)", .="Median (Q1-Q3)"))
table1(~RS.Swasta + RS.Milik.Pemerintah + RS.Kelas.D + RS.Kelas.C + RS.Kelas.B + RS.Khusus + RS.Kelas.A + Lainnya| cost_cat, data = table, render.categorical=render.categorical, overall = c(left="Total"),     
       render.strat=render.strat, render.continuous = c(.="Mean(SD)", .="Median (Q1-Q3)"))
table1(~RJTL + RITL + KELAS.I + KELAS.II + KELAS.III + los| cost_cat, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = c(.="Mean(SD)", .="Median (Q1-Q3)"))
table1(~ringan + sedang + berat + rjalan| cost_cat, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = c(.="Mean(SD)", .="Median (Q1-Q3)"))
table1(~chap_1 + chap_2 + chap_3 + chap_4 + chap_5 + chap_6| cost_cat, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = c(.="Mean(SD)", .="Median (Q1-Q3)"))
table1(~chap_7 + chap_8 + chap_9 + chap_10 + chap_11 + chap_12| cost_cat, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = c(.="Mean(SD)", .="Median (Q1-Q3)"))
table1(~chap_13 + chap_14 + chap_15 + chap_17 + chap_18 + chap_19 + chap_21| cost_cat, data = table, render.categorical=render.categorical, overall = c(left="Total"),
       render.strat=render.strat, render.continuous = c(.="Mean(SD)", .="Median (Q1-Q3)"))

#Partition
# membagi data 80% training, 20% testing
library(caret)
set.seed(1234)
bagi <- createDataPartition(dat$cost, p = 0.8, list=F) 
training<- dat[bagi,]
testing<- dat[-bagi,]

## METODE VALIDASI ##
# cross-validasi 5 folds
fit.control <- trainControl(method = "cv", number = 5)

# REGRESI LINIER #
#running model
lr <- lm(cost ~ ., data = training)
summary(lr)
testing$prediksiLR <- predict(lr, testing)

# performance
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

# prediction
testing$prediksiForest20 <- predict(model_list$rf_ntree_20, testing)
testing$prediksiForest50 <- predict(model_list$rf_ntree_50, testing)
testing$prediksiForest100 <- predict(model_list$rf_ntree_100, testing)
testing$prediksiForest200 <- predict(model_list$rf_ntree_200, testing)
testing$prediksiForest500 <- predict(model_list2$rf_ntree_500, testing)
testing$prediksiForest700 <- predict(model_list2$rf_ntree_700, testing)
View(testing)

# performance
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
  #data.frame(mtry = tree500$mtry, RMSE = tree500$RMSE, TreeSize = factor("500", levels = c("20", "50", "100", "200", "500", "700"))),
  #data.frame(mtry = tree700$mtry, RMSE = tree700$RMSE, TreeSize = factor("700", levels = c("20", "50", "100", "200", "500", "700")))
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
                         'sedang' = 'Severity Moderate',
                         'berat' = 'Severity Severe',
                         'usia' = 'Age',
                         'RS.Kelas.B' =  'Hospital Type B',
                         'KELAS.I' = 'Class I Facility',
                         'RS.Kelas.C' =  'Hospital Type C',
                         'RS.Khusus' = 'Special Hospital',
                         'RS.Milik.Pemerintah' = 'Government Hospital',
                         'RS.Swasta' = 'Private Hospital',
                         'genderPEREMPUAN' = 'Female',
                         'chap_11' = 'ICD-10 Chapter 11',
                         'chap_21' = 'ICD-10 Chapter 21',
                         'KELAS.III' = 'Class IIII Facility',
                         'pesertaPBPU' = 'Non-Wage Recipient Workers',
                         'RS.Kelas.D' =  'Hospital Type D',
                         'chap_7' = 'ICD-10 Chapter 7',
                          ))

vip %>% ggplot() +
  geom_point(aes(x = name, y = Variable, size = Importance, color = type)) + xlab('Alghoritm') + ylab('Variable') +
  theme_minimal() +
  theme(legend.position="top") 
vip$name

#value of vip
vip_icd <- vip %>% filter(str_detect(Variable, "ICD"))
summary(vip_icd$Importance)
vip_out <- vip %>% filter(Variable == 'Outpatients')
summary(vip_out$Importance)
#feature selection
#rf
vp_rf200 <- varImp(model_list$rf_ntree_200, scale = TRUE)
print(rf_importance)
selected_features_rf <- rownames(rf_importance$importance)[
  rf_importance$importance$Overall > quantile(rf_importance$importance$Overall, 0.80)
]
print(selected_features_rf)

#xgb
xgb_importance <- varImp(xgb500, scale = TRUE)
selected_features_xgb <- rownames(xgb_importance$importance)[
  xgb_importance$importance$Overall > quantile(xgb_importance$importance$Overall, 0.80)
]
print(selected_features_xgb)


#rerun
# Subset Dataset Based on Selected Features
selected_rf_data <- dat[, c(selected_features_rf, "cost")]  # For Random Forest
selected_xgb_data <- dat[, c(selected_features_xgb, "cost")]  # For XGBoost

#feature selection2
dat <- dat %>%
  select(cost, everything())
library(datawizard)
dat <- to_numeric(dat)

set.seed(123)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)


lmProfile <- rfe(x = dat[,2:42], 
                 y = dat$cost,
                 sizes = c(1:41),
                 rfeControl = ctrl)

lmProfile
predictors_tab <- lmProfile$results
write.csv(predictors_tab, 'predictors_tab.csv')

predictors(lmProfile)
predictors <- predictors(lmProfile)
write.csv(predictors, 'predictors.csv')

trellis.par.set(caretTheme())
plot(lmProfile, type = c("g", "o"))

dat_clean <- dat %>% select(predictors)
write.csv(dat_clean, 'feature_selection.csv')
