library(tidyverse)
dat = read.csv('final_dataset.csv', header = TRUE)

dat <- dat %>%
  mutate_if(is.character, factor)
summary(dat)
dat <- dat %>% select(-PSTV01)
dat <- na.omit(dat)

#feature selection
predictor <- read.csv('predictors.csv', header = TRUE)

dat <- dat %>% select(predictor$x, cost)

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

#XGB
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

set.seed(1234)
xgb500 <- caret::train(
  cost ~ ., data = training,
  trControl = fit.control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)



#save model
#saveRDS(xgb500,'xgb500_rfe.RData')
#xgb500 <- readRDS("xgb500.RData")


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

# RANDOM FOREST #
# Define the different values of ntree you want to test
ntree_values <- c(50, 100, 200)
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
#model_list$rf_ntree_10
#save.image("ml_progress2.RData")
saveRDS(model_list,'rforest_rfe_list.RData')
#load("ml_progress2.RData")
#model_list2 <- readRDS("rforest_list2.RData")
#model_list <- readRDS("rforest_list1.RData")

