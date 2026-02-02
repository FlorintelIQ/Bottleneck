# Mon Feb  2 15:21:56 2026 ------------------------------
# Modelling pipeline

# considered models:
# Poisson GLM
# Knn
# Randomforest
# XgBoost

load('data.Rdata')
attach(data)
#### Train test split #####

cut_date <- as.Date("2025-10-01")

train <- data[data$date < cut_date, ]
test  <- data[data$date >= cut_date, ]


##### Poisson #####

fit_poisson <- glm(deliveries ~ rush + t + weekday +
             seasonality1 + seasonality2 + val_window + mom_window + xmas_window,
           family = poisson(), data = train)


##### KNN #####

library(caret)

train$weekday <- factor(train$weekday)
test$weekday <- factor(test$weekday)

ctrl <- trainControl(
  method = "cv",
  number = 10
)

set.seed(42)

knn_fit <- train(
  deliveries ~ t + rush + weekday + seasonality1 + seasonality2 +
    val_window + mom_window + xmas_window,
  data = train,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(1,30, by = 2) )
)

knn_fit

knn_pred <- predict(knn_fit, test)

rmse_knn <- sqrt(mean((test$deliveries - knn_pred)^2))
mae_knn  <- mean(abs(test$deliveries - knn_pred))
rmse_knn;mae_knn
