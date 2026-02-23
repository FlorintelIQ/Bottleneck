# Mon Feb  2 15:21:56 2026 ------------------------------
# Modelling pipeline

# considered models:
# Poisson GLM
# Knn
# Randomforest
# Quantile regression forest
# XgBoost


library(caret)
library(randomForest)
library(quantregForest)
library(xgboost)
library(nnet)


load('data.Rdata')
attach(data)

# Train test split ####


cut_date <- as.Date("2025-10-01")

train <- data[data$date < cut_date, ]
test  <- data[data$date >= cut_date, ]


# Poisson ####

fit_poisson <- glm(deliveries ~ rush + t + weekday +
             seasonality1 + seasonality2 + val_window + mom_window + xmas_window,
           family = poisson(), data = train)

lambda_hat <- predict(fit_poisson, test, type = "response")

rmse_pois <- sqrt(mean((test$deliveries - lambda_hat)^2))
mae_pois  <- mean(abs(test$deliveries - lambda_hat))
rmse_pois
mae_pois


gen_newdata_from_date <- function(date, val = 0, mom = 0, xmas = 0, seed = NULL) {
  
  # date kan Date zijn of "YYYY-MM-DD" string
  date <- as.Date(date)
  if (is.na(date)) stop("`date` could not be parsed as Date.")
  
  if (!is.null(seed)) set.seed(seed)
  
  doy <- as.integer(strftime(date, format = "%j"))  # 1..366
  day <- strftime(date, format = "%a")              # "Mon", "Tue", ...
  
  t <- 0:19
  
  make_peak <- function(base_start, base_len,
                        drift_amp = 2, jitter_sd = 1, len_jitter = 1) {
    
    drift <- round(drift_amp * sin(2 * pi * doy / 365))
    
    start <- base_start + drift + round(rnorm(1, 0, jitter_sd))
    len   <- base_len   + sample(seq(-len_jitter, len_jitter), 1)
    
    start <- max(0, min(19, start))
    len   <- max(1, len)
    end   <- min(19, start + len)
    
    list(start = start, end = end)
  }
  
  two_peak_days <- c("Mon", "Wed", "Fri")
  is_two_peaks <- day %in% two_peak_days
  
  if (is_two_peaks) {
    p1 <- make_peak(base_start = 2,  base_len = 3, drift_amp = 2, jitter_sd = 1)
    p2 <- make_peak(base_start = 11, base_len = 3, drift_amp = 2, jitter_sd = 1)
    rush <- as.integer((t >= p1$start & t <= p1$end) | (t >= p2$start & t <= p2$end))
  } else {
    p1 <- make_peak(base_start = 6, base_len = 4, drift_amp = 2, jitter_sd = 1)
    rush <- as.integer(t >= p1$start & t <= p1$end)
  }
  
  newdata <- data.frame(
    date         = date,  # handig om mee te nemen
    t            = t,
    rush         = rush,
    weekday      = factor(day, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
    doy          = doy,
    seasonality1 = sin(2 * pi * doy / 365),
    seasonality2 = cos(2 * pi * doy / 365),
    val_window   = as.integer(val),
    mom_window   = as.integer(mom),
    xmas_window  = as.integer(xmas)
  )
  
  newdata
}

newdata <- gen_newdata_from_date('2026-02-22')

# #confidence interval
pred <- predict(fit_poisson, newdata, type = "link", se.fit = TRUE)
newdata$lower <- exp(pred$fit - 1.96 * pred$se.fit)
newdata$upper <- exp(pred$fit + 1.96 * pred$se.fit)

newdata$lambda_hat <- predict(fit_poisson, newdata, type = "response")

plot(newdata$lambda_hat, type = 'b', ylim = c(0,4))


# KNN ####

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

# Random forest ####

set.seed(1)
mtry_grid <- 1:12
oob_mse <- numeric(length(mtry_grid))

for (i in seq_along(mtry_grid)) {
  rf_tmp <- randomForest(
    deliveries ~ t + rush + weekday + seasonality1 + seasonality2 +
      val_window + mom_window + xmas_window,
    data = train,
    ntree = 300,
    mtry = mtry_grid[i]
  )
  oob_mse[i] <- rf_tmp$mse[rf_tmp$ntree]
}

data.frame(
  mtry = mtry_grid,
  oob_mse = oob_mse)[,2] %>% which.min()



rf_tmp <- randomForest(
  deliveries ~ t + rush + weekday + seasonality1 + seasonality2 +
    val_window + mom_window + xmas_window,
  data = test,
  ntree = 300,
  mtry = 2
)

plot(rf_tmp)
summary(rf_tmp)
importance(rf_tmp)
varImpPlot(rf_tmp)

rf_pred <- predict(rf_tmp, test)

rmse_rf <- sqrt(mean((test$deliveries - rf_pred)^2))
mae_rf  <- mean(abs(test$deliveries - rf_pred))
rmse_rf
mae_rf

# Quantile regres
x_vars <- c( 't' , 'rush' , 'weekday' , 'seasonality1' , 'seasonality2',
               'val_window' , 'mom_window' , 'xmas_window')
qrf_fit <- quantregForest(
  x = train[,x_vars],
  y = train$deliveries,
  ntree = 500, 
  mtry = 2
)

pred_q <- predict(qrf_fit, test, quantiles = c(0.05, 0.5, 0.95))

rmse_qrf <- sqrt(mean((test$deliveries - pred_q[,2])^2))
mae_qrf  <- mean(abs(test$deliveries - pred_q[,2]))
rmse_qrf
mae_qrf

# XgBoost ####

X_train <- model.matrix(
  ~ t + rush + weekday + seasonality1 + seasonality2 + val_window + mom_window + xmas_window,
  data = train)[, -1]

X_test <- model.matrix(
  ~ t + rush + weekday + seasonality1 + seasonality2 + val_window + mom_window + xmas_window,
  data = test
)[, -1]

y_train <- train$deliveries
y_test  <- test$deliveries

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

grid <- expand.grid(
  eta = c(0.05, 0.1),
  max_depth = c(3, 4, 5),
  min_child_weight = c(1, 5),
  subsample = c(0.8, 1.0),
  colsample_bytree = c(0.8, 1.0)
)

best_rmse <- Inf
best_params <- NULL
best_model <- NULL

set.seed(1)

for (i in 1:nrow(grid)) {
  cat('Grid number: ', i, '\n')
  params_i <- list(
    objective = "count:poisson",
    eval_metric = "rmse",
    eta = grid$eta[i],
    max_depth = grid$max_depth[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  m <- xgb.train(
    params = params_i,
    data = dtrain,
    nrounds = 2000,
    evals = list(train = dtrain, test = dtest),
    early_stopping_rounds = 30,
    verbose = 0
  )
  
  rmse_i <- xgb.attributes(m)$best_score
  if (rmse_i < best_rmse) {
    best_rmse <- rmse_i
    best_params <- params_i
    best_model <- m
  }
}

best_rmse
best_params

xgb_pred <- predict(best_model, dtest)

# --- test RMSE (point prediction) ---
rmse_xgb <- sqrt(mean((y_test - xgb_pred)^2))
mae_xgb  <- mean(abs(test$deliveries - xgb_pred))
rmse_xgb
mae_xgb


# NN ####

mu <- colMeans(X_train)
sd <- apply(X_train, 2, sd)
sd[sd == 0] <- 1

X_train_s <- scale(X_train, center = mu, scale = sd)
X_test_s  <- scale(X_test,  center = mu, scale = sd)

sizes  <- c(5, 10, 20, 40)
decays <- c(0, 1e-4, 1e-3, 1e-2)

grid <- expand.grid(size = sizes, decay = decays)
grid$rmse <- NA_real_

set.seed(1)

for (i in seq_len(nrow(grid))) {
  cat('Grid number: ', i, '\n')
  fit <- nnet(
    x = X_train_s,
    y = y_train,
    size = grid$size[i],
    decay = grid$decay[i],
    linout = TRUE,
    maxit = 500,
    trace = FALSE
  )
  
  pred <- as.numeric(predict(fit, X_test_s))
  grid$rmse[i] <- sqrt(mean((y_test - pred)^2))
}

grid[order(grid$rmse), ]

fit_nn <- nnet(
  x = X_train_s,
  y = y_train,
  size = 5,
  decay = 0.01,
  linout = TRUE,
  maxit = 500,
  trace = FALSE
)

pred <- as.numeric(predict(fit_nn, X_test_s))
rmse_nn <- sqrt(mean((y_test - pred)^2))
mae_nn  <- mean(abs(y_test - pred))
rmse_nn
mae_nn
