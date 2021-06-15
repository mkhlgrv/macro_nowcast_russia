get.next.weekday <- function(date, day, lead=0){
  date <- as.Date(date)
  out <- Date()
  for(i in 1:length(date)){
    dates <- seq(date[i]+ 7*(lead), date[i] + 7*(lead+1) - 1, by="days")
    out[i] <- dates[wday(dates)==day]
  }
  
  out
}





get.available.observation <- function(week_n, lag){
  previuos_q <- min(12, 12+week_n - lag)
  current_q <-  min(12, week_n - lag)
  c(previuos_q, current_q)
}
get.actual.observation <- function(week_n, quarter_lag = 0){
  import('data/transformed/desc.xlsx', sheet = 1, skip=0) %>%
    .[c('name', 'lag')] %>%
    group_by(name) %>%
    mutate(available.observation = 
             get.available.observation(week_n=week_n, lag=lag)[2-quarter_lag]) %>%
    select(1,3)
}





get.df <- function(start_training_date=as.Date('2007-04-01'),
                   end_training_date=as.Date('2014-10-01'),
                   testing_date =as.Date('2015-01-01'),
                   week_n=0, # от недели под номером -4 до 28
                   target,
                   predictor_group=NA){
  df_dates <- c(seq(as.Date(start_training_date),as.Date(end_training_date),
                    by = '1 quarter'), testing_date)
  target_data <- stat_data %>%
    filter(variable==target) %>%  
    filter(h == max(h))%>%
    select(date=quarter_first_friday, variable, value) %>%
    mutate(variable = 'y')
  
  
  pred_data <- import('data/transformed/desc.xlsx', sheet=1)[c('name', 'is_noise','is_sna')]
  if(predictor_group=='All'){
    predictors <- pred_data %>% pull(name)
  } else if(predictor_group=='non_sna'){
    predictors <- pred_data %>% filter(!is_sna) %>% pull(name)
  } else if(predictor_group=='non_financial'){
    
    predictors <- pred_data %>%
      filter(!is_noise) %>%
      pull(name)
  }
  
  
  x_data <- bind_rows(stat_data %>% 
                        filter(variable %in% predictors) %>%
                        inner_join(get.actual.observation(week_n=week_n, quarter_lag=0),
                                   by = c('variable'='name')) %>%
                        filter(h <= available.observation) %>%
                        group_by(variable, quarter_first_friday) %>%
                        filter(h == max(h)) %>%
                        ungroup,
                      stat_data %>%
                        filter(variable %in% predictors) %>%
                        inner_join(get.actual.observation(week_n=week_n, quarter_lag=1),
                                   by = c('variable'='name')) %>%
                        filter(h <= available.observation) %>%
                        group_by(variable, quarter_first_friday) %>%
                        filter(h == max(h)) %>%
                        ungroup %>%
                        mutate(quarter_first_friday = as.yearqtr(quarter_first_friday+100) %>%
                                 as.Date %>%
                                 get.next.weekday(day = 6, lead = 0),
                               variable = paste0(variable, '_lag'))
  ) %>%
    select(date=quarter_first_friday, variable, value)
  
  bind_rows(target_data, x_data) %>%
    filter(date
           %in% as.Date(df_dates %>%
                          get.next.weekday(day = 6, lead = 0)))%>%
    dcast(date~variable)
  
}



train.model <- function(model = 'rf',
                        week_n = 0,
                        target='gdp_real',
                        start_training_date= as.Date('2007-04-01'),
                        end_training_date = as.Date('2014-10-01'),
                        testing_date = as.Date('2015-01-01'),
                        predictor_group = 'All'){
  
  
  df <- get.df(target = target,
               predictor_group = predictor_group,
               week_n = week_n,
               start_training_date =start_training_date ,
               end_training_date = end_training_date,
               testing_date = testing_date
  )
  
  
  
  
  
  
  
  
  # матрицы с 1 строкой не воспринимаются как матрицы, поэтому 
  # приходится повторять тестовую матрицу
  train_n <- 1:(nrow(df)-1)
  test_n <- nrow(df)
  
  X.matrix <- model.matrix(y~0+., data = df[,-1])
  X.train <- X.matrix[train_n,]
  
  X.test <- X.matrix[rep(test_n,2),]
  
  y.train <- df$y[train_n] %>% as.numeric
  
  y.test <- df$y[rep(test_n,2)] %>% as.numeric
  
  
  
  if (model == 'rf'){
    
    model_fit <- randomForest(x = X.train,
                              y = y.train,
                              metric = "RMSE",
                              ntree = 2000,
                              nodesize =c(3,5,7),
                              replace = TRUE,
                              mtry = floor((nrow(df)-2)/3),
                              corr.bias=TRUE,)
    
    
    
  } else if (model == 'boost'){
    
    tune_grid <- expand.grid(nrounds = c(20,50,100),
                             max_depth = c(4,5, 6),
                             eta = c(0.1,0.2, 0.3),
                             gamma = 0,
                             colsample_bytree = 0.33,
                             min_child_weight = 1,
                             subsample = 1)
    
    
    model_fit <- train(x = X.train,
                       y = y.train,
                       method = "xgbTree",
                       metric = "RMSE",
                       tuneGrid = tune_grid,)
    
    
  }
  else if(model == 'elnet'){
    tune_grid <- expand.grid(
      # .alpha = seq(0,1, by = 0.1),
      .alpha = c(0.5),
      .lambda = seq(0.1, 0.00000000001,length.out = 500))
    model_fit <- train(x=X.train,
                       y=y.train,
                       method = "glmnet",
                       metric = "RMSE",
                       tuneGrid =tune_grid
    )
    
    
    
  }else if(model == 'lasso'){
    tc <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3,
                       search = 'grid')
    tune_grid <- expand.grid(
      # .alpha = seq(0,1, by = 0.1),
      .alpha = c(1),
      .lambda = seq(0.1, 0.00000000001,length.out = 500))
    
    model_fit <- train(x=X.train,
                       y=y.train,
                       method = "glmnet",
                       metric = "RMSE",
                       tuneGrid =tune_grid,
                       trControl = tc
    )
    
    
    
    
  }
  else if(model == 'ridge'){
    tc <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3,
                       search = 'grid')
    tune_grid <- expand.grid(
      # .alpha = seq(0,1, by = 0.1),
      .alpha = c(0),
      .lambda = seq(0.1, 0.00000000001,length.out = 500))
    
    model_fit <- train(x=X.train,
                       y=y.train,
                       method = "glmnet",
                       metric = "RMSE",
                       tuneGrid =tune_grid,
                       trControl = tc
    )
    
    
    
    
  }
  
  else if(model == 'svm'){
    tc <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3,
                       search = 'grid')
    
    model_fit <- train(x=X.train,
                       y=y.train,
                       method = "svmLinear",
                       metric = "RMSE",
                       tuneLength = 100,
                       trControl = tc
    )
    
    
    
    
  }
  
  else if(model == 'knn'){
    tc <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3,
                       search = 'grid')
    
    model_fit <- train(x=X.train,
                       y=y.train,
                       method = "knn",
                       metric = "RMSE",
                       tuneLength = 100,
                       trControl = tc
    )
    
    
    
    
  }
  else if(model == 'bagging'){
    tc <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3,
                       search = 'grid')
    
    model_fit <- train(x=X.train,
                       y=y.train,
                       method = "treebag",
                       metric = "RMSE",
                       tuneLength = 100,
                       trControl = tc
    )
    
    
    
    
  }

  pred <- predict(model_fit, newdata = X.test) %>%
    as.numeric  
  
  
  tibble(
    startdt = df$date[1],
    enddt = df$date[nrow(df)-1],
    forecastdate = df$date[nrow(df)],
    date = df$date[nrow(df)],
    y_true = y.test[1],
    y_pred = pred[1]
  )
}








# 2007Q3
start_training_date <- as.Date('2007-04-01')
# 2014Q4 -- 2020Q1
end_training_dates <- seq(as.Date('2014-10-01'),as.Date('2020-07-01'), by = 'quarter')
# 2015Q1 -- 202Q2
end_testing_dates <- seq(as.Date('2015-01-01'),as.Date('2020-10-01'), by = 'quarter')