wolak_test <-
  expand.grid(
                          model = c('rf', 'boost','lasso', 'ridge',
  #                                   # "arx",
  #                                   'knn',
    'svm', "bagging"),
                          target = c(
                            # 'gdp_real','cons_real',
                            #          'invest_real',
                            #          'invest_fixed_capital_real', 'export_real',
                                     'import_real',
                                     'export_usd',
                                     'import_usd')) %>%
  split(seq(1:nrow(.))) %>%
map_dfr(function(x){
  
  

  pred_mat <- out %>%
    filter(model == x$model, target == x$target) %>%
    mutate(
      week_n = - week_n) %>%
    dcast(date ~ week_n,value.var='y_pred') %>%
    select(-date)
  true_mat <- out %>%
    filter(model == x$model, target == x$target)%>%
    mutate(
      week_n = - week_n) %>%
    dcast(date ~ week_n,value.var='y_true') %>%
    select(-date)
  max_h <- ncol(pred_mat)
  
  # 1) Monotonicity of the Forecast Errors
  e2 <- (true_mat-pred_mat)^2
  
  temp <- e2[,2:(max_h)]-e2[,1:(max_h-1)]
  res1 <- wolak(temp, difference=TRUE)[1]
  
  # 2) Monotonicity of the Mean Squared Forecast
  temp <- (pred_mat[,2:(max_h)])^2-(pred_mat[,1:(max_h-1)])^2
  res2 <- wolak(temp, difference=TRUE, increasing	=FALSE)[1]
  
  # 3) Monotonicity of Covariance Between the Forecast
  # and the Target Variable
  y_hat <- true_mat*pred_mat
  temp <- y_hat[,1:(max_h-1)] - y_hat[,2:(max_h)]
  res3 <- wolak(temp, difference=TRUE)[1]
  
  
  
  tibble(
         target = x$target,
         model = x$model,
         p_value = c(res1, res2, res3) %>%
           as.numeric)
  
})
wolak_test %>%
  mutate(test_n = rep(c(1,2,3), 56)) %>%
  dcast(target+test_n~model, value.var = 'p_value') %>%
  export('mono_test.xlsx')

