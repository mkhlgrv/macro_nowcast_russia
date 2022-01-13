wolak_test <-
  expand.grid(
                          model = c('rf', 'boost','lasso', 'ridge',
                                    'knn',"elnet",
    'svm', "bagging"),
                          target = c(
                            'gdp_real','cons_real',
                                     'invest_real',
                                     'invest_fixed_capital_real', 'export_real',
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
  
  res1 <- res2 <- res3 <- NA
  try({
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
  })

  
  
  
  tibble(
         target = x$target,
         model = x$model,
         p_value = c(res1, res2, res3) %>%
           as.numeric)
  
})

# save(wolak_test,file =  "wolak_test.Rdata")
# '-' если нулевая гипотеза отвергается

wolak_test %>%
  mutate(test_n = rep(c(1,2,3), 64)) %>%
  mutate(model = model_label_switch(model),
         target = target_rus_label_switch(target)) %>%
  arrange(model) %>%
  mutate(sign = ifelse(p_value<=0.025, "-", "")) %>%
  ggplot(aes(y = model, x = test_n))+
  geom_tile(show.legend = FALSE, fill="#F8766D00", color = "grey")+
  geom_text(aes(label=sign), size = 12)+
  facet_wrap(vars(target), ncol=4)+
  labs(y = '', x='')+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12))

