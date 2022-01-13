make.rmse.table <- function(end_testing_dates = seq(as.Date("2015-01-01"),as.Date("2021-04-01"), by = "quarter"),
                            week_n_ = NULL,
                            target = c('gdp_real','cons_real',
                                       'invest_real',
                                       'invest_fixed_capital_real', 'export_real', 'import_real',
                                       'export_usd', 'import_usd'),
                            model_ = c('rf', 'boost','lasso', 'ridge',"elnet",
                                       "ar", "arx",
                                       'knn', 'svm', "bagging", 'rw'),
                            relative_to_rw = TRUE,
                            export = FALSE,
                            export_path=NULL,
                            cumulative_rmse = FALSE,
                            dcast = TRUE
                            ){
  if(is.null(week_n_)){
    week_n_ <- unique(out$week_n)
  }
  df <- out %>%
    mutate(date = zoo::as.Date(zoo::as.yearqtr(out$date))) %>%
    filter(date %in% end_testing_dates,
           week_n %in% week_n_,
           model %in% model_
    )
  if(cumulative_rmse){
    df <- df %>%
      arrange(date) %>%
      group_by(target, model, week_n) %>%
      mutate(rmse = sqrt(cummean((y_true-y_pred)^2)))

  } else {
    df <- df %>%
      group_by(target, model, week_n) %>%
      summarise(rmse = sqrt(mean((y_true-y_pred)^2)))
  }
  
  if(relative_to_rw){
    value_to_join <- c("target", "week_n")
    if(cumulative_rmse){
      value_to_join <- c(value_to_join, "date")
    }

    print(df)
    df_not_rw <- df %>% filter(model != "rw")
    df_rw <- df %>% filter(model == "rw")
    df <- df_not_rw %>%
      inner_join(df_rw,
                 by = value_to_join,
                 suffix = c("", "_rw")) %>%
      mutate(rmse = rmse/rmse_rw) %>%
      select(-c(rmse_rw, model_rw))
    print(df)
  }
  if(dcast){
    df <- df %>% reshape2::dcast(target+week_n~model)
  }
  
  if(export&!is.null(export_path)){
    
    rio::export(df,
                file = export_path)
  }
  df

    
  
}
