# Расчет значений теста ДМ
make.dm.table <- function(end_testing_dates = seq(as.Date("2015-01-01"),as.Date("2021-04-01"), by = "quarter"),
                          week_n_ = c(-10, -6, -5, -2, 0, 2, 5, 6, 10, 14, 15, 18, 20, 22),
                          target = c('gdp_real','cons_real',
                                     'invest_real',
                                     'invest_fixed_capital_real', 'export_real', 'import_real',
                                     'export_usd', 'import_usd'),
                          model_ = c('rf', 'boost','lasso', 'ridge',"elnet",
                                    "ar", "arx",
                                    'knn', 'svm', "bagging", 'rw')
){
  # Делает таблицу с результатами теста Диболда-Мариано
  
  df <- out %>%
    mutate(forecastdate = zoo::as.Date(zoo::as.yearqtr(out$date))) %>%
    filter(forecastdate %in% end_testing_dates,
           week_n %in% week_n_,
           model %in% model_
           )
  get.dm <- function(df,
                     week_n_,
                     target_, 
                     model_1,
                     model_2){
    
    df1 <- df %>%
      filter(week_n == week_n_,
             target == target_,
             model == model_1) %>%
      select(date, y_true, y_pred) %>%
      magrittr::set_colnames(c('date','y', 'f1'))
    
    
    df2 <- df %>%
      filter(week_n == week_n_,
             target == target_,
             model == model_2) %>%
      select(date, y_true, y_pred) %>%
      magrittr::set_colnames(c('date','y', 'f2'))
    
    
    df3 <- merge(df1, df2, by =c('date', 'y')) %>% na.omit %>% unique
    
    test_out <- DM.test(df3$f1, df3$f2, df3$y, c = TRUE)
    
    tibble(
      week_n = week_n_,
      target = target_, 
      model_1 = model_1,
      model_2 = model_2,
      n = nrow(df3),
      dm_stat = test_out$statistic,
      pvalue = test_out$p.value
    )
    
    
    
  }
  
  
  dm_res <- expand.grid(week_n =week_n_,
                        target = target, 
                        model_1 = model_,
                        model_2 = model_,
                        stringsAsFactors = FALSE) %>%
    filter(!(target %in% c('export_usd', 'import_usd')& week_n>=18)
    ) %>%
    split(seq(1:nrow(.))) %>%
    map_dfr(function(x){
      get.dm(df = df,
             week_n_ = x$week_n,
             target_ = x$target, 
             model_1 = x$model_1,
             model_2 = x$model_2)})
  dm_res

  
}

dm_res <- make.dm.table()

# dm_res %>% filter(week_n %in% c(-6, 6, 14), model_1 != "elnet", model_2 != "elnet") %>%
#   .[,c("target","week_n","model_1","model_2","dm_stat", "pvalue")] %>%
#   mutate(pvalue = ifelse(pvalue>0.05, NA, 
#                          ifelse(sign(dm_stat)==-1,
#                                 paste0("-", round(pvalue,2)),
#                                 paste0("+", round(pvalue,2))))) %>%
#   select(-dm_stat) %>%
#   mutate(model_1 = model_label_switch(model_1),
#          model_2 = model_label_switch(model_2),
#          target = target_rus_label_switch(target)) %>%
#   dcast(week_n+target+model_1~model_2) %>% export("dm_test.xlsx")

# чем больше статистика диболда--- мариано, тем больше ошибки в первой модели
# знаки поменяли местами, зеленый означает превосходство модели с наукастом, красный - без наукаста
# на уровне значимости 5%
dm_res %>%
  arrange(model_1, model_2) %>%
  filter(week_n %in% c(-10,0,10,20)) %>%
  mutate(sign_color =ifelse(is.na(pvalue),"a",
                      ifelse(pvalue <= 0.05,
                      ifelse(pvalue <= 0.025,
                             ifelse( sign(dm_stat)==1,
                                     'darkgreen','darkred'),
                             ifelse( sign(dm_stat)==1,'green','red') ), "a")),
         model_1 = model_label_switch(model_1),
         model_2 = model_label_switch(model_2),
         target = target_rus_label_switch(target),
         sign = ifelse(sign_color %in% c("green", "darkgreen"),
                       "+",
                       ifelse(sign_color %in% c("red", "darkred"), "-", ""))) %>%
  filter(model_1<=model_2) %>%
  ggplot(aes(y = model_2, x = model_1))+
  geom_tile(show.legend = FALSE, fill="#F8766D00", color = "grey")+
  geom_text(aes(label=sign), size = 10)+
  facet_grid(week_n~target)+
  labs(y = '', x='')+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(angle = 90, size=12))

