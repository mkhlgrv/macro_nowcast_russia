# Расчет значений теста ДМ
make.dm.table <- function(end_testing_dates = seq(as.Date("2015-01-01"),as.Date("2021-04-01"), by = "quarter"),
                          week_n_ = seq(-10,22,by = 4 ),
                          target = c('gdp_real','cons_real',
                                     'invest_real',
                                     'invest_fixed_capital_real', 'export_real', 'import_real',
                                     'export_usd', 'import_usd'),
                          model_ = c('rf', 'boost','lasso', 'ridge',
                                    "ar", "arx",
                                    'knn', 'svm', "bagging", 'rw')
){
  # Делает таблицу с результатами теста Диболда-Мариано
  
  df <- out %>%
    mutate(forecastdate = zoo::as.Date(zoo::as.yearqtr(out$forecastdate))) %>%
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
    filter(!(target %in% c('export_usd', 'import_usd')& week_n==22)
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



dm_res2 <- bind_cols(dm_res[,1:3] %>%
                       magrittr::set_colnames(c('week_n', 'target', 'model'))%>%
                       magrittr::set_colnames(c('week_n', 'target', 'model_1')),
                     dm_res[,c(2,4)] %>%
                       magrittr::set_colnames(c('target','model')) %>%
                       select(2)%>%
                       magrittr::set_colnames(c('model_2')),
                     dm_res[,5:7]
)
# чем больше статистика диболда--- мариано, тем больше ошибки в первой модели
# знаки поменяли местами, зеленый означает превосходство модели с наукастом, красный - без наукаста
# на уровне значимости 5%
dm_res2 %>%
  na.omit %>%
  # mutate(target = ifelse(target == 'gdp_real',
  #                        'ВВП',
  #                        ifelse(target == 'cons_real',
  #                               'Потребление д/х',
  #                               ifelse(target == 'import_real',
  #                                      'Импорт', target)))) %>%
  # mutate(target = ifelse(target == 'export_real',
  #                        'Экспорт',
  #                        ifelse(target == 'export_usd',
  #                               'Экспорт (USD)',
  #                               ifelse(target == 'import_usd',
  #                                      'Импорт (USD)', target)))) %>%
  # mutate(target = ifelse(target == 'invest_real',
  #                        'Инвестиции совокупные',
  #                        ifelse(target == 'invest_fixed_capital_real',
  #                               'Инвестиции в осн-ой капитал',target))) %>%
  # mutate(model_1 = ifelse(model_1 == 'elnet',
  #                       'ЭC',
  #                       ifelse(model_1 == 'rf',
  #                              'СЛ',
  #                              ifelse(model_1 == 'boost',
  #                                     'Б', ifelse(model_1 == 'rw',
  #                                                 'СБ',model_1))))) %>%
  # mutate(model_2 = ifelse(model_2 == 'elnet',
  #                       'ЭС',
  #                       ifelse(model_2 == 'rf',
  #                              'СЛ',
  #                              ifelse(model_2 == 'boost',
  #                                     'Б', ifelse(model_2 == 'rw',
  #                                                 'СБ',model_2))))) %>%
  arrange(model_1) %>%
  # filter(grepl('В', predictor_group_1)&grepl('В', predictor_group_2)) %>%
  #mutate(sign =ifelse(pvalue <= 0.05, ifelse( sign(dm_stat)==1,'green','red'), NA))  %>%
  mutate(sign =ifelse(pvalue <= 0.05,
                      ifelse(pvalue <= 0.025, ifelse( sign(dm_stat)==1,'darkgreen','darkred'),ifelse( sign(dm_stat)==1,'green','red') ), NA))  %>%
  ggplot(aes(y = model_2, x = model_1))+
  geom_tile(aes(fill = sign),show.legend = FALSE, color='grey')+
  facet_grid(week_n~target)+
  labs(y = '', x='')+
  scale_fill_manual(values = c(#'darkgreen',
    #'darkred' #,
    '#71d466',
    '#d9454d' ,
    "#add1a9",
    '#db696f' #,
    #"#2bd918",
    # '#d60f1a',
    #'white'
  ))+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(angle = 90, size=8))


