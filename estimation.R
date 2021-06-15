




# 2007Q3
start_training_date <- as.Date('2007-04-01')
# 2014Q4 -- 2020Q1
end_training_dates <- seq(as.Date('2014-10-01'),as.Date('2020-07-01'), by = 'quarter')
# 2015Q1 -- 202Q2
end_testing_dates <- seq(as.Date('2015-01-01'),as.Date('2020-10-01'), by = 'quarter')

train.model()
  
out <- expand.grid(model = c('rf', 'boost', 'elnet'),
            target = c('gdp_real','cons_real',
                       'invest_real',
                       'invest_fixed_capital_real', 'export_real', 'import_real',
                       'gdp_nom','cons_nom',
                       'invest_nom','invest_fixed_capital_nom', 'export_nom', 'import_nom',
                       'export_usd', 'import_usd'),
            i= c(1:22),
            week_n =c(-4, 0, 10,  20, 28),
            predictor_group = c('All', 'non_financial'),
            stringsAsFactors = FALSE
            
              )%>% 
  split(seq(1:nrow(.))) %>%
  map_dfr(function(x){
    train_datei <- end_training_dates[x$i]
    test_datei <- end_testing_dates[x$i]
    train.model(end_training_date = train_datei,
                       testing_date = test_datei,
                model = x$model,
                target = x$target,
                week_n = x$week_n) %>%
      mutate(week_n = x$week_n,
             model = x$model,
             target = x$target,
             predictor_group=x$predictor_group)
  })


train.model(model ="rf" ,
            target = 'gdp_real',
            week_n = 0,
            end_training_date = '2020-01-01',
            testing_date = '2020-04-01')

out <- rbind(out1, out2, out3) #%>%
  # filter(!target %in% c(
  #   'export_real', 'import_real', 'export_nom', 'import_nom',
  #   'export_usd', 'import_usd'
  # ))
out <- do.call(rbind, list(out,
                           out3,
                           out4 %>% filter(week_n !=15),
                           out5,
                           out6 %>% filter(week_n !=15),
                           out7))

save(out, file = 'chamber/out.RData')

year14 <- import('data/transformed/quarterly.xlsx') %>%
  filter(date == '2014-10-01')
colnames(year14[-c(1:4)]) %>% length()
as.numeric(year14[-c(1:4)])

prevyear <- tibble(startdt = as.Date('2007-04-06'),
       enddt = as.Date('2007-04-06'),
       forecastdate=as.Date('2007-04-06'),
       date = as.Date('2014-10-01'),
       y_true = rep(as.numeric(year14[-c(1:4)]), 6*3),
       y_pred = NA,
       week_n = rep(c(-4, 0, 10, 15, 20, 28), 14*3),
       model = rep(c('rf', 'boost', 'elnet'), 6*14),
       target = rep(colnames(year14[-c(1:4)]), 6*3),
       predictor_group = 'All'
       
       )
out %>% View
out %>% 
  #filter(date != '2020-04-03') %>%
  rbind(prevyear) %>%
  arrange(date) %>% 
 # filter(date != max(date)) %>%
  #filter(predictor_group=='non_financial') %>%
  group_by(week_n, model, target, predictor_group) %>% 
  mutate(direction_true = sign(y_true-lag.xts(y_true)),
         direction_pred = sign(y_pred-lag.xts(y_true)),
         error = abs(direction_true - direction_pred)/2) %>%  
   # summarise(rmse = sqrt(mean((y_true - y_pred)^2))) %>%
  summarise(rmse = mean(error, na.rm = TRUE)) %>%
  select(week_n, model, rmse, target, predictor_group) %>%
  dcast(target+week_n+predictor_group~model, value.var = 'rmse') %>% View#  export('chamber/rmse.xlsx')

out %>%  filter(#model == 'elnet',
         predictor_group=='All'
         , target %in% c('gdp_real')
         ) %>%
  dplyr::mutate(Неделя = ifelse(week_n==-4, '-4', ifelse(week_n==28, '28', '0-22'))) %>%
  #filter(date != max(date)) %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = y_true))+
  geom_point(aes(y = y_true), size =2, alpha=0.5)+
  geom_point(aes(x=date+week_n-28,y = y_pred, color = Неделя), size =3)+
  geom_line(aes(x=date+week_n-28,y = y_pred,
                group=interaction(date, predictor_group, model)))+
  labs(y = 'Изменение', x = 'Дата', title = '')+
  facet_grid(model~., scales = 'free_y')

out %>%
  filter(model == 'elnet',
         predictor_group=='All',
         target %in% c('cons_real')) %>%
  dplyr::mutate(Неделя = ifelse(week_n==-4, '-4', ifelse(week_n==28, '28', '0-22'))) %>%
  #filter(date != max(date)) %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = y_true))+
  geom_point(aes(y = y_true), size =2, alpha=0.5)+
  geom_point(aes(x=date+week_n-28,y = y_pred, color = Неделя), size =3)+
  geom_line(aes(x=date+week_n-28,y = y_pred,
                group=interaction(date, predictor_group, model)))+
  labs(y = 'Изменение', x = 'Дата', title = '')
# facet_wrap(vars(target), scales = 'free_y')



out %>%
  filter(model == 'elnet',
         predictor_group=='All',
         target %in% c('export_real')) %>%
  dplyr::mutate(Неделя = ifelse(week_n==-4, '-4', ifelse(week_n==28, '28', '0-22'))) %>%
  #filter(date != max(date)) %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = y_true))+
  geom_point(aes(y = y_true), size =2, alpha=0.5)+
  geom_point(aes(x=date+week_n-28,y = y_pred, color = Неделя), size =3)+
  geom_line(aes(x=date+week_n-28,y = y_pred,
                group=interaction(date, predictor_group, model)))+
  labs(y = 'Изменение', x = 'Дата', title = '')


res <- out %>% filter(model == 'rf',
               predictor_group=='All',
               target %in% c('gdp_real'), week_n == 20)

lm(y_true~y_pred,data = res) %>% summary
sqrt(mean((y_true[81:102]-y_true[80:101])^2))
sqrt(mean((y_true[81:101]-y_true[80:100])^2))
