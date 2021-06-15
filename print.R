out <- rbind(out1, out2, out3)
out %>%
  filter(
    target %in% c('export_real', 'export_usd'),
         # date >= '2020-01-01'
         ) %>%
  group_by(target) %>%
  # filter(week_n == max(week_n)) %>%
  # filter(date <='2020-03-31') %>%
  dplyr::mutate(Неделя = ifelse(week_n==-10, '-10', ifelse(week_n==22, '22', '-5 - 15'))) %>%
  filter(predictor_group == 'All') %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = y_true))+
  geom_point(aes(y = y_true), size =2, alpha=0.5)+
  geom_point(aes(x=date+2*week_n-22,y = y_pred, color = Неделя), size =3)+
  geom_line(aes(x=date+2*week_n-22,y = y_pred,
                group=interaction(date, model)), alpha = 0.5)+
  labs(y = 'Изменение', x = 'Дата', title = '')+
  facet_grid(target~model, scales = 'free_y')

out_to_score %>%
  mutate(model = ifelse(model == 'elnet',
                        'Эластичная сеть',
                        ifelse(model == 'rf',
                               'Случайный лес',
                               ifelse(model == 'boost',
                                      'Бустинг', ifelse(model == 'rw',
                                                        'Случайное блуждание',model))))) %>%
  mutate(target = ifelse(target == 'gdp_real',
                         'ВВП',
                         ifelse(target == 'cons_real',
                                'Потребление д/х',
                                ifelse(target == 'import_real',
                                       'Импорт', target)))) %>%
  mutate(target = ifelse(target == 'export_real',
                         'Экспорт',
                         ifelse(target == 'export_usd',
                                'Экспорт (USD)',
                                ifelse(target == 'import_usd',
                                       'Импорт (USD)', target)))) %>%
  mutate(target = ifelse(target == 'invest_real',
                         'Инвестиции совокупные',
                         ifelse(target == 'invest_fixed_capital_real',
                                'Инвестиции в основной капитал',target))) %>%
  group_by(model, target, week_n) %>%
  mutate(error = (y_true-y_pred)^2) %>%
  summarise(rmse = sqrt(mean(error))) %>%
  ggplot(aes(x = week_n, y = rmse, color = model))+
  geom_line(aes(group = model), size=1.3)+
  facet_wrap(.~target, scales = 'free_y')+
  labs(y = 'RMSFE', x = 'Неделя', title = '', color = 'Модель')+
  facet_wrap(vars(target), scales = 'free_y')+
  scale_color_manual(breaks = c('Случайное блуждание','Эластичная сеть', 'Случайный лес', 'Бустинг'),
                     values=c("grey", 'cornflowerblue', 'limegreen', 'coral'))+
  theme_bw()+
  theme(legend.position="bottom")


out %>%
  # filter(date <='2020-03-31') %>%
  filter(predictor_group=='All') %>%
  group_by(model, target, week_n) %>%
  mutate(error = (y_true-y_pred)^2) %>%
  summarise(rmse = sqrt(mean(error)))



rbind(out) %>%
  # filter(date <='2020-03-31') %>%
  # filter(predictor_group=='All') %>%
  group_by(model, target, week_n, predictor_group) %>%
  mutate(error = (y_true-y_pred)^2) %>%
  summarise(rmse = sqrt(mean(error))) %>% 
  dcast(target+week_n~model) %>%
  export('chamber/rmse.xlsx')

bold.somerows <- 
  function(x) gsub('BOLD(.*)',paste('\\\\textbf{\\1','}'),x)
table_predictor <- 
  import('data/transformed/desc.xlsx', sheet = 6) %>%
  select('Показатель' = 1,'Частота'=4,'Лаг'=7,'Группа'=9,'Источник'=11,
         'Агрегация'= 12,'Трансформация'= 13) %>%
  xtable()
align(table_predictor) = 'XXXXXXXX' 
table_predictor %>%
  print(include.rownames = FALSE,
                          sanitize.text.function = bold.somerows,
                          tabular.environment='tabularx',
                          width="\\textwidth",
                          file = 'tex/predictors.tex'
)

out_to_score %>%
  group_by(model, target, week_n) %>%
  mutate(error = (y_true-y_pred)^2) %>%
  summarise(rmse = sqrt(mean(error))) %>%
  ggplot(aes(x = week_n, y = rmse, color = model))+
  geom_line(aes(group = model), size =1)+
  facet_wrap(.~target, scales = 'free_y')
  


out_2020 <- rbind(out_2020_1, out_2020_2)


out_2020 %>%
  filter(
    #model == 'elnet',
    target %in% c('gdp_real', 'cons_real', 'import_real'),
    date >= '2020-01-01') %>%
  mutate(target = ifelse(target == 'gdp_real',
                         'ВВП',
                         ifelse(target == 'cons_real',
                                'Потребление д/х',
                                ifelse(target == 'import_real',
                                       'Импорт', NA)))) %>%
  mutate(model = ifelse(model == 'elnet',
                         'Эластичная сеть',
                         ifelse(model == 'rf',
                                'Случайный лес',
                                ifelse(model == 'boost',
                                       'Бустинг', NA)))) %>%
  group_by(target) %>%
  # filter(week_n == max(week_n)) %>%
  # filter(date <='2020-03-31') %>%
  # dplyr::mutate(Неделя = ifelse(week_n==-10, '-10', ifelse(week_n==22, '22', '-5 - 15'))) %>%
  filter(predictor_group == 'All') %>%
  filter(!(target %in% c('export_usd', 'import_usd') & week_n > 15)) %>%
  ggplot(aes(x=date))+
  # geom_hline(aes(yintercept = y_true))+
  geom_segment(aes(x=date, xend = date+90, y = y_true, yend = y_true,
                   color = 'Истинное значение'),
               
               
               size =2,
                alpha=0.5)+
  # geom_point(aes(x=date+90/2.5,y = y_true), size =3, alpha=0.5)+
  # geom_point(aes(y = y_true, x = date+90), size =2, alpha=0.5)+
  geom_point(aes(x=date+week_n*2.5,y = y_pred, shape=model), size =1)+
  # geom_label(aes(x=date+week_n*7,y = y_pred,label =  week_n), size = 0.1)+
  geom_line(aes(x=date+week_n*2.5,y = y_pred,
                group=interaction(date, model)), alpha = 0.5)+
  labs(y = 'Изменение', x = 'Дата', title = '', shape = 'Модель', color='')+
  facet_wrap(vars(target), scales = 'free_y')+
  scale_color_manual(
                     values=c("grey"))+
  theme_bw()+
  theme(legend.position="bottom")




out_2020 %>%
  filter(!target %in% c('gdp_real', 'import_real', 'import_usd', 'invest_real')) %>%
  mutate(forecastdate_factor = ifelse(forecastdate == '2020-01-03', 'I-ый квартал', 
                                      ifelse(forecastdate == '2020-04-03', 'II-ой квартал',
                                      'III-ий квартал'))) %>%
  filter(
    #model == 'elnet',
    #target %in% c('gdp_real', 'cons_real', 'import_real'),
    # date > '2020-03-01',
    # date < '2020-07-01'
    ) %>%
  mutate(target = ifelse(target == 'gdp_real',
                         'ВВП',
                         ifelse(target == 'cons_real',
                                'Потребление д/х',
                                ifelse(target == 'import_real',
                                       'Импорт', target)))) %>%
  mutate(target = ifelse(target == 'export_real',
                         'Экспорт',
                         ifelse(target == 'export_usd',
                                'Экспорт (USD)',
                                ifelse(target == 'import_usd',
                                       'Импорт (USD)', target)))) %>%
mutate(target = ifelse(target == 'invest_real',
                       'Инвестиции совокупные',
                       ifelse(target == 'invest_fixed_capital_real',
                              'Инвестиции в о. к.',target))) %>%
  mutate(model = ifelse(model == 'elnet',
                        'Эластичная сеть',
                        ifelse(model == 'rf',
                               'Случайный лес',
                               ifelse(model == 'boost',
                                      'Бустинг', model)))) %>%
  group_by(target) %>%
  # filter(week_n == max(week_n)) %>%
  # filter(date <='2020-03-31') %>%
  # dplyr::mutate(Неделя = ifelse(week_n==-10, '-10', ifelse(week_n==22, '22', '-5 - 15'))) %>%
  filter(predictor_group == 'All') %>%
  filter(!(target %in% c('export_usd', 'import_usd') & week_n > 15)) %>%
  # filter(target %in% c('ВВП', 'Импорт')) %>%
  ggplot(aes(x=date))+
  # geom_hline(aes(yintercept = y_true))+
  geom_segment(aes(x=date, xend = date+90, y = y_true, yend = y_true,
                   color = 'Истинное значение'),
               
               
               size =2,
               alpha=0.5)+
  # geom_point(aes(x=date+90/2.5,y = y_true), size =3, alpha=0.5)+
  # geom_point(aes(y = y_true, x = date+90), size =2, alpha=0.5)+
  geom_point(aes(x=date+week_n*7,y = y_pred, color=model), size =2)+
  # geom_label(aes(x=date+week_n*7,y = y_pred,label =  week_n), size = 0.1)+
  geom_line(aes(x=date+week_n*7,y = y_pred,
                group=interaction(date, model)), alpha = 0.5)+
  labs(y = 'Изменение', x = 'Дата', title = '', color = '')+
  facet_grid(target~forecastdate_factor, scales = 'free')+
   scale_color_manual(breaks = c('Истинное значение','Эластичная сеть', 'Случайный лес', 'Бустинг'),
     values=c("grey", 'cornflowerblue', 'limegreen', 'coral'))+
  theme_bw()+
  theme(legend.position="bottom")

out %>% filter(target=='import_usd') %>% View


out_to_score %>%
  group_by(target, model, week_n) %>%
  mutate(sef = sd(y_true-y_pred)) %>%
  ggplot(aes(week_n, sef, color=model))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(target), scales = 'free_y')
