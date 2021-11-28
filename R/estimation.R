
# source("R/import.R")
# import.data()
# source("R/stationarize.R")
# stationarize.data()
source("R/fun.R")
source("R/lib.R")
source("R/run_job.R")
# run.job()
source("R/utils.R")
collect_jobs_out(out_import = "out/")
load("out.Rdata")
# make.dm.table(week_n = 22, model = c("rf", "boost"),
#               end_testing_dates = seq(as.Date("2015-01-01"),
#                                       as.Date("2020-04-01"), by = "quarter"))
source("R/rmse.R")
make.rmse.table(relative_to_rw=FALSE, week_n_ = c(-6,6, 18)) %>% reshape2::dcast(target+week_n~model) %>% na.omit
model_group_switch <- Vectorize(vectorize.args = "x",
                 FUN = function(x) {
                   switch(x, "rf" = "ML",
                          "boost" = "ML",
                          "svm"="ML",
                          "knn"="ML",
                          "bagging"="ML",
                          "lasso"="Reg",
                          "ridge"="Reg",
                          "elnet"="Reg",
                          "ar"="AR",
                          "arx"="AR",
                          "rw"="AR")})


make.rmse.table(cumulative_rmse = TRUE, relative_to_rw = FALSE) %>%
  filter( week_n %in% c(-10)) %>%
  mutate(model_group = model_group_switch(model)) %>%
  ggplot(aes(x = date,y=rmse, group = model, color = model_group))+
  geom_line()+
  facet_wrap(target~week_n, scales = "free_y")


out %>% filter(date >= "2020-01-01")%>% ggplot(aes(x= date, y = y_true))+geom_point()
# 
# 
# out %>% 
#   #filter(date != '2020-04-03') %>%
#   rbind(prevyear) %>%
#   arrange(date) %>% 
#  # filter(date != max(date)) %>%
#   #filter(predictor_group=='non_financial') %>%
#   group_by(week_n, model, target, predictor_group) %>% 
#   mutate(direction_true = sign(y_true-lag.xts(y_true)),
#          direction_pred = sign(y_pred-lag.xts(y_true)),
#          error = abs(direction_true - direction_pred)/2) %>%  
#    # summarise(rmse = sqrt(mean((y_true - y_pred)^2))) %>%
#   summarise(rmse = mean(error, na.rm = TRUE)) %>%
#   select(week_n, model, rmse, target, predictor_group) %>%
#   dcast(target+week_n+predictor_group~model, value.var = 'rmse') %>% View#  export('chamber/rmse.xlsx')
# 
# out %>%  filter(#model == 'elnet',
#          predictor_group=='All'
#          , target %in% c('gdp_real')
#          ) %>%
#   dplyr::mutate(Неделя = ifelse(week_n==-4, '-4', ifelse(week_n==28, '28', '0-22'))) %>%
#   #filter(date != max(date)) %>%
#   ggplot(aes(x=date))+
#   geom_line(aes(y = y_true))+
#   geom_point(aes(y = y_true), size =2, alpha=0.5)+
#   geom_point(aes(x=date+week_n-28,y = y_pred, color = Неделя), size =3)+
#   geom_line(aes(x=date+week_n-28,y = y_pred,
#                 group=interaction(date, predictor_group, model)))+
#   labs(y = 'Изменение', x = 'Дата', title = '')+
#   facet_grid(model~., scales = 'free_y')
# 
# out %>%
#   filter(model == 'elnet',
#          predictor_group=='All',
#          target %in% c('cons_real')) %>%
#   dplyr::mutate(Неделя = ifelse(week_n==-4, '-4', ifelse(week_n==28, '28', '0-22'))) %>%
#   #filter(date != max(date)) %>%
#   ggplot(aes(x=date))+
#   geom_line(aes(y = y_true))+
#   geom_point(aes(y = y_true), size =2, alpha=0.5)+
#   geom_point(aes(x=date+week_n-28,y = y_pred, color = Неделя), size =3)+
#   geom_line(aes(x=date+week_n-28,y = y_pred,
#                 group=interaction(date, predictor_group, model)))+
#   labs(y = 'Изменение', x = 'Дата', title = '')
# # facet_wrap(vars(target), scales = 'free_y')
# 
# 
# 
# out %>%
#   filter(model == 'elnet',
#          predictor_group=='All',
#          target %in% c('export_real')) %>%
#   dplyr::mutate(Неделя = ifelse(week_n==-4, '-4', ifelse(week_n==28, '28', '0-22'))) %>%
#   #filter(date != max(date)) %>%
#   ggplot(aes(x=date))+
#   geom_line(aes(y = y_true))+
#   geom_point(aes(y = y_true), size =2, alpha=0.5)+
#   geom_point(aes(x=date+week_n-28,y = y_pred, color = Неделя), size =3)+
#   geom_line(aes(x=date+week_n-28,y = y_pred,
#                 group=interaction(date, predictor_group, model)))+
#   labs(y = 'Изменение', x = 'Дата', title = '')
# 
# 
# res <- out %>% filter(model == 'rf',
#                predictor_group=='All',
#                target %in% c('gdp_real'), week_n == 20)
# 
# lm(y_true~y_pred,data = res) %>% summary
# sqrt(mean((y_true[81:102]-y_true[80:101])^2))
# sqrt(mean((y_true[81:101]-y_true[80:100])^2))
