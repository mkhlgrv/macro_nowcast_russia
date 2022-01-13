
# source("R/import.R")
# import.data()
# source("R/stationarize.R")
# stationarize.data()
source("R/fun.R")
source("R/lib.R")
source("R/run_job.R")
# run.job()
source("R/utils.R")
collect_jobs_out(out_import = "out/",
                 week_n = c(-10, -6, -5, -2, 0, 2, 5, 6, 10, 14, 15, 18, 20, 22),
                 end_testing_dates= seq(as.Date("2015-01-01"),
                                                             
                                                             as.Date("2021-04-01"),
                                                             by = "quarter"))
load("out.Rdata")
# make.dm.table(week_n = 22, model = c("rf", "boost"),
#               end_testing_dates = seq(as.Date("2015-01-01"),
#                                       as.Date("2020-04-01"), by = "quarter"))
source("R/rmse.R")
make.rmse.table(relative_to_rw=TRUE,
                week_n_ = c(-10,0,10,20),dcast=FALSE) %>%
  mutate(model = model_label_switch(model),
         target = target_rus_label_switch(target),
         rmse = round(rmse, 2)) %>%
  dcast(target+week_n~model) %>%
  dplyr::rename(Переменная=target) %>%
  export("tab_rmse.xlsx", overwrite = TRUE) # %>%.[.$target %in% c("export_and_stocks", "export_real","invest_real"),]
make.rmse.table(end_testing_dates =seq(as.Date("2015-01-01"),as.Date("2019-01-01"), by = "quarter"),
                relative_to_rw=TRUE,
                week_n_ = c(-10,0,10,20),dcast=FALSE) %>%
  mutate(model = model_label_switch(model),
         target = target_rus_label_switch(target),
         rmse = round(rmse, 2)) %>%
  dcast(target+week_n~model) %>%
  dplyr::rename(Переменная=target) %>%
  export("tab_rmse_19.xlsx", overwrite = TRUE) 
  

library(hrbrthemes)
cairo_pdf("plot/cumulative_rmse.pdf", width =10, height = 16)
make.rmse.table(target = c('gdp_real','cons_real',
                           'invest_real',
                           'invest_fixed_capital_real', 'export_real', 'import_real',
                           'export_usd', 'import_usd'),
                relative_to_rw=FALSE,dcast = FALSE,cumulative_rmse = TRUE,
                week_n_ = c(-10,0,10,20)) %>%
  filter(target != "export_and_stocks", date >="2015-01-01") %>%
  mutate(model_group = model_group_switch(model),
         target = target_rus_label_switch(target),
         facet_var = paste0(target, " (",week_n, ")")) %>%

  ggplot(aes(x = date,y=rmse))+
  stat_summary(
    fun = min,
    geom = "line",
    linetype = 2
  )+
  stat_summary(
    fun = max,
    geom = "line",
    linetype = 2
  )+
stat_summary(
  fun = median, 
  geom = "line"
)+
  facet_grid(target~week_n, scales="free_y")+
  labs(y="RMSFE", x = "Дата")+
  theme_minimal()+ theme(legend.position="bottom",
                         legend.title=element_blank(),
                         axis.text.y = element_text(size=12),
                         axis.text.x = element_text(size=12))
dev.off()

cairo_pdf("plot/forecast.pdf", width =16, height = 14)
out %>%
  filter(date >= "2015-01-01", 
         (target %in% c("cons_real")&model %in% c("svm"))
         ) %>%
  mutate(
         model_group = model_group_switch(model),
              target = target_rus_label_switch(target),
              facet_var = paste0(target, " (",week_n,")"))  %>%
  filter() %>%
  ggplot()+
  geom_segment(aes(x= date,
                   xend = date+90,
             y = y_true,
             yend = y_true), color = "black",size=1)+
  stat_summary(aes(x = date+900/32 + week_n*90/32,
                   y = y_pred,
                   group = date),
    fun = min, 
    geom = "line"
  )+
  stat_summary(aes(x = date+900/32 + week_n*90/32,
                   y = y_pred,
                   group = date),
               fun = max, 
               geom = "line"
  )+
  facet_wrap(vars(target), ncol = 1, scales = "free_y")+
  theme_minimal()+
  labs(x = "Дата", y = "Изменение")
  scale_color_ft()
dev.off()

cairo_pdf("plot/rmse.pdf", width =16, height = 14)

make.rmse.table(relative_to_rw=FALSE,
                week_n = c(-10, -6, -5, -2, 0, 2, 5, 6, 10, 14, 15, 18, 20, 22),
                target = c('gdp_real','cons_real',
                           'invest_real',
                           'invest_fixed_capital_real', 'export_real', 'import_real',
                           'export_usd', 'import_usd'),
                dcast = FALSE) %>%
  filter(target!="export_and_stocks") %>%
  mutate(
    model_group = model_group_switch(model),
    target = target_rus_label_switch(target))  %>%

  ggplot()+
  stat_summary(aes(x=week_n, y = rmse, linetype=model_group),
               fun = median, geom = "line", size = 0.8)+
  facet_wrap(vars(target), ncol = 4, scales="free_y")+
  labs(linetype="Группа\nмоделей", y = "RMSFE", x = TeX("Неделя"))+
  theme_minimal()
dev.off()