rm(list = ls())
source("fun.R")
source('lib.R')
plan(multisession(workers = 4))
load("data/stat_data.Rdata")
st <- Sys.time()
try({
  out <- list()
  for(iter in 1:24){
    out[[iter]] <- expand.grid(
      model = c('rf', 'boost','lasso', 'ridge',
                'knn', 'svm', "bagging"),
      target = c('gdp_real','cons_real',
                 'invest_real',
                 'invest_fixed_capital_real', 'export_real', 'import_real', 'export_usd', 'import_usd'),
      i= iter,
      
      week_n =seq(-10,22,by = 4 ),
      # test
      # model = c( 'svm', "bagging"),
      # target = c('cons_real'),
      # i= c(1:24),
      # week_n =c(-10, 0, 16),
      # predictor_group = c('All'),
      stringsAsFactors = FALSE) %>%
      filter(!(target %in% c('export_usd', 'import_usd') & week_n > 15)) %>%
      # sample_n(size = 1) %>%
      split(seq(1:nrow(.))) %>%
      future_map_dfr(function(x){
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
    message(paste0(Sys.time(),": ",
        iter, "/24"))
  }
  out <- do.call(rbind, out)
  save(out, file="out.Rdata")
  
  

}, silent = FALSE,
outFile = "out.txt")

diff_time <- Sys.time()-st

bot <- Bot(Sys.getenv("telegram_api_key"))

chat_id <- "269425799"

bot$sendMessage(chat_id,
                text = paste0("Вычисления закончились. Время начала: " ,st,
                              ". Время окончания: ",
                              Sys.time()
                              ),
                parse_mode = "Markdown"
)
try({
  bot$sendDocument(chat_id,
                   document = "out.txt")
})

try({
out$target %>% unique() %>%
  walk(function(targeti){
    rmse_rw_full <- import('data/rmse rw.xlsx') %>%
      filter(target ==targeti) %>% pull(rmse_all)
    rmse_rw_19 <- import('data/rmse rw.xlsx') %>%
      filter(target ==targeti) %>% pull(rmse_19)
    cairo_pdf(paste0("plot/",targeti, ".pdf"), width = 15, height = 15, onefile = TRUE)
    print(out %>%
            filter(target ==targeti) %>%
            ggplot()+
            geom_segment(aes(x=date, xend = date+90, y = y_true, yend = y_true), color="black", size =1) +
            geom_line(aes(x = date + week_n*2.5+25, y = y_pred, group=forecastdate), color ="cornflowerblue")+
            geom_point(aes(x = date + week_n*2.5+25, y = y_pred, group=forecastdate), color ="cornflowerblue")+
            facet_wrap(vars(model))+
            labs(y = "value", title = targeti, subtitle = "nowcast")+
            theme_minimal()
    )

    
    
    print(out %>%
            filter(target ==targeti) %>%
            group_by(model, week_n, target) %>%
            summarise(rmse = mean((y_true-y_pred)^2)) %>%
            ggplot(aes(x = week_n, y = rmse))+
            geom_bar(stat = "identity", fill='cornflowerblue', position = position_dodge())+
            geom_abline(slope = 0, intercept = rmse_rw_full, color = "darkgreen", size =1)+
            facet_wrap(vars(model))+
            labs(y = "rmse", title = targeti, subtitle = "RMSE (2015Q1-2020Q4)")+
            theme_minimal())
    
    
    print(out %>%
            filter(date < "2020-01-01") %>%
            filter(target ==targeti) %>%
            group_by(model, week_n, target) %>%
            summarise(rmse = mean((y_true-y_pred)^2)) %>%
            ggplot(aes(x = week_n, y = rmse), fill='cornflowerblue')+
            geom_bar(stat = "identity", fill='cornflowerblue', position = position_dodge())+
            geom_abline(slope = 0, intercept = rmse_rw_19, color = "darkgreen", size =1)+
            facet_wrap(vars(model))+
            labs(y = "rmse", title = targeti, subtitle = "RMSE (2015Q1-2019Q4)")+
            theme_minimal())
    dev.off()
    bot$sendDocument(chat_id,
                     document = paste0("plot/",targeti, ".pdf")
    )
  })


})

