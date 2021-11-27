run.job <- function(job_name = "Job 1",
                    start_training_date = as.Date("2007-04-01"),
                    end_training_dates = seq(as.Date("2014-10-01"),as.Date("2021-01-01"), by = "quarter"),
                    end_testing_dates = seq(as.Date("2015-01-01"),as.Date("2021-04-01"), by = "quarter"),
                    date_sample = NULL,
                    model = c("rf", "boost","lasso", "ridge",
                              "ar", "arx",
                              "knn", "svm", "bagging"),
                    target = c("gdp_real","cons_real",
                               "invest_real",
                               "invest_fixed_capital_real",
                               "export_real",
                               "import_real",
                               "export_usd", "import_usd"),
                    week_n = seq(-10,22,by = 4 ),
                    seed = 123,
                    use_bot=TRUE,
                    send_all_messages = TRUE,
                    out_path = "out/"){
  
  if(!is.null(date_sample)){
    if(length(date_sample)>length(end_training_dates)|
       length(date_sample)>length(end_testing_dates)){
      stop()
    } else{
      end_training_dates <- end_training_dates[date_sample]
      end_testing_dates <- end_testing_dates[date_sample]
    }

  }

  source("R/fun.R")
  source("R/lib.R")
  
  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  load("data/stationary_data.Rdata", envir = global_env())
  st <- Sys.time()
  st_formatted <- format(st, format = "%Y_%m_%d_%H_%M_%S")
  
  bot <- Bot(Sys.getenv("telegram_api_key"))
  
  chat_id <- Sys.getenv("computation_bot_chat_id")
  
  log_file <- tempfile(fileext = ".txt")
  
  try({
    set.seed(seed)
    
    len <- length(end_testing_dates)
    out <- list()
    for(iter in 1:len){
        out[[iter]] <- expand.grid(
          model = model,
          target = target,
          i= iter,
          
          week_n =week_n,
          stringsAsFactors = FALSE) %>%
          
          filter(!(target %in% c("export_usd", "import_usd") & week_n > 15)) %>%
          # dplyr::sample_n(size = 1) %>%
          split(seq(1:nrow(.))) %>%

          purrr::map_dfr(function(x){
           
            train_datei <- end_training_dates[x$i]
            test_datei <- end_testing_dates[x$i]
            print(test_datei)
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

        message_text <- paste0(Sys.time(),": ",
                               iter, "/", len)
        if(use_bot&send_all_messages){
          bot$sendMessage(chat_id, text = paste0(job_name,": ", iter, "/", len))
        }
        message(message_text)

      
    }
    out <- do.call(rbind, out)
    save(out, file=paste0(out_path, "out_",st_formatted,".Rdata"))
    
    
  }, 
  silent = FALSE,
  outFile = log_file)
  
  if(use_bot){
    diff_time <- Sys.time()-st
    

    
    bot$sendMessage(chat_id,
                    text = paste0(job_name, ": ","evaluation started at " ,format(st, format = "%H:%M:%S"),
                                  ", ended at ",
                                  format(Sys.time(), format = "%H:%M:%S")
                    ),
                    parse_mode = "Markdown"
    )
    try(bot$sendDocument(chat_id, document = log_file))
    
    
  }

}

