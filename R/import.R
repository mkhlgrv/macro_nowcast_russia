library(dplyr)


# Загружает данные из базы данных в нужный формат (эксель из трех колонок)
import.data <- function(desc_path="data/desc.xlsx",
                        db_path = "../rmedb_usage/data/tf/",
                        db_format=".csv",
                   export_path = "data/raw_data.xlsx"){
  tickers <- rio::import(desc_path)[,c("name", "freq")]
  
  weeks <- tibble(date = seq.Date(as.Date("1992-07-03"), lubridate::today(), by="7 days"))
  
  cbind.data.by.freq <- function(freq_){
    freq_ <- substring(freq_, 1, 1)
    
    purrr::map(tickers$name[tickers$freq==freq_],
               function(namei){
        rio::import(paste0(db_path, namei, db_format))[,c("date", "value")] %>%
                   mutate(date = as.Date(date)) %>%
                   arrange(date) %>%
          magrittr::set_colnames(c("date", namei)) %>%
                   (
                     function(x){ if(freq_=="w"){
                       x %>%
                         dplyr::right_join(weeks, by ="date") %>%
                         zoo::na.locf() %>%
                         dplyr::inner_join(weeks, by ="date")
                     } else {
                       x
                     }
                     }
                   )

          }) %>%
        plyr::join_all(by = "date")
  }
  
  list_names <- c("weekly", "montly", "quarterly")
  purrr::map(list_names,cbind.data.by.freq) %>%
    magrittr::set_names(list_names) %>%
    rio::export(export_path,overwrite=TRUE)

}



