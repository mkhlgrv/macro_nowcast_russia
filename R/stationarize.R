
stationarize.data<- function(desc_path="data/desc.xlsx",
                              import_path = "data/raw_data.xlsx",
                              export_path = "data/stationary_data.Rdata"){
  agg_info <- rio::import(desc_path)[c('name', 'aggregation', 'stationarity')]
  pub_lags <- rio::import(desc_path)[c('name', 'lag')]
  
  stat_data <- 1:3 %>% 
    purrr::map_dfr(function(i){
    
    allraw <- rio::import(file = import_path, sheet = i)

    
    d1 <- allraw %>% 
      mutate(date = as.Date(date) %>% get.next.weekday(6),
             quarter_first_friday = zoo::as.yearqtr(date) %>% as.Date %>% get.next.weekday(6),
             h = as.numeric(date - quarter_first_friday)/7) %>%
      reshape2::melt(id.vars=c('date','quarter_first_friday','h' )) %>%
      inner_join(pub_lags[c('name', 'lag')], by = c('variable'='name'))
    
    
    
    if(i==2){
      d1 %<>% mutate(h = ifelse(h %in% c(4,8), h +1, h))
    }
    d2 <- d1 %>%
      inner_join(agg_info[c('name', 'aggregation')],
                 by = c('variable'='name')) %>%
      group_by(quarter_first_friday, variable) %>% 
      mutate(value = ifelse(aggregation=='last', value,
                            ifelse(aggregation=='mean', cummean(value),
                                   ifelse(aggregation=='sum', cumsum(value), NA))))
    
    quarter_end_data <- d2 %>% group_by(quarter_first_friday, variable) %>%
      filter(h == max(h)) %>%
      mutate(next_year_quarter_first_friday = zoo::as.yearqtr(quarter_first_friday +390) %>%
               as.Date%>%
               get.next.weekday(6)) %>%
      ungroup %>%
      select(quarter_first_friday=next_year_quarter_first_friday, variable, 
             value_to_frac = value) %>%
      unique
    
    
    
    d3 <- d2 %>%
      inner_join(quarter_end_data, by =c('quarter_first_friday', 'variable')) %>%
      inner_join(agg_info[c('name', 'stationarity')],
                 by = c('variable'='name')) %>%
      mutate(value = ifelse(stationarity==0, value,
                            ifelse(stationarity==1, value - value_to_frac,
                                   ifelse(stationarity==2, log(value) - log(value_to_frac), NA)))) %>%
      ungroup()
    
    d3[,c('quarter_first_friday', 'h', 'variable', 'value')]
    
    
  })
  
  save(stat_data, file=export_path)
  
}



