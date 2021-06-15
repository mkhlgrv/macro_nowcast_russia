dates <- tibble(date = seq.Date(as.Date("1992-07-03"), today(), by="7 days"))

list(
  
  weekly=cbind(import("C:/Users/mkhlgrv/Documents/forecast/database/binded/binded d.xlsx", guess_max=5000) %>%
          select(-brent_urals_spread) %>%
          arrange(date) %>%
          reshape2::melt(id.vars="date") %>%
          mutate(yearmon = as.yearmon(date)) %>%
          group_by(yearmon, variable) %>%
          mutate(value = cummean(value)) %>%
          ungroup %>%
          right_join(dates, by ="date") %>%
          group_by(variable)%>%
          na.locf() %>%
          reshape2::dcast(date~variable) %>%
          inner_join(dates, by ="date") %>%
          arrange(date)
        
        ,
        
        import("C:/Users/mkhlgrv/Documents/forecast/database/binded/binded w.xlsx") %>%
          reshape2::melt(id.vars="date") %>%
          right_join(dates, by ="date") %>%
          group_by(variable) %>%
          na.locf %>%
          inner_join(dates, by ="date") %>%
          reshape2::dcast(date~variable) %>%
          select(-date))%>%
    mutate(date = as.Date(date)),
  # raw_m
  monthly = import("C:/Users/mkhlgrv/Documents/forecast/database/binded/binded m.xlsx") %>%
    mutate(date = as.Date(date)),
  
  quarterly = import("C:/Users/mkhlgrv/Documents/forecast/database/binded/binded q.xlsx")%>%
    mutate(date = as.Date(date))
) %>%
  export("chamber/data/raw data/all raw.xlsx")




