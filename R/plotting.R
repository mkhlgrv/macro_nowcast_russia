predictors <- import('data/transformed/desc.xlsx') %>% select(2,4,7, 9)
res <- seq(-4, 28, by = 1) %>%
  map_dfr(function(week_n){
    1:nrow(predictors) %>% 
    map_dfr(function(j){
      n <- get.available.lags(predictors$freq[j], predictors$lag[j], week_n+5) %>%
        length
      tibble(week_n = week_n,
             freq = predictors$freq[j],
             n = n, 
             group = predictors$`Группа (широкая)`[j])
    })

})

res %>%
  group_by(week_n, freq, group) %>%
  filter(group!='Финансовые') %>%
  summarise(n = sum(n)) %>% 
  ungroup %>%
  group_by(group, freq) %>%
  arrange(week_n) %>%
  mutate(n = diff.xts(n)) %>%
  ggplot(aes(x =week_n,y=n, fill = group))+
  geom_bar(stat="identity", position = position_dodge())
