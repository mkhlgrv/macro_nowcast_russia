# # daily data----
# days <- seq(from = as.Date('1995-01-01'), to =as.Date('2021-01-18'), by = '1 day')
# # fred 
# sp500 <- import('data/raw data/fred raw.xlsx', sheet=2)
# sp500 <- xts(sp500$`SP500 Close`, order.by = sp500$Date)
# oil <- import('data/raw data/fred raw.xlsx', sheet=3)
# oil <- xts(oil$`Global price of Brent Crude`, order.by = oil$date)
# 
# # cbr
# usd <- import('data/raw data/cbr raw data.xlsx', sheet=1)
# usd <- xts(usd$curs, order.by = usd$data)
# mosprime <- import('data/raw data/cbr raw data.xlsx', sheet=2)
# mosprime <- xts(mosprime$`1 мес.`, order.by = mosprime$Дата)
# saldo <- import('data/raw data/cbr raw data.xlsx', sheet=3)
# saldo <- xts(saldo$Сальдо, order.by = saldo$Дата)
# repo <- import('data/raw data/cbr raw data.xlsx', sheet=4)
# repo <- xts(repo$`Объем задолженности`, order.by = repo$Дата)
# # ставка miacr доступна только на ледующий день,
# # поэтому используем как предиктор ее значение в четверг, а не в пятницу,
# # то есть сместить ее на 1 день
# miacr <- import('data/raw data/cbr raw data.xlsx', sheet=5, skip =2)
# miacr <- xts(miacr$`1 день`, order.by = miacr$Дата)
# miacr[,1] <- lag.xts(miacr[,1], k = 1)
# 
# data_daily <- xts(x=rep(NA, length(days)), order.by = days) %>%
#   merge.xts(sp500, oil, usd, mosprime, saldo, repo, miacr) %>% .[,-1] %>%
#   na.locf()
# 
# data_daily<- data_daily[which(weekdays(time(data_daily), abbreviate = TRUE)=='Пт'),] %>%
#   set_colnames(c('sp500','oil',  'usd', 'mosprime', 'saldo', 'repo', 'miacr'))
# 
# 
# #  weekly----
# # moex
# moex <- import('data/raw data/moex.xlsx')
# moex <- xts(moex[,-1], order.by = as.Date(moex$date))
# # cbr
# fer <- import('data/raw data/cbr raw data.xlsx', sheet=6)
# fer <- xts(fer$Объем, order.by = fer$Дата)
# 
# weekly <- merge.xts(moex, fer, data_daily)%>% na.locf 
# 
# # monthly ----
# # cbr
# money_base <- import('data/raw data/cbr raw data.xlsx', sheet=8)
# money_base <- xts(money_base$`Денежная база (в широком определении)`,
#                   order.by = as.Date(money_base$Дата))
# 
# m2 <- import('data/raw data/cbr raw data.xlsx', sheet=9)
# m2 <- xts(m2$`Денежная масса (М2)`,
#                   order.by = as.Date(m2$Дата))
# # fred
# fred_mon <- import('data/raw data/fred raw.xlsx', sheet =1)
# fred_mon <- xts(fred_mon[,-1], seq(from = as.Date('1995-01-01'),
#                                    length.out = nrow(fred_mon), by = '1 month'))
# 
# # rosstat
# rosstat_mon <- import('data/raw data/rosstat raw data.xlsx', sheet=1, skip=2,)
# rosstat_mon <- xts(rosstat_mon[,-c(1,2)], 
#     order.by = seq(from = as.Date('1991-01-01'),
#                    length.out = nrow(rosstat_mon), by = '1 month'))
# 
# rosstat_survey_mon <- import('data/raw data/rosstat survey raw.xlsx', sheet=1)
# rosstat_survey_mon <- xts(rosstat_survey_mon[,-1], 
#                    order.by = seq(from = as.Date('2006-01-01'),
#                                   length.out = nrow(rosstat_survey_mon), by = '1 month'))
# 
# monthly <- merge.xts(rosstat_survey_mon,
#                      rosstat_mon,
#                      fred_mon,
#                      money_base,
#                      m2)
# 
# 
# # quarterly ----
# rosstat_survey_quart <- import('data/raw data/rosstat survey raw.xlsx', sheet=2)
# rosstat_survey_quart <- xts(rosstat_survey_quart[,-1], 
#                           order.by = seq(from = as.Date('2006-01-01'),
#                                          length.out = nrow(rosstat_survey_quart),
#                                          by = '3 month'))
# 
# 
# rosstat_sns_quart <-import('data/raw data/stsepka_2020q2.xlsx', sheet = 3, skip = 1)
# rosstat_sns_quart <- rosstat_sns_quart[,c(3,5,8, 9,10,11,14,16,19, 20,21,22)]
# 
# rosstat_sns_quart %<>%
#   set_colnames(c('gdp_real','cons_real',
#                  'invest_real',
#                  'invest_fixed_capital_real', 'export_real', 'import_real',
#                  'gdp_nom','cons_nom',
#                  'invest_nom','invest_fixed_capital_nom', 'export_nom', 'import_nom')) %>%
#   xts(x = .,
#       order.by = seq(as.Date('1995-01-01'),
#                      by = 'quarter',
#                      length.out = nrow(.)))
# 
# 
# trade_usd_quart <-import('data/raw data/usd.xlsx', sheet = 1)
# trade_usd_quart <- trade_usd_quart[,-1]
# trade_usd_quart %<>%
#   set_colnames(c( 'export_usd', 'import_usd')) %>%
#   xts(x = .,
#       order.by = seq(as.Date('2005-01-01'),
#                      by = 'quarter',
#                      length.out = nrow(.)))
# quarterly <- merge.xts(rosstat_survey_quart, rosstat_sns_quart, trade_usd_quart)
# 
# # export ----
# 
# 
# for(i in c("cpi" , "ppi" , "cargo_price", 'base_output', 'retail_turn_constant')){
#   last_na <- (which(!is.na(monthly[,i])) %>% first) - 1
#   
#   monthly[(last_na+1):nrow(monthly),i] <-
#     cumprod(monthly[(last_na+1):nrow(monthly),i]/100)
#   
#   
# }
# 
# to_export <- list('weekly'= weekly %>% as.data.frame %>% rownames_to_column('date'),
#                   'monthly'=monthly %>% as.data.frame %>% rownames_to_column('date'),
#                   'quarterly' = quarterly %>% as.data.frame %>% rownames_to_column('date'))
# 
# 
# 
# 
# openxlsx::write.xlsx(to_export, file = "data/raw data/all raw.xlsx")
# transform -----



