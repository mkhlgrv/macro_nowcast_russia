collect_jobs_out <- function(out_import = "out", out_export = "",target = c("gdp_real","cons_real",
                                                                           "invest_real",
                                                                           "invest_fixed_capital_real",
                                                                           "export_real",
                                                                           "import_real",
                                                                           "export_usd", "import_usd"),
                             week_n = seq(-10,22,by = 4 ),
                             end_testing_dates, different_lag=TRUE){
  colnames_to_bind <- c("date","y_true","y_pred", "week_n", "model", "target")
  
  tmp <- make.rw.out(end_testing_dates_start = end_testing_dates[1],
                     end_testing_dates_end = end_testing_dates[length(end_testing_dates)],
                     week_n= week_n,
                     target = target, different_lag= different_lag)[,colnames_to_bind]
  for(filei in list.files(path = out_import, pattern = ".Rdata", full.names = TRUE)){
    load(filei)
    tmp <- rbind(tmp, out[,colnames_to_bind])
  }
  out <- tmp
  save(out, file=paste0(out_export,"out.Rdata"))
  
  
}
