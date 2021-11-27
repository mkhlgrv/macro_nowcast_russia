
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
            geom_text(aes(label = paste0(round(rmse/rmse_rw_full*100,1), " %")),
                      stat = "identity",position = position_dodge(width = 1),
                      vjust = -0.5, size = 2)+
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
            geom_text(aes(label = paste0(round(rmse/rmse_rw_19*100,1), " %")),
                      stat = "identity",position = position_dodge(width = 1),
                      vjust = -0.5, size = 2)+
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

