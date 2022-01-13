model_group_switch <- Vectorize(vectorize.args = "x",
                                FUN = function(x) {
                                  switch(x, "rf" = "ML",
                                         "boost" = "ML",
                                         "svm"="ML",
                                         "knn"="ML",
                                         "bagging"="ML",
                                         "lasso"="Reg",
                                         "ridge"="Reg",
                                         "elnet"="Reg",
                                         "ar"="AR",
                                         "arx"="AR",
                                         "rw"="AR",
                                         "other")})
model_label_switch <- Vectorize(vectorize.args = "x",
                                FUN = function(x) {
                                  switch(x, "rf" = "RF",
                                         "boost" = "XGboost",
                                         "svm"="SVM",
                                         "knn"="kNN",
                                         "bagging"="Bagging",
                                         "lasso"="LASSO",
                                         "ridge"="Ridge",
                                         "elnet"="EN",
                                         "ar"="AR",
                                         "arx"="ARX",
                                         "rw"="RW",
                                         "other")})

target_label_switch <- Vectorize(vectorize.args = "x",
                                FUN = function(x) {
                                  switch(x, "cons_real" = "Consumption",
                                         "export_real" = "Export",
                                         "export_usd"="Export (USD)",
                                         "gdp_real"="GDP",
                                         "import_real"="Import",
                                         "import_usd"="Import (USD)",
                                         "invest_fixed_capital_real"="Investment to FC",
                                         "invest_real"="Investment",
                                         "export_and_stocks"="Export&Stocks",
                                         "other")})

target_rus_label_switch <- Vectorize(vectorize.args = "x",
                                 FUN = function(x) {
                                   switch(x, "cons_real" = "Потребление",
                                          "export_real" = "Экспорт",
                                          "export_usd"="Экспорт (долл.)",
                                          "gdp_real"="ВВП",
                                          "import_real"="Импорт",
                                          "import_usd"="Импорт (долл.)",
                                          "invest_fixed_capital_real"="ВНОК",
                                          "invest_real"="Валовое накопление",
                                          "export_and_stocks"="Экспорт и инвестиции в запасы",
                                          "other")})
