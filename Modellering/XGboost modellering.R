#_________________________________ Innehåll _________________________________
# 1. Laddar paket och data
# ___________________________________________________________________________
# 2. Parametertuning med datamaterial för olika 
# - 2.1 Funktion för att hitta mest optimala hyperparametrar för alla
#       driftområden och prognoshorisonter
# ___________________________________________________________________________
# 3.Skapar modeller utifrån hyperparametrarna 
# - 3.1 Skattar modeller och för varje driftområde och prognoshorsiont
# ___________________________________________________________________________
# 4. Beräknar RMSE för träningsmängd och valideringsmängd
# - 4.1 RMSE för träningsmängd
# - 4.2 RMSE för valideringsmängd
# - 4.3 RMSE för testmängden
# __________________________________________________________________________
# 5. Beräkar MAE för samtliga mängder
# __________________________________________________________________________
# 6. Skapar prediktioner för de driftområden som hade XGBoost som bäsa modell
# ___________________________________________________________________________


#### 1. Laddar paket och data ####
# Laddar paket
load_package<-function(){
  paket<- c("dplyr","tsibble", "tibble", "lubridate", "xgboost", "caret",
            "plyr","ggplot2","forecast","fastDummies","data.table")        # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])] # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                 # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()

day_data<-read.csv("day_data.csv", fileEncoding = "Windows-1252")

# Laddar in XGBoost modeller om koden körts tidigare
#load("XGBoost_models.RData")


#### 2. Parametertuning med datamaterial för olika prognoshorisonter ####
#### 2.1 - Funktion för att hitta mest optimala hyperparametrar  ####
param_drift <- function() {
  # Skapar en grid med olika kombinationer av värden för hyperparametrarna
  param_grid <- expand.grid(eta=seq(0,1,by=0.2),   # Kan sättas mellan 0 och 1
                            gamma=seq(2,12,by=2),  # Kan sättas från 0 till onändligheten, desto större värde desto mer konservativ
                            max_depth=c(3,5,10,20),# Kan sättas från 0 till oändligheten, desto större värde desto mer konservativ
                            lambda=seq(1,10,by=2)) # L2 regularisering, desto större värde desto mer konservativ
  
  # Skapar en tom matris att lägga in RMSE i tillsammans med hyperparametrarna 
  rmse_param <- data.frame(Rmse=rep(0,nrow(param_grid)))
  rmse_param <- cbind(rmse_param,param_grid)
  
  # Skapar en tom lista
  final_param_horiz <- list()
  final_param_drift <- list()
  
  for (j in 1:length(unique(day_data$Driftområde))) {
    # Filtrerar ut ett specifikt driftområde
    drift <- gsub("/","_",unique(day_data$Driftområde)[j])
    
    for (k in 1:14) {
      filter_data_train <- list_train[[k]] %>% filter(Driftområde==drift)
      filter_data_val <- list_val[[k]] %>% filter(Driftområde==drift)
      # Plockar ut de förklarande variablerna
      train_x <- data.matrix(filter_data_train[, -1:-4])
      # Plockar ut responsvariabeln
      train_y <- data.matrix(filter_data_train[,4])
      # Skapar matriser för data
      val_x <- data.matrix(filter_data_val[, -1:-4])
      val_y <- data.matrix(filter_data_val[,4])
      # Skapar XGB matriser
      xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
      xgb_val <- xgb.DMatrix(data = val_x, label = val_y)
      # Skapar watchlist, denna används i xgb.train 
      watchlist <- list(train = xgb_train, test = xgb_val)
      
      # Loopar över alla kombinationer av hyperparametrar
      for (i in 1:nrow(param_grid)) {
        # Lista med specifika hyperparametrar
        param <- list(eta = param_grid[i,1],
                    gamma = param_grid[i,2],
                    max_depth = param_grid[i,3],
                    lambda = param_grid[i,4])
        
        #Skattar XGboost med de olika hyperparametrarna
        model <- xgb.train(data = xgb_train,watchlist=watchlist,
                           nrounds = 100, objective = "count:poisson",
                           params = param,
                           eval_metric = "rmse",
                           early.stop.round = 10,
                           verbose = 0)
        
        # Hittar det minsta RMSE för valideringsmängden
        test_rmse <- model$evaluation_log[,3]
        index_min <- which.min(test_rmse$test_rmse)
        # Lägger till det minsta RMSE för valideringsmängden med tillhörande parameterkombination
        rmse_param[i,1] <- test_rmse$test_rmse[index_min] 
        print(i)
      }
      
      # Tar fram lägsta RMSE och tillhörande parametrar av alla kombinationer för ett sprcifikt driftområde och progrnoshorisont
      index_min_final <- which.min(rmse_param$Rmse)
      final_mod <- rmse_param[index_min_final,]
      
      # Skapar en lista med den bästa kombinationen av hyperparametrar
      final_param <- list(eta = final_mod[1,2],
                          gamma = final_mod[1,3],
                          max_depth = final_mod[1,4],
                          lambda = final_mod[1,5])
      
      final_param_horiz[[k]] <- final_param
      names(final_param_horiz)[k] <- paste0("Prognoshorisont_",k)
  }
    final_param_drift[[j]] <- final_param_horiz
    names(final_param_drift)[[j]] <- drift
}
  final_param_drift <<- final_param_drift
}

param_drift()
final_param_drift

# Sparar resultaten
#save(final_param_drift, file="XGBoost_Parameter.RData")
#write.csv(final_param_drift, ".../XGBoostParameter.csv", row.names = FALSE)


#### 3.Skapar modeller utifrån hyperparametrarna ####
#### 3.1 - Skattar modeller och för varje driftområde och prognoshorsiont ####
load("XGBoost_Parameter.RData")

XG_model_skatt <- function() {
  moder<- list()
  dotter <- list()
  for (i in 1:9) {
    drift <- gsub("/","_",unique(day_data$Driftområde)[i])

    for (j in 1:14) {
      filter_data_train <- list_train[[j]] %>% filter(Driftområde==drift)
      train_x <- data.matrix(filter_data_train[, -1:-4]) # Tar bort den sista kolumnen då detta är responsvariabeln
      train_y <- data.matrix(filter_data_train[,4])
      xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
      final <- xgboost(data = xgb_train,
                       params = final_param_drift[[i]][[j]],
                       nrounds = 1000, 
                       verbose = 2,
                       objective = "count:poisson",
                       eval_metric="rmse",
                       early_stopping_rounds = 20)
      dotter[[j]] <- final
      names(dotter)[j] <- paste0("Prognos_",j)
    }
    moder[[i]] <- dotter
   names(moder)[i] <- drift 
  }
  moder <<- moder
}
XG_model_skatt()




#### 4. Beräknar RMSE för träningsmängd och valideringsmängd ####
#### 4.1 RMSE för träningsmängd ####
Xg_RMSE_train <- function(){
  t <- 0
  RMSE_drift_train <- data.frame(Horiz = rep(1:14,9),
                               RMSE = rep(rep(0,14),9),
                               Model = "XGBoost",
                               Driftområde = rep(0,126))
  
  # Loopar över varje driftområde och gör prediktioner, samt beräknar RMSE
  
  for (i in 1:9) {
    drift <- gsub("/","_",unique(day_data$Driftområde)[i])
    for (j in 1:14) {
      # Index för att loopa över olika prognoshorisonter 
      t <- t+1
      # Skapar prediktioner för olika driftområden för att kunna ta fram specfika RMSE
      # Gör ett datamaterial med förklaringsvariabler och en med responsvariabel för träningsdata
      val_drift_x <- data.matrix(list_train[[j]][list_train[[j]]$Driftområde==drift,-1:-4])
      val_drift_y <- data.matrix(list_train[[j]][list_train[[j]]$Driftområde==drift,4])
      xgb_val_drift <- xgb.DMatrix(data = val_drift_x, label = val_drift_y)
      pred_y <- round(predict(moder[[i]][[j]], xgb_val_drift),0)
      # Tar fram RMSE
      RMSE_drift_train[t,2] <- caret::RMSE(val_drift_y, pred_y)
      RMSE_drift_train[t,4] <- gsub("_","/",drift)
    }
    
  }
  RMSE_drift_train <<- RMSE_drift_train
}
Xg_RMSE_train()
#write.csv(Xg_RMSE_H(),".../RMSE_XGBoost_train.csv", row.names = FALSE)





#### 4.2 RMSE för valideringsmängd ####
Xg_RMSE_val <- function(){
  t <- 0
  RMSE_drift_val <- data.frame(Horiz = rep(1:14,9),
                             RMSE = rep(rep(0,14),9),
                             Model = "XGBoost",
                             Driftområde = rep(0,126))
  
  # Loopar över varje driftområde och gör prediktioner, samt beräknar RMSE
  for (i in 1:9) {
    drift <- gsub("/","_",unique(day_data$Driftområde)[i])
    for (j in 1:14) {
      # Index för att loopa över olika prognoshorisonter 
      t <- t+1
      # Skapar prediktioner för olika driftområden för att kunna ta fram specfika RMSE
      # Gör ett datamaterial med förklaringsvariabler och en med responsvariabel för valieringsdata
      val_drift_x <- data.matrix(list_val[[j]][list_val[[j]]$Driftområde==drift,-1:-4])
      val_drift_y <- data.matrix(list_val[[j]][list_val[[j]]$Driftområde==drift,4])
      xgb_val_drift <- xgb.DMatrix(data = val_drift_x, label = val_drift_y)
      pred_y <- round(predict(moder[[i]][[j]], xgb_val_drift),0)
      # Tar fram RMSE
      RMSE_drift_val[t,2] <- caret::RMSE(val_drift_y, pred_y)
      RMSE_drift_val[t,4] <- gsub("_","/",drift)
    }
    
  }
  RMSE_drift_val <<- RMSE_drift_val
}
Xg_RMSE_val()
#write.csv(Xg_RMSE_H(),".../RMSE_XGBoost_val.csv", row.names = FALSE)





#### 4.3 Beräkar RMSE för testmängden ####
Xg_RMSE_test <- function(){
  t<- 0
  RMSE_XG_test <- data.frame(Horiz = rep(1:14,9),
                               RMSE = rep(rep(0,14),9),
                               Model = "XGBoost",
                               Driftområde = rep(0,126))
  
  # Loopar över varje driftområde och gör prediktioner, samt beräknar RMSE
  for (i in 1:9) {
    drift <- gsub("/","_",unique(day_data$Driftområde)[i])
    for (j in 1:14) {
      # Index för att loopa över olika prognoshorisonter 
      t <- t+1
      # Skapar prediktioner för olika driftområden för att kunna ta fram specfika RMSE
      # Gör ett datamaterial med förklaringsvariabler och en med responsvariabel för valieringsdata
      test_drift_x <- data.matrix(list_test[[j]][list_test[[j]]$Driftområde==drift,-1:-4])
      test_drift_y <- data.matrix(list_test[[j]][list_test[[j]]$Driftområde==drift,4])
      xgb_test_drift <- xgb.DMatrix(data = test_drift_x, label = test_drift_y)
      pred_y <- round(predict(moder[[i]][[j]], xgb_test_drift),0)
      # Tar fram RMSE
      RMSE_XG_test[t,2] <- caret::RMSE(test_drift_y, pred_y)
      RMSE_XG_test[t,4] <- gsub("_","/",drift)
    }
    
  }
  RMSE_XG_test <<- RMSE_XG_test
}
Xg_RMSE_test()
#write.csv(RMSE_XG_test,".../RMSE_XGBoost_test.csv", row.names = FALSE)



#### 5. Beräkar MAE för samtliga mängder ####
Xg_MAE_test <- function(){
  datas <- c("val", "test", "train")
  for (DaMa in 1:length(datas)){
    data <- datas[DaMa]
    if (datas == "val"){
      list <- list_val
    }
    if (datas == "test"){
      list <- list_test
    }
    if (datas == "train"){
      list <- list_train
    }
    t <- 0
    MAE_drift_test <- data.frame(Horiz = rep(1:14,9),
                                 MAE = rep(rep(0,14),9),
                                 Model = "XGBoost",
                                 Driftområde = rep(0,126))
    
    # Loopar över varje driftområde och gör prediktioner, samt beräknar MAE
    for (i in 1:9) {
      drift <- gsub("/","_",unique(day_data$Driftområde)[i])
      for (j in 1:14) {
        # Index för att loopa över olika prognoshorisonter 
        t <- t+1
        # Skapar prediktioner för olika driftområden för att kunna ta fram specfika MAE
        # Gör ett datamaterial med förklaringsvariabler och en med responsvariabel för valieringsdata
        drift_x <- data.matrix(list[[j]][list[[j]]$Driftområde==drift,-1:-4])
        drift_y <- data.matrix(list[[j]][list[[j]]$Driftområde==drift,4])
        xgb_drift <- xgb.DMatrix(data = drift_x, label = drift_y)
        pred_y <- round(predict(moder[[i]][[j]], xgb_drift),0)
        # Tar fram RMSE
        MAE_drift_test[t,2] <- caret::MAE(drift_y, pred_y)
        MAE_drift_test[t,4] <- gsub("_","/",drift)
      }
      
    }
    MAE_drift_test <<- MAE_drift_test
  }
}
Xg_MAE_test()
#write.csv(Xg_MAE_test(),".../MAE_XGBoost_test.csv", row.names = FALSE)



#### 6. Skapar prediktioner för de driftområden som hade XGBoost som bäsa modell ####
# Driftområden som har XGBoost som bästa modell
drift_xgboost <- c(unique(day_data$Driftområde)[2],
                   unique(day_data$Driftområde)[5],
                   unique(day_data$Driftområde)[6],
                   unique(day_data$Driftområde)[7],
                   unique(day_data$Driftområde)[9])

# Skapar en tom lista att lägga in prediktioner i 
progs_XGBoost <- list()

# Loopar över att skapa prediktioner för de fem driftområdena
for (i in 1:5) {
  drift <- gsub("/","_",drift_xgboost[i])
  pred_y_1 <- data.frame(Datum = list_test[[1]][1:14,3],
                         pred = rep(0,14),
                         true = rep(0,14))
  
  pred_y_2 <- data.frame(Datum = list_test[[1]][15:28,3],
                         pred = rep(0,14),
                         true = rep(0,14))
  for (j in 1:14) {
    # Index för att loopa över olika prognoshorisonter 
    # Skapar prediktioner för olika driftområden för att kunna ta fram specfika RMSE
    # Gör ett datamaterial med förklaringsvariabler och en med responsvariabel för valieringsdata
    # Skapar prognoser  mellan 2 september till 15 september
    test_drift_x_1 <- data.matrix(list_test[[j]][list_test[[j]]$Driftområde==drift,-1:-4][1,])
    test_drift_y_1 <- data.matrix(list_test[[j]][list_test[[j]]$Driftområde==drift,4][1])
    xgb_test_drift_1 <- xgb.DMatrix(data = test_drift_x_1, label = test_drift_y_1)
    pred_y_1[j,2] <- round(predict(moder[[i]][[j]], xgb_test_drift_1),0)
    pred_y_1[j,3] <- test_drift_y_1
    # Skapar prognoser mellan 16 september till 29 september
    test_drift_x_2 <- data.matrix(list_test[[j]][list_test[[j]]$Driftområde==drift,-1:-4][15,])
    test_drift_y_2 <- data.matrix(list_test[[j]][list_test[[j]]$Driftområde==drift,4][15])
    xgb_test_drift_2 <- xgb.DMatrix(data = test_drift_x_2, label = test_drift_y_2)
    pred_y_2[j,2] <- round(predict(moder[names(moder)==drift][[1]][[j]], xgb_test_drift_2),0)
    pred_y_2[j,3] <- test_drift_y_2
  }
  progs_XGBoost[[i]] <- list("14_1"=pred_y_1,
                             "14_2"=pred_y_2)
  names(progs_XGBoost)[i] <- drift
}
#save(progs_XGBoost, file="progs_XGBoost.RData")
