#____________________ Innehåll ____________________
# 1. Laddar paket och data
# _________________________________________________
# 2. Bearbetar data
# - 2.1 Allmänt
# - 2.2 För ARIMA-modeller
# - 2.3 Dummys för veckodag
# - 2.4 Dummys för månader
# - 2.5 Dummys för månader och veckodagar
# _________________________________________________
# 3. Skattar modeller
# - 3.1 ARIMA
# - 3.2 Dummys för veckodag
# - 3.3 Dummys för månader
# - 3.4 Dummys för månader och veckodagar
# _________________________________________________
# 4. Sparar alla ARIMA modeller
# _________________________________________________
# 5. Beräknar RMSE
# - 5.1 För valideringsmängden
# - 5.2 För träningsmängden
# - 5.3 För testmängden
# _________________________________________________
# 6. Residualanalys
# _________________________________________________
# 7. Beräknar MAE för samtliga mängder och modeller
# _________________________________________________

#### 1. Laddar paket och data ####
# Laddar in paket
load_package <- function(){
  paket<- c("fpp3", "forecast", "ggpubr", "ggplot2","tsibbledata","tsbox") # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])] # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                 # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()
# Laddar in data
day_data <- read.csv("day_data.csv", fileEncoding = 'Windows-1252')

# Om koden körts tidigare kan alla modeller laddas in:
#load("ARIMA_models.RData")

#### 2. Databearbetning ####
#### 2.1 Allmänt ####
# Tar fram antalet ärenden per driftområde och dag.
drift_day <- aggregate(day_data$Antal, by=list(day_data$Driftområde, day_data$Tid), FUN=sum)
colnames(drift_day) <-c ("Driftområde", "Datum", "Antal")
drift_day$Datum <- as.Date(drift_day$Datum, format="%Y-%m-%d")

# Datum för respektive mängd
# 62 månader träning
start_train_date <- "2015-11-02"
end_train_date <- "2020-11-30"

# 9 månader validering
start_val_date <- "2020-12-01"
end_val_date <- "2021-08-31"

# 3 månader testmängd
start_test_date <- "2021-09-01"
end_test_date <- "2021-12-01"


#### 2.2 För ARIMA-modeller ####
# Skapar objekt - för alla obs
drift_day_tib <- as_tsibble(drift_day, key = Driftområde)
# Skapar objekt - för träning
train <- drift_day_tib %>% filter(Datum >= start_train_date & Datum <= end_train_date) 
# Skapar objekt - för validering
val <- drift_day_tib %>% filter(Datum >= start_val_date & Datum <= end_val_date)
# Skapar objekt - för test
test <- drift_day_tib %>% filter(Datum >= start_test_date & Datum <= end_test_date)


#### 2.3 Dummys för veckodag ####
# Skapar objekt - alla obs
drift_day_dummyD <- drift_day_tib %>% 
  mutate(var=1, Veckodag=wday(Datum, label=TRUE, week_start=1)) %>%
  spread(key=Veckodag,value=var, fill=0) %>%
  dplyr::select(Driftområde, Datum, Antal, tis, ons, tor, fre, lör, sön)
# Skapar objekt - för träning
train_dummyD <- drift_day_dummyD %>% filter(Datum >= start_train_date & Datum <= end_train_date) 
# Skapar objekt - för validering
val_dummyD <- drift_day_dummyD %>% filter(Datum >= start_val_date & Datum <= end_val_date)
# Skapar objekt - för test
test_dummyD <- drift_day_dummyD %>% filter(Datum >= start_test_date & Datum <= end_test_date)


#### 2.4 Dummys för månader ####
drift_day_dummyM <- drift_day_tib %>% 
  mutate(var1=1,
         Månad=month(Datum, label=TRUE)) %>%
  spread(key=Månad, value=var1, fill=0) %>%
  dplyr::select(Driftområde, Datum, Antal,
                feb, mar, apr, maj, jun, jul, aug, sep, okt, nov, dec)

# Skapar objekt - för träning
train_dummyM <- drift_day_dummyM %>% filter(Datum >= start_train_date & Datum <= end_train_date) 
# Skapar objekt - för validering
val_dummyM <- drift_day_dummyM %>% filter(Datum >= start_val_date & Datum <= end_val_date)
# Skapar objekt - för test
test_dummyM <- drift_day_dummyM %>% filter(Datum >= start_test_date & Datum <= end_test_date)


#### 2.5 Dummys för månader och veckodagar ####
# Skapar objekt - alla obs
drift_day_dummyDM <- drift_day_tib %>% 
  mutate(var1=1,
         var2=1,
         Veckodag=wday(Datum, label=TRUE, week_start=1),
         Månad=month(Datum, label=TRUE)) %>%
  spread(key=Veckodag,value=var1, fill=0) %>%
  spread(key=Månad, value=var2, fill=0) %>%
  dplyr::select(Driftområde, Datum, Antal, tis, ons, tor, fre, lör, sön,
                feb, mar, apr, maj, jun, jul, aug, sep, okt, nov, dec)

# Skapar objekt - för träning
train_dummyDM <- drift_day_dummyDM %>% filter(Datum >= start_train_date & Datum <= end_train_date) 
# Skapar objekt - för validering
val_dummyDM <- drift_day_dummyDM %>% filter(Datum >= start_val_date & Datum <= end_val_date)
# Skapar objekt - för test
test_dummyDM <- drift_day_dummyDM %>% filter(Datum >= start_test_date & Datum <= end_test_date)





#### 3. Skattar modeller ####
### 3.1 ARIMA ####
ARIMA_modeller <- train %>% model(ARIMA=ARIMA(Antal, stepwise=FALSE))

#### 3.2 Dummys för veckodag ####
ARIMA_modeller_dummyD <- train_dummyD %>%
  model(ARIMA_veckodagar=ARIMA(Antal~tis+ons+tor+fre+lör+sön, stepwise=FALSE))

#### 3.3 Dummys för månader ####
ARIMA_modeller_dummyM <- train_dummyM %>%
  model(ARIMA_månader=ARIMA(Antal~feb+mar+apr+maj+jun+jul+aug+sep+okt+nov+dec,
                            stepwise=FALSE))

#### 3.4 Dummys för månader och veckodagar ####
ARIMA_modeller_dummyDM <- train_dummyDM %>%
  model(ARIMA_veckodagar_månader=ARIMA(Antal~tis+ons+tor+fre+lör+sön+feb+mar+apr+maj+jun+jul+aug+sep+okt+nov+dec,
                                       stepwise=FALSE))


#### 4. Sparar alla ARIMA modeller ####
#save(ARIMA_modeller, ARIMA_modeller_dummyD, ARIMA_modeller_dummyM, ARIMA_modeller_dummyDM, file = "ARIMA_models.RData")

#### 5. Beräknar RMSE ####
#### 5.1 Beräknar RMSE för valideringsmängden ####
RMSE_ARIMA_funk_val <- function(H=14){
  for (D in 1:length(unique(day_data$Driftområde))) {
    drift <- unique(day_data$Driftområde)[D]
    # Lista över modellerna
    Models<-c("ARIMA", "ARIMA_D","ARIMA_DM", "ARIMA_M")
    
    # Väljer modell och valideringsmängd uppdelat på typ av modell
    for(M in 1:length(Models)){
      Model <- Models[M]
      if (Model=="ARIMA"){
        model_temp <- ARIMA_modeller
        val_temp <- val
      }
      if (Model=="ARIMA_D"){
        model_temp <- ARIMA_modeller_dummyD
        val_temp <- val_dummyD
      }
      if (Model=="ARIMA_M"){
        model_temp <- ARIMA_modeller_dummyM
        val_temp <- val_dummyM
      }
      if (Model=="ARIMA_DM"){
        model_temp <- ARIMA_modeller_dummyDM
        val_temp <- val_dummyDM
      }
      # Plockar fram modellen (M) för driftområdet (D)
      val_fit_temp <- model_temp %>% filter(Driftområde==drift) %>% refit(val_temp)
      
      # Plockar fram Valideringsmängden drifrområdet (D)
      val_temp <- val_temp %>% filter(Driftområde==drift)
      
      # Skapar en tom matris
      res_mat <- matrix(0,nrow = nrow(val_temp), ncol = H)
      colnames(res_mat) <- paste0("h_",1:H)
      
      # Tar fram skattade värden för varje prognosghorisont (H) för en modell (M) för ett driftområde (D)
      for(i in 1:H){
        # Tar fram skattade värden för horisonten i
        fit_predict_temp <- fitted(val_fit_temp, h = i)
        # Sätter negativa skattade värden till 0
        fit_predict_temp$.fitted[fit_predict_temp$.fitted<0] <- 0
        # Avrundar till heltal
        res_mat[,i] <- round(fit_predict_temp$.fitted,0)
      }
      
      # Skattade värden för varje pronoshorisont (H) för en modell (M) för ett driftområde (D)
      pred_all_H <- as.data.frame(res_mat)
      
      # Skapar en tom data.frame för RMSE för en modell (M) för ett driftområde (D)
      RMSE_h_frame <- data.frame(Horiz=1:H, RMSE=rep(0,H), Model=Model, Driftområde=drift)
      
      # Beräknar RMSE för varje prognoshorisont (H) för en modell (M) för ett driftområde (D)
      for(i in 1:H){
        RMSE_h_frame[i,2] <- sqrt(mean((pred_all_H[,i]-val_temp$Antal)^2, na.rm=TRUE))
      }
      # Tilldelar RMSE för en modell (M)
      assign(paste0("RMSE", M), RMSE_h_frame)
    }
    
    # Slår samman RMSE för varje modell (M)
    res <- rbind(RMSE1, RMSE2, RMSE3, RMSE4)
    # Tilldelar en modells (M) RMSE till ett driftområde (D)
    assign(paste0("res",D), res)
    
  }
  # Sparar resultatden från varje driftområde (D) till en data.frame
  result_val <<- rbind(res1, res2, res3, res4, res5, res6, res7, res8, res9)
}

#RMSE_ARIMA_funk()
#write.csv(RMSE_ARIMA_funk_val(), ".../RMSE_ARIMA_val.csv", row.names = FALSE)

#### 5.2 Beräknar RMSE för träningsmängden #### 
RMSE_ARIMA_funk_train <- function(H=14){
  for (D in 1:length(unique(day_data$Driftområde))) {
    drift <- unique(day_data$Driftområde)[D]
    # Lista över modellerna
    Models<-c("ARIMA", "ARIMA_D","ARIMA_DM", "ARIMA_M")
    
    # Väljer modell och träningsmängd uppdelat på typ av modell
    for(M in 1:length(Models)){
      Model <- Models[M]
      if (Model=="ARIMA"){
        model_temp <- ARIMA_modeller
        train_temp <- train
      }
      if (Model=="ARIMA_D"){
        model_temp <- ARIMA_modeller_dummyD
        train_temp <- train_dummyD
      }
      if (Model=="ARIMA_M"){
        model_temp <- ARIMA_modeller_dummyM
        train_temp <- train_dummyM
      }
      if (Model=="ARIMA_DM"){
        model_temp <- ARIMA_modeller_dummyDM
        train_temp <- train_dummyDM
      }
      # Plockar fram modellen (M) för driftområdet (D)
      train_fit_temp <- model_temp %>% filter(Driftområde==drift) %>% refit(train_temp)
      
      # Plockar fram träningsmängden drifrområdet (D)
      train_temp <- train_temp %>% filter(Driftområde==drift)
      
      # Skapar en tom matris
      res_mat <- matrix(0, nrow = nrow(train_temp), ncol = H)
      colnames(res_mat) <- paste0("h_",1:H)
      
      # Tar fram skattade värden för varje prognosghorisont (H) för en modell (M) för ett driftområde (D)
      for(i in 1:H){
        # Tar fram skattade värden för horisonten i
        fit_predict_temp <- fitted(train_fit_temp, h = i)
        # Sätter negativa skattade värden till 0
        fit_predict_temp$.fitted[fit_predict_temp$.fitted<0] <- 0
        # Avrundar till heltal
        res_mat[,i] <- round(fit_predict_temp$.fitted,0)
      }
      
      # Skattade värden för varje pronoshorisont (H) för en modell (M) för ett driftområde (D)
      pred_all_H <- as.data.frame(res_mat)
      
      # Skapar en tom data.frame för RMSE för en modell (M) för ett driftområde (D)
      RMSE_h_frame <- data.frame(Horiz=1:H, RMSE=rep(0,H), Model=Model, Driftområde=drift)
      
      # Beräknar RMSE för varje prognoshorisont (H) för en modell (M) för ett driftområde (D)
      for(i in 1:H){
        RMSE_h_frame[i,2] <- sqrt(mean((pred_all_H[,i]-train_temp$Antal)^2, na.rm=TRUE))
      }
      # Tilldelar RMSE för en modell (M)
      assign(paste0("RMSE", M), RMSE_h_frame)
    }
    
    # Slår samman RMSE för varje modell (M)
    res <- rbind(RMSE1, RMSE2, RMSE3, RMSE4)
    # Tilldelar en modells (M) RMSE till ett driftområde (D)
    assign(paste0("res",D), res)
    
  }
  # Sparar resultatden från varje driftområde (D) till en data.frame
  result_train <<- rbind(res1, res2, res3, res4, res5, res6, res7, res8, res9)
}
#write.csv(RMSE_ARIMA_funk_train(), ".../RMSE_ARIMA_train.csv", row.names = FALSE)

#### 5.3 Beräknar RMSE för testmängden #### 
RMSE_ARIMA_funk_test <- function(H=14){
  for (D in 1:length(unique(day_data$Driftområde))) {
    drift <- unique(day_data$Driftområde)[D]
    # Lista över modellerna
    Models<-c("ARIMA", "ARIMA_D","ARIMA_DM", "ARIMA_M")
    
    # Väljer modell och träningsmängd uppdelat på typ av modell
    for(M in 1:length(Models)){
      Model <- Models[M]
      if (Model=="ARIMA"){
        model_temp <- ARIMA_modeller
        train_temp <- test
      }
      if (Model=="ARIMA_D"){
        model_temp <- ARIMA_modeller_dummyD
        train_temp <- test_dummyD
      }
      if (Model=="ARIMA_M"){
        model_temp <- ARIMA_modeller_dummyM
        train_temp <- test_dummyM
      }
      if (Model=="ARIMA_DM"){
        model_temp <- ARIMA_modeller_dummyDM
        train_temp <- test_dummyDM
      }
      # Plockar fram modellen (M) för driftområdet (D)
      train_fit_temp <- model_temp %>% filter(Driftområde==drift) %>% refit(train_temp)
      
      # Plockar fram träningsmängden drifrområdet (D)
      train_temp <- train_temp %>% filter(Driftområde==drift)
      
      # Skapar en tom matris
      res_mat <- matrix(0, nrow = nrow(train_temp), ncol = H)
      colnames(res_mat) <- paste0("h_",1:H)
      
      # Tar fram skattade värden för varje prognosghorisont (H) för en modell (M) för ett driftområde (D)
      for(i in 1:H){
        # Tar fram skattade värden för horisonten i
        fit_predict_temp <- fitted(train_fit_temp, h = i)
        # Sätter negativa skattade värden till 0
        fit_predict_temp$.fitted[fit_predict_temp$.fitted<0] <- 0
        # Avrundar till heltal
        res_mat[,i] <- round(fit_predict_temp$.fitted,0)
      }
      
      # Skattade värden för varje pronoshorisont (H) för en modell (M) för ett driftområde (D)
      pred_all_H <- as.data.frame(res_mat)
      
      # Skapar en tom data.frame för RMSE för en modell (M) för ett driftområde (D)
      RMSE_h_frame <- data.frame(Horiz=1:H, RMSE=rep(0,H), Model=Model, Driftområde=drift)
      
      # Beräknar RMSE för varje prognoshorisont (H) för en modell (M) för ett driftområde (D)
      for(i in 1:H){
        RMSE_h_frame[i,2] <- sqrt(mean((pred_all_H[,i]-train_temp$Antal)^2, na.rm=TRUE))
      }
      # Tilldelar RMSE för en modell (M)
      assign(paste0("RMSE", M), RMSE_h_frame)
    }
    
    # Slår samman RMSE för varje modell (M)
    res <- rbind(RMSE1, RMSE2, RMSE3, RMSE4)
    # Tilldelar en modells (M) RMSE till ett driftområde (D)
    assign(paste0("res",D), res)
    
  }
  # Sparar resultatden från varje driftområde (D) till en data.frame
  result_test <<- rbind(res1, res2, res3, res4, res5, res6, res7, res8, res9)
}
#write.csv(RMSE_ARIMA_funk_test(), ".../RMSE_ARIMA_test.csv", row.names = FALSE)


### 6. Residualanalys ####

LB_ARIMA <- augment(ARIMA_modeller) %>% features(.innov, ljung_box, lag = 50, dof=0)
LB_ARIMA_df <- data.frame(Driftområde=LB_ARIMA$Driftområde,
                       P_value=round(LB_ARIMA$lb_pvalue,2))

LB_ARIMA_D <- augment(ARIMA_modeller_dummyD) %>% features(.innov, ljung_box, lag = 50, dof=6)
LB_ARIMA_D_df <- data.frame(Driftområde=LB_ARIMA_D$Driftområde,
                          P_value=round(LB_ARIMA_D$lb_pvalue,2))

LB_ARIMA_M <- augment(ARIMA_modeller_dummyM) %>% features(.innov, ljung_box, lag = 50, dof=11)
LB_ARIMA_M_df <- data.frame(Driftområde=LB_ARIMA_M$Driftområde,
                            P_value=round(LB_ARIMA_M$lb_pvalue,2))

LB_ARIMA_DM <- augment(ARIMA_modeller_dummyDM) %>% features(.innov, ljung_box, lag = 50, dof=17)
LB_ARIMA_DM_df <- data.frame(Driftområde=LB_ARIMA_DM$Driftområde,
                            P_value=round(LB_ARIMA_DM$lb_pvalue,2))

# Skapar tabeller med p-värden
xtable(LB_ARIMA_df)
xtable(LB_ARIMA_D_df)
xtable(LB_ARIMA_M_df)
xtable(LB_ARIMA_DM_df)

### 7. Beräknar MAE för samtliga mängder och modeller ####
MAE_ARIMA_funk <- function(H=14){
  datas <- c("val", "test", "train")
  models <- c("ARIMA", "ARIMA_D","ARIMA_DM", "ARIMA_M")
  drifts <- unique(day_data$Driftområde)
  
  for (DaMa in 1:length(datas)){
    data <- datas[DaMa]
    for (D in 1:length(drifts)) {
      drift <- drifts[D]
      for(M in 1:length(models)){
        model <- models[M]
        
        if (data=="val"){
          if (model=="ARIMA"){
            model_temp <- ARIMA_modeller
            data_temp <- val
          }
          if (model=="ARIMA_D"){
            model_temp <- ARIMA_modeller_dummyD
            data_temp <- val_dummyD
          }
          if (model=="ARIMA_M"){
            model_temp <- ARIMA_modeller_dummyM
            data_temp <- val_dummyM
          }
          if (model=="ARIMA_DM"){
            model_temp <- ARIMA_modeller_dummyDM
            data_temp <- val_dummyDM
          }
        }
        
        if (data=="test"){
          if (model=="ARIMA"){
            model_temp <- ARIMA_modeller
            data_temp <- test
          }
          if (model=="ARIMA_D"){
            model_temp <- ARIMA_modeller_dummyD
            data_temp <- test_dummyD
          }
          if (model=="ARIMA_M"){
            model_temp <- ARIMA_modeller_dummyM
            data_temp <- test_dummyM
          }
          if (model=="ARIMA_DM"){
            model_temp <- ARIMA_modeller_dummyDM
            data_temp <- test_dummyDM
          }
          
        }
        if (data=="train"){
          if (model=="ARIMA"){
            model_temp <- ARIMA_modeller
            data_temp <- train
          }
          if (model=="ARIMA_D"){
            model_temp <- ARIMA_modeller_dummyD
            data_temp <- train_dummyD
          }
          if (model=="ARIMA_M"){
            model_temp <- ARIMA_modeller_dummyM
            data_temp <- train_dummyM
          }
          if (model=="ARIMA_DM"){
            model_temp <- ARIMA_modeller_dummyDM
            data_temp <- train_dummyDM
          }
        }
        # Plockar fram modellen (M) för driftområdet (D)
        fit_temp <- model_temp %>% filter(Driftområde==drift) %>% refit(data_temp)
        
        # Plockar fram data drifrområdet (D)
        data_temp <- data_temp %>% filter(Driftområde==drift)
        
        # Skapar en tom matris
        res_mat <- matrix(0, nrow = nrow(data_temp), ncol = H)
        colnames(res_mat) <- paste0("h_",1:H)
        
        # Tar fram skattade värden för varje prognosghorisont (H) för en modell (M) för ett driftområde (D)
        for(i in 1:H){
          # Tar fram skattade värden för horisonten i
          fit_predict_temp <- fitted(fit_temp, h = i)
          # Sätter negativa skattade värden till 0
          fit_predict_temp$.fitted[fit_predict_temp$.fitted<0] <- 0
          # Avrundar till heltal
          res_mat[,i] <- round(fit_predict_temp$.fitted,0)
        }
        
        # Skattade värden för varje pronoshorisont (H) för en modell (M) för ett driftområde (D)
        pred_all_H <- as.data.frame(res_mat)
        
        # Skapar en tom data.frame för MAE för en modell (M) för ett driftområde (D)
        MAE_h_frame <- data.frame(Horiz = 1:H, MAE = rep(0,H), Model = model, Driftområde = drift)
        
        # Beräknar MAE för varje prognoshorisont (H) för en modell (M) för ett driftområde (D)
        for(i in 1:H){
          MAE_h_frame[i,2] <- mean(abs(pred_all_H[,i]-data_temp$Antal), na.rm=TRUE)
        }
        # Tilldelar MAE för en modell (M)
        assign(paste0("MAE", M), MAE_h_frame)
      }
      # Slår samman RMSE för varje modell (M)
      res <- rbind(MAE1, MAE2, MAE3, MAE4)
      # Tilldelar en modells (M) RMSE till ett driftområde (D)
      assign(paste0("res", D), res)
    }
      # Sparar resultatden från varje driftområde (D) till en data.frame
  ARIMA_MAE <- rbind(res1, res2, res3, res4, res5, res6, res7, res8, res9)
  # Tilldelar alla driftområdens (D) MAE till ett datamaterial (DaMa)
  assign(paste0("ARIMA_MAE_", data), ARIMA_MAE)
  }
  ARIMA_MAE_val <<- ARIMA_MAE_val
  ARIMA_MAE_test <<- ARIMA_MAE_test
  ARIMA_MAE_train <<- ARIMA_MAE_train
}
MAE_ARIMA_funk()
#write.csv(ARIMA_MAE_val, ".../ARIMA_MAE_val.csv", row.names = FALSE)
#write.csv(ARIMA_MAE_test, ".../ARIMA_MAE_test.csv", row.names = FALSE)
#write.csv(ARIMA_MAE_train, ".../ARIMA_MAE_train.csv", row.names = FALSE)


