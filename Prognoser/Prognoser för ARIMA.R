#________________________ Innehåll ________________________
# 1. Laddar paket och data
#__________________________________________________________
# 2. Databearbetning
# - 2.1 Justering av datamaterialet
# - 2.2 Datamängdernas storlek
# - 2.3 ARIMA-modeller
# - 2.4 Dummys för veckodag
# - 2.5 Dummys för månader
# - 2.6 Dummys för månader och veckodagar
#__________________________________________________________
# 3. Funktion för att skapa prognoser
#__________________________________________________________
# 4. Prognoser för de modellerna där ARIMA presterade bäst
#__________________________________________________________

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
#### 2.1 Justering av datamaterialet ####
# Tar fram antalet ärenden per driftområde och dag.
drift_day <- aggregate(day_data$Antal, by=list(day_data$Driftområde, day_data$Tid), FUN=sum)
colnames(drift_day) <-c ("Driftområde", "Datum", "Antal")
drift_day$Datum <- as.Date(drift_day$Datum, format="%Y-%m-%d")


#### 2.2 Datamängdernas storlek ####
# Datum för respektive datamängd
# 62 månader träning
start_train_date <- "2015-11-02"
end_train_date <- "2020-11-30"

# 9 månader validering
start_val_date <- "2020-12-01"
end_val_date <- "2021-08-31"

# 3 månader testmängd
start_test_date <- "2021-09-01"
end_test_date <- "2021-12-01"


#### 2.3 ARIMA-modeller ####
# Skapar objekt - för alla obs
drift_day_tib <- as_tsibble(drift_day, key = Driftområde)
# Skapar objekt - för träning
train <- drift_day_tib %>% filter(Datum >= start_train_date & Datum <= end_train_date) 
# Skapar objekt - för validering
val <- drift_day_tib %>% filter(Datum >= start_val_date & Datum <= end_val_date)
# Skapar objekt - för test
test <- drift_day_tib %>% filter(Datum >= start_test_date & Datum <= end_test_date)


#### 2.4 Dummys för veckodag ####
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


#### 2.5 Dummys för månader ####
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


#### 2.6 Dummys för månader och veckodagar ####
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



#### 3. Funktion för att skapa prognoser ####
prognos_ARIMA <- function(drift, model){
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
  data_temp <- data_temp %>% filter(Driftområde==drift)
  model_temp <- model_temp %>% filter(Driftområde==drift)
  
  for (h in 1:2){
    if (h == 1){
      test_H <- data_temp[2:15,]
    }
    if (h == 2){
      test_H <- data_temp[16:29,]
    }

    pred_H <- model_temp  %>% refit(test_H) %>% forecast(new_data=test_H)
    pred_H$.mean[pred_H$.mean<0] <- 0

    
    plot_data_H <- data.frame(Datum = test_H$Datum,
                               pred = round(pred_H$.mean,0),
                               true = test_H$Antal)

    assign(paste0("plot_data_H", h), plot_data_H)
  }
  lista <- list(plot_data_H1, plot_data_H2)
  names(lista) <- paste0("14_",1:h)
  return(lista)
}

#### 4. Prognoser för de modellerna där ARIMA presterade bäst ####
prog_Haga_city <- prognos_ARIMA(drift="Hagastaden/City", model="ARIMA")
prog_malmo <- prognos_ARIMA(drift="Malmö", model="ARIMA_D")
prog_med_lilj <- prognos_ARIMA(drift="Medborgarplatsen/Liljeholmen", model="ARIMA_DM")
prog_sickla <- prognos_ARIMA(drift="Sickla", model="ARIMA_DM")

prognoser_ARIMA <- list("Hagastaden/City" = prog_Haga_city,
                    "Malmö" = prog_malmo,
                    "Medborgarplatsen/Liljeholmen" = prog_med_lilj,
                    "Sickla" = prog_sickla)
# Sparar
#save(prognoser_ARIMA, file="progs_ARIMA.RData")
