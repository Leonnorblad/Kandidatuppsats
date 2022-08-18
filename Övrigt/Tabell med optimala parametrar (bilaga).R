#____________ Innehåll ____________
# 1. XGBoost parametrar
#__________________________________
# 2. ARIMA parametrar
#__________________________________


#### 1. XGBoost ####
# Laddar in parametrarna
load("XGBoost_Parameter.rData")

# Funktion för att ta fram en tabell med de mest optimala parametrarna
xgboost_opti_params <- function(){
  # Tom data.frame som sedan fylls i med fullständiga optimala parametrar
  param_frame <- data.frame(Driftområde=rep(0,126),
                            Prognoshorisont=rep(0,126),
                            Eta=rep(0,126),
                            Gamma=rep(0,126),
                            Max_djup=rep(0,126),
                            Lambda=rep(0,126))
  m <- 0
  for (i in 1:9) {
    drift <- names(final_param_drift)[i]
    for (j in 1:14) {
      #Skapar index för var parametern skall sättas in
      n <- 2
      #Skapar indec för vilken rad parametern skall sättas in
      m <- m+1
      param_frame[m,2] <- j
      param_frame[m,1] <- drift
      for (k in 1:4) {
        #Skapar index för vilken kolumn parametern skall sättas in
        n <- n+1
        param_frame[m,n] <-final_param_drift[[i]][[j]][[k]]
      }
    }
  }
  return(param_frame)
}
param_frame <- xgboost_opti_params()

# Slutgiltig data.frame
param_frame

# Skapar tabell till pdf
library(xtable)
xtable(param_frame,
       digits=c(0,0,0,2,0,0,0))


#### 2. ARIMA ####
# Laddar in ARIMA-modeller
load("ARIMA_models.RData")

# Tar fram parametrarna
ARIMA_modeller %>% report()
