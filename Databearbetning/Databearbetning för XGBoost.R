#____________________ Innehåll ______________________
# Data anpassas för att kunna träna XGBoost modeller.
#____________________________________________________

#### Laddar in paket och data ####
load_package<-function(){
  paket<- c("dplyr","tsibble", "tibble", "lubridate", "xgboost", "caret",
            "plyr","ggplot2","forecast","fastDummies","data.table")        # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])] # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                 # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()

data <- read.csv("day_data.csv", fileEncoding = "Windows-1252")



#### Funktion för att anpassa data ####
xgboost_data <- function() {
  
  # Aggregerar på datum och driftområde
  data_agg <- aggregate(data$Antal, by=list(data$Driftområde,
                                            ymd(data$Tid)),FUN=sum)
  # Lägger till kolumnnamn
  colnames(data_agg) <- colnames(data[,-2])
  data_agg$Driftområde <- gsub("/","_",data_agg$Driftområde)
  # Skapar dummy för veckodag och månad
  data_agg$Veckodag <- weekdays(data_agg$Tid)
  data_agg$Månad <- months(data_agg$Tid)
  data_agg <- dummy_cols(data_agg, select_columns = c("Veckodag","Månad","Driftområde"))

  # Skapar träningsmängd
  train <- data_agg[data_agg$Tid>="2015-11-02"&data_agg$Tid<="2020-11-30",]

  # Skapar valideringsmängd 
  val <- data_agg[data_agg$Tid>="2020-12-01"&data_agg$Tid<="2021-08-31",]

  # Skapar testmängd
  test <- data_agg[data_agg$Tid>="2021-09-01"&data_agg$Tid<="2021-12-01",]
  
  list_train <- list()
  list_val <-list()
  list_test <-list()
  
  for (n in 1:14) {
  # Skapar olika datamaterial för att kunna skapa laggar per driftområde
  for (i in 1:3) {
    if (i==1) {
      data_create <- train
      data_namn <-"_train" # Skapar ett namn till datamaterialet 
    } else if (i==2) {
      data_create <- val
      data_namn <- "_val" # Skapar ett namn till datamaterialet 
    } else {
      data_create <- test
      data_namn <- "_test" # Skapar ett namn till datamaterialet 
    }
    
    # Utförande på data
    for (j in 1:length(unique(data_agg$Driftområde))) {
      drift <- unique(data_agg$Driftområde)[j]
      namn <- paste0(gsub("/","_",drift),data_namn)
      # Skapar en dataframe för varje specifikt driftområde, plockar ut data mellan 2015-11-02 och 2021-09-21
      assign(namn, data.frame(Driftområde=gsub("/","_",drift),
                              Tid = data_create[data_create$Driftområde==drift,2],
                              Tid_pred = shift(data_create[data_create$Driftområde==drift,2],-n),
                              Yt_pred = shift(data_create[data_create$Driftområde==drift,3],-n),
                              # Lägger till Y för att kunna kontrollera att all data har laggats rätt
                              Y = data_create[data_create$Driftområde==drift,3]))
      
      namn2 <- eval(parse(text=paste0(gsub("/","_",drift),data_namn)))
      data_comb <- data_agg[which(data_agg$Driftområde==drift),]
      colnames(data_comb)[2] <- "Tid_pred"
      
      namn2 <- join(namn2,data_comb[c(2,6:ncol(data_comb))], by="Tid_pred")
      
      # skapar 14 st förklarande variabler med tidigare värden på antal ärenden
      for (k in 1:15) {
        # Anger namnet på variabeln
        Lag <- paste0("X_yt-",k-1)
        # Anger vilken förskjutning, 
        ant_lag <- k-1
        # Lägger till de olika förklarande variablerna som innehåller observerat antal ärenden från tidigare tidpunkter
        namn2[ , ncol(namn2) + 1] <- shift(namn2$Y,n=ant_lag)
        colnames(namn2)[ncol(namn2)] <- Lag
        
      }
      # Sparar data med nya variablerna, tar bort NA värden
      assign(namn,namn2[c(-1:-14,-(nrow(namn2)-(n-1)):-nrow(namn2)),])
    }
  }
  
  
  # Aggregerar ihop alla driftområden och skapar datamaterial utifrån prognoshorizont
  # För träning
  data_xg_train <-rbind.fill(Göteborg_Lindholmen_train,Hagastaden_City_train,Kista_Sundbyberg_train,
                             Malmö_train,Medborgarplatsen_Liljeholmen_train,Sickla_train,Slakthusområdet_Proppen_train,
                             Slussen_train,Uppsala_train)
  list_train[[n]] <- subset(data_xg_train, select=-c(Y,Veckodag_måndag,Månad_januari,Driftområde_Göteborg_Lindholmen))
  
  # För validering
  data_xg_val <-rbind.fill(Göteborg_Lindholmen_val,Hagastaden_City_val,Kista_Sundbyberg_val,
                           Malmö_val,Medborgarplatsen_Liljeholmen_val,Sickla_val,Slakthusområdet_Proppen_val,
                           Slussen_val,Uppsala_val)
  list_val[[n]] <-subset(data_xg_val, select=-c(Y,Veckodag_måndag,Månad_januari,Driftområde_Göteborg_Lindholmen))
  
  # För test
  data_xg_test <-rbind.fill(Göteborg_Lindholmen_test,Hagastaden_City_test,Kista_Sundbyberg_test,
                            Malmö_test,Medborgarplatsen_Liljeholmen_test,Sickla_test,Slakthusområdet_Proppen_test,
                            Slussen_test,Uppsala_test)
  list_test[[n]] <- subset(data_xg_test, select=-c(Y,Veckodag_måndag,Månad_januari,Driftområde_Göteborg_Lindholmen))
  }
  
  # De färdiga listorna
  list_train <<- list_train
  list_val <<-list_val
  list_test <<-list_test
}

xgboost_data()
