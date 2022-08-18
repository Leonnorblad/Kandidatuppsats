#_________ Innehåll __________
# 1. Laddar paket och data
#_____________________________
# 2. MAE plot
#_____________________________
# 3. RMSE plot
#_____________________________
# 4. Skapar plottar
# - 4.1 9 i 1 plot för MAE
# - 4.2 9 i 1 plot för RMSE
#_____________________________



#### 1. Laddar paket och data ####
load_package <- function(){
  paket<- c("fpp3", "forecast", "ggpubr", "ggplot2","tsibbledata","tsbox") # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])] # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                 # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()

day_data <- read.csv("day_data.csv", fileEncoding = "Windows-1252")

# Laddar in värden för MAE - ARIMA
ARIMA_MAE_test <- read.csv("ARIMA_MAE_test.csv")

# Laddar in värden för MAE - XGBoost
XGBoost_MAE_test <-read.csv("MAE_XGBoost_test.csv")

# Slår samman MAE data från ARIMA och XGBoost
MAE_data_test <- rbind(ARIMA_MAE_test, XGBoost_MAE_test)


# Laddar in värden för RMSE - ARIMA
RMSE_ARIMA_test <- read.csv("RMSE_ARIMA_test.csv", fileEncoding = "UTF-8")

# Laddar in värden för RMSE - XGBoost
RMSE_XGBoost_test <- read.csv("RMSE_XGBoost_test.csv", fileEncoding = "Windows-1252")
colnames(RMSE_ARIMA_test) <- colnames(RMSE_XGBoost_test)

# Slår samman RMSE data från ARIMA och XGBoost
RMSE_data_test <- rbind(RMSE_ARIMA_test, RMSE_XGBoost_test)



#### 2. MAE plot ####
# Lägger till färgpaletten
cbbPalette <- c("#000000",
                "#E69F00", 
                "#56B4E9", 
                "#009E73", 
                "#F0E442",
                "#0072B2", 
                "#D55E00", 
                "#CC79A7")

# Funktion för att plotta MAE
plot_MAE <- function(MAE_data, driftområde, model){
  # Väljer olika färger för olika modelltyper
  if (model =="ARIMA"){
    cb=cbbPalette[1]
  }
  if (model =="ARIMA_D"){
    cb=cbbPalette[2]
  }
  if (model =="ARIMA_DM"){
    cb=cbbPalette[3]
  }
  if (model =="ARIMA_M"){
    cb=cbbPalette[4]
  } 
  if (model =="XGBoost"){
    cb=cbbPalette[5]
  }
  # Filtrerar ut ett driftområde
  MAE_data_driftTemp <- MAE_data %>% filter(Driftområde==driftområde & Model==model)
  
  # Max horisonten H
  H <- max(MAE_data$Horiz)
  
  # Skapar plot 
  plot <- ggplot(MAE_data_driftTemp,
               aes(x = Horiz, y = MAE, color=Model)) +
    geom_line() +
    labs(y = "MAE",
         title = paste(driftområde),
         x="Prognoshorisont") + 
    scale_color_manual(values = cb) +
    theme_bw() + geom_point(size=2) + scale_x_continuous(breaks=1:H) +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          legend.position = 'none',
          plot.subtitle = element_text(hjust=0.5, size=16),
          panel.grid.minor = element_blank())
  return(plot)
}



#### 3. RMSE plot ####
plot_RMSE <- function(RMSE_data, driftområde, model){
  # Väljer olika färger för olika modelltyper
  if (model =="ARIMA"){
    cb=cbbPalette[1]
  }
  if (model =="ARIMA_D"){
    cb=cbbPalette[2]
  }
  if (model =="ARIMA_DM"){
    cb=cbbPalette[3]
  }
  if (model =="ARIMA_M"){
    cb=cbbPalette[4]
  } 
  if (model =="XGBoost"){
    cb=cbbPalette[5]
  }
  # Filtrerar ut ett driftområde
  MAE_data_driftTemp <- RMSE_data %>% filter(Driftområde==driftområde & Model==model)
  
  # Max horisonten H
  H <- max(RMSE_data$Horiz)
  
  # Skapar plot 
  plot <- ggplot(MAE_data_driftTemp,
                 aes(x = Horiz, y = RMSE, color = Model)) +
    geom_line() +
    labs(y = "RMSE",
         title = paste(driftområde),
         x="Prognoshorisont") + 
    scale_color_manual(values = cb) +
    theme_bw() + geom_point(size=2) + scale_x_continuous(breaks=1:H) +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          legend.position = 'none',
          plot.subtitle = element_text(hjust=0.5, size=16),
          panel.grid.minor = element_blank())
  return(plot)
}


# Ordningen som driftområdena loopas i
unique(day_data$Driftområde)
# Bästa modellen för respektive driftområde
models <- c("ARIMA_DM", # Medborgarplatsen/Liljeholmen
            "XGBoost",  # Slakthusområdet/Proppen
            "ARIMA_D",  # Malmö
            "ARIMA_DM", # Sickla
            "XGBoost",  # Uppsala
            "XGBoost",  # Kista/Sundbyberg
            "XGBoost",  # Göteborg/Lindholmen
            "ARIMA",    # Hagastaden/City
            "XGBoost")  # Slussen

#### 4. SKapar plottar ####
# 4.1 9 i 1 plot för MAE ####
for (i in 1:9) {
  name <- paste0("MAE_plt",i)
  drift <- unique(day_data$Driftområde)[i]
  model1 <- models[i]
  assign(name,plot_MAE(MAE_data_test, driftområde=drift, model=model1))
}

ggarrange(MAE_plt1,MAE_plt2,MAE_plt3,
         MAE_plt4, MAE_plt5, MAE_plt6,
         MAE_plt7, MAE_plt8, MAE_plt9,
         ncol=3,nrow=3)


# 4.2 9 i 1 plot för RMSE ####
for (i in 1:9) {
  name <- paste0("RMSE_plt",i)
  drift <- unique(day_data$Driftområde)[i]
  model1 <- models[i]
  assign(name,plot_RMSE(RMSE_data_test, driftområde=drift, model=model1))
}

ggarrange(RMSE_plt1,RMSE_plt2,RMSE_plt3,
          RMSE_plt4, RMSE_plt5, RMSE_plt6,
          RMSE_plt7, RMSE_plt8, RMSE_plt9,
          ncol=3,nrow=3)
