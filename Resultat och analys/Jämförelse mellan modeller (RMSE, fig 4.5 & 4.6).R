#__________________ Innehåll __________________
# 1. Laddar paket och data
#______________________________________________
# 2. RMSE plot
# - 2.1 Färgpalett som fungerar för färgblinda
# - 2.2 Funktion
# _____________________________________________
# 3. Skapar plottar
# - 3.1 Unika plottar
# - 3.2 9 i 1 plot
# _____________________________________________




#### 1. Laddar paket och data ####
# Laddar in paket
load_package <- function(){
  paket<- c("fpp3", "forecast", "ggpubr", "ggplot2","tsibbledata","tsbox",
            "ggplot2", "gridExtra", "grid") # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])] # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                 # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()

# Läser in data
day_data <- read.csv("day_data.csv", fileEncoding = "Windows-1252")

# Läser in RMSE-data för ARIMA.
RMSE_ARIMA_val <- read.csv("RMSE_ARIMA_val.csv", fileEncoding = "Windows-1252")
RMSE_ARIMA_train <- read.csv("RMSE_ARIMA_train.csv", fileEncoding = "Windows-1252")

# Läser in RMSE-data för XGBoost.
RMSE_XGBoost_train <- read.csv("RMSE_XGBoost_train.csv", fileEncoding = "Windows-1252")
RMSE_XGBoost_val <- read.csv("RMSE_XGBoost_val.csv", fileEncoding = "Windows-1252")

# Slår samman materialen
RMSE_data_val <- rbind(RMSE_ARIMA_val, RMSE_XGBoost_val)
RMSE_data_train <- rbind(RMSE_ARIMA_train, RMSE_XGBoost_train)



#### 2. RMSE plot ####
#### 2.1 Färgpalett som fungerar för färgblinda ####
# En färg för varje typ av modell
cbbPalette <- c("#000000",
                "#E69F00",
                "#56B4E9",
                "#009E73",
                "#F0E442",
                "#0072B2",
                "#D55E00",
                "#CC79A7")


#### 2.2 RMSE plot funktion ####
plot_RMSE <- function(RMSE_data, Driftområde){
  # Plockar ut ett driftområde
  RMSE_data_driftTemp <- RMSE_data[RMSE_data$Driftområde==Driftområde,]
  H <- max(RMSE_data$Horiz)
  
  # Skapar plot ###
  plot <- ggplot(RMSE_data_driftTemp,
               aes(x = Horiz, y = RMSE,
                   group = Model,
                   colour = Model)) +
    geom_line() + 
    labs(y = "RMSE", 
         title = paste(Driftområde),
         x="Prognoshorisont") +
    theme_bw() + geom_point(size=2) + scale_x_continuous(breaks=1:H) +
    scale_color_manual(values = cbbPalette) +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          legend.title = element_text(size=18, face="bold"),
          legend.text = element_text(size=16),
          plot.subtitle = element_text(hjust=0.5, size=16),
          panel.grid.minor = element_blank())
  return(plot)
}



#### 3. Skapar plottar ####
#### 3.1 Unika plottar ####
plot_RMSE(RMSE_ARIMA_val, Driftområde="Medborgarplatsen/Liljeholmen")
plot_RMSE(RMSE_ARIMA, Driftområde="Slakthusområdet/Proppen")
plot_RMSE(RMSE_ARIMA, Driftområde="Malmö")
plot_RMSE(RMSE_ARIMA, Driftområde="Sickla")
plot_RMSE(RMSE_ARIMA, Driftområde="Uppsala")
plot_RMSE(RMSE_ARIMA, Driftområde="Kista/Sundbyberg")
plot_RMSE(RMSE_ARIMA, Driftområde="Göteborg/Lindholmen")
plot_RMSE(RMSE_ARIMA, Driftområde="Hagastaden/City")
plot_RMSE(RMSE_ARIMA, Driftområde="Slussen")


#### 3.2 9 i 1 plot ####
for (i in 1:9) {
  name <- paste0("RMSE_plt",i)
  drift<-unique(day_data$Driftområde)[i]
  assign(name,plot_RMSE(RMSE_data_train, Driftområde=drift))
}

grid_arrange_shared_legend <- function(...,
                                       nrow = 1,
                                       ncol = length(list(...)),
                                       position = c("bottom", "right")){
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
}

grid_arrange_shared_legend(RMSE_plt1,RMSE_plt2,RMSE_plt3,
                           RMSE_plt4, RMSE_plt5,RMSE_plt6,
                           RMSE_plt7,RMSE_plt8,RMSE_plt9,
                           ncol=3,nrow=3)


