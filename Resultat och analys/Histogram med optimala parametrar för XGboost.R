#__________________________ Innehåll _________________________
# 1. Laddar in paket och parametrar
#_____________________________________________________________
# 2. Funktion för att skapa histogram över optimala parametrar
#_____________________________________________________________
# 3. 9 i 1 plot
#_____________________________________________________________
# 4. Plottar histogram
#_____________________________________________________________



##### 1. Laddar in paket och parametrar ####
load_package <- function(){
  paket <- c("fpp3", "forecast", "ggpubr", "ggplot2","tsibbledata","tsbox","dplyr",
            "scales") # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])] # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                 # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()
load("XGBoost_Parameter.rData")



#### 2. Funktion för att plotta optimala parametrar ####
histo_XG <- function(driftområde, parameter){
  # Vektorer att stoppa in de valda värdena 
  eta <- c()
  gamma <- c()
  max_djup <- c()
  lambda <- c()
  
  # Sekvensen för parametrarna, används för att kontrollera vilka som inte används
  eta_val <- data.frame(eta=as.factor(seq(0,1,0.2)),
                        n=rep(0,6))
  gamma_val <- data.frame(gamma=as.factor(seq(2,12,2)),
                          n=rep(0,length(seq(2,12,2))))
  max_depth_val <- data.frame(max_djup=as.factor(c(3,5,10,20)),
                              n=rep(0,4))
  lambda_val <- data.frame(lambda=as.factor(seq(1,10,2)),
                           n=rep(0,length(seq(1,10,2))))
  for (i in 1:9) {
    drift <- names(final_param_drift)[i]
    name <- paste0("Param_",drift)
    for (j in 1:14) {
      eta[j] <- as.character(final_param_drift[[i]][[j]][[1]])
      gamma[j] <- as.character(final_param_drift[[i]][[j]][[2]])
      max_djup[j] <- as.character(final_param_drift[[i]][[j]][[3]])
      lambda[j] <- as.character(final_param_drift[[i]][[j]][[4]])
    }
    assign(name, tibble(eta=eta,
                        gamma=gamma,
                        max_djup=max_djup,
                        lambda=lambda))
    
  }
  
  if (parameter=="eta") {
    name2 <- eval(parse(text=paste0("Param_",driftområde)))
    compare_param <- eta_val
    
    # Plockar ut för vilken driftområde som det skall plottas                    
    data_plot  <- as.data.frame((name2 %>% count(eta)))
    
    agg_data_plot <- aggregate(. ~ eta, rbind(compare_param,data_plot), FUN=sum)
    
    param <- "\u03B7"  }
  if (parameter=="gamma") {
    name2 <- eval(parse(text=paste0("Param_",driftområde)))
    compare_param <- gamma_val
    
    # Plockar ut för vilken driftområde som det skall plottas                    
    data_plot  <- as.data.frame((name2 %>% count(gamma)))
    
    agg_data_plot <- aggregate(. ~ gamma, rbind(compare_param,data_plot), FUN=sum)
    
    param <- "\u03B3"  }
  if (parameter=="max_djup") {
    name2 <- eval(parse(text=paste0("Param_",driftområde)))
    compare_param <- max_depth_val
    
    # Plockar ut för vilken driftområde som det skall plottas                    
    data_plot  <- as.data.frame((name2 %>% count(max_djup)))
    
    agg_data_plot <- aggregate(. ~ max_djup, rbind(compare_param,data_plot), FUN=sum)
    
    param <- "Max djup" }
  if (parameter=="lambda") {
    name2 <- eval(parse(text=paste0("Param_",driftområde)))
    compare_param <- lambda_val
    
    # Plockar ut för vilken driftområde som det skall plottas                    
    data_plot  <- as.data.frame((name2 %>% count(lambda)))
    
    agg_data_plot <- aggregate(. ~ lambda, rbind(compare_param,data_plot), FUN=sum)
    
    param <- "\u03BB" }
  
    # Skapar counts för varje parametervärde
    plot <- ggplot(agg_data_plot, aes(x=agg_data_plot[,1], y=n)) + 
      geom_bar(stat = "identity",fill="#b7a075") + theme_bw() + 
      labs(y = "Frekvens", 
           title = paste(gsub("_","/",driftområde)),
           x=param) +
      scale_y_continuous(expand = c(0, 0), 
                         limits = c(0, 14),
                         n.breaks = 6,
                         labels = label_number(accuracy=1))+
      theme(plot.title = element_text(hjust = 0.5, face="bold", size=14),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())

  return(plot)
}



#### 3. 9 i 1 plot ####
# Använder funktionen 'histo_XG' och skapar en 
# 9 i 1 plot över histogrammen.
histo_XG_9in1 <- function(parameter){
  for (i in 1:9) {
    name <- paste0("param_plt",i)
    drift <- names(final_param_drift)[i]
    assign(name,histo_XG(driftområde=drift,parameter))
  }
  
  ggarrange(param_plt1,param_plt2,param_plt3,
            param_plt4, param_plt5,param_plt6,
            param_plt7,param_plt8,param_plt9,
            ncol=3,nrow=3)
}



#### 4. Plottar histogram ####
# Eta
histo_XG_9in1("eta")

# Gamma
histo_XG_9in1("gamma")

# Max djup
histo_XG_9in1("max_djup")

# Lambda
histo_XG_9in1("lambda")

