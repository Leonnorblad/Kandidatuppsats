#____________________ Innehåll ____________________
# 1. Laddar paket och data
# _________________________________________________
# 2. Funktion för att plotta prediktioner
# _________________________________________________
# 3. Funktion för att plotta flera plots en
#__________________________________________________
# 4. Prognoserna för Malmö 
#__________________________________________________

#### 1. Laddar paket och data ####
# Laddar paket
load_package<-function(){
  paket<- c("dplyr","tsibble", "tibble", "lubridate", "xgboost", "caret",
            "plyr","ggplot2","forecast","fastDummies","data.table","scales",
            "gridExtra", "grid")         # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])] # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                 # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()

# Laddar data
day_data<-read.csv("day_data.csv", fileEncoding = "Windows-1252")

# Laddar in prognoserna från XGBoost och ARIMA
load("progs_XGBoost.RData")
load("progs_ARIMA.RData")

# Slår samman mängderna
progs_All <- append(progs_XGBoost,prognoser_ARIMA)



#### 2. Funktion för att plotta prediktioner ####
colors <- c("Observerade värden" = "blue", # Observerade värden = blåa
            "Skattade värden" = "red")     # Skattade värden = röda

for (i in 1:9) {
  drift <- gsub("/","_",names(progs_All)[i])
  for (j in 1:2) {
    if(j==1){
      rub <- paste0("Prognoser för ", gsub("_","/",names(progs_All)[i])," september 2021")
      x_lab <- c()
    }
    if (j==2){
      rub <- c()
      x_lab <- " "
    }
    #plocka fram maxvärden för att få snygga y-axel värden
    max_val_1 <- max(progs_All[[i]][[1]]$pred,progs_All[[i]][[1]]$true)
    max_val_2 <- max(progs_All[[i]][[2]]$pred,progs_All[[i]][[2]]$true)
    
    name <- paste0(drift,"_prog_",j)
    plot <- ggplot(progs_All[[i]][[j]],
                   aes(x = Datum)) +
      geom_line(aes(y=true, color="Observerade värden")) +
      geom_line(aes(y=pred, color="Skattade värden"))+
      geom_point(aes(y=true, color="Observerade värden")) +
      geom_point(aes(y=pred, color="Skattade värden"))+
      labs(y = "Antal ärenden", 
           title = rub,
           x=x_lab,
           color="Serie") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size=14, angle=23,vjust = 0.9),
            axis.text.y = element_text(size=14),
            legend.title = element_text(size=18, face="bold"),
            legend.text = element_text(size=16),
            plot.subtitle = element_text(hjust=0.5, size=18),
            panel.grid.minor = element_blank()) +
      scale_color_manual(values=colors)+
      scale_x_date(date_labels="%d %b", breaks = unique(progs_All[[1]][[j]]$Datum))+
      scale_y_continuous(limits = c(0, max(max_val_1,max_val_2)+2),
                         n.breaks = 6,
                         labels = label_number(accuracy=1))
    assign(name,plot)
  }
}



#### 3. Funktion för att plotta flera plots en ####
grid_arrange_shared_legend <- function(...,
                                       nrow = 1,
                                       ncol = length(list(...)),
                                       position = c("bottom", "right")) {
  
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

names(progs_All)


#### 4. Prognoserna för Malmö ####
grid_arrange_shared_legend(Malmö_prog_1,
                           Malmö_prog_2,
                           ncol=1,nrow=2)
