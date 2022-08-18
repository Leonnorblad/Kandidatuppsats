#____________________________________ Innehåll ____________________________________
# Två funktioner för att plotta tidsserier skapar här.
# Tidsserieplotfunktion 1 summerar alla driftområden eller kategorier.
# Tidsserieplotfunktion 2 visar kombinationen av ett önskat driftområde + kategori.
#__________________________________________________________________________________
# 1. Laddar in paket och data
#__________________________________________________________________________________
# 2. Tidsserieplotfunktion 1 - driftområde eller kategori
#__________________________________________________________________________________
# 3. Skapar plottar - dagsnivå
# - 3.1 Plottar för kategorier med uppdelning dagar
# - 3.2 Plottar för driftområden med uppdelning dagar
#__________________________________________________________________________________
# 4. Skapar plottar - veckonivå
# - 4.1 Plottar för kategorier med uppdelning veckor
# - 4.2 Plottar för driftområden med uppdelning veckor
#__________________________________________________________________________________
# 5. Skapar plottar - Månadsnivå
# - 5.1 Plottar för kategorier med uppdelning på månad
# - 5.2 Plottar för driftområden med uppdelning månad
#__________________________________________________________________________________
# 6. Tidsserieplotfunktion 2 - driftområde och kategori
# - 6.1 Plots för sickla
#__________________________________________________________________________________


#### 1. Laddar in paket och data ####
require(lubridate)
require(ggplot2)
require(aweek)
library(ggpubr)
# Läser in data
data <- read.csv("bearbetad_data.csv", fileEncoding = "Windows-1252")
# Formaterar datumvariabeln
data$Datum <- ymd(data$Datum)


#### 2. Tidsserieplotfunktion ####
# Beskrivning av funktionens argument:

# TID: Den tidsaggegering som önskas
# 'D' - Dagsvis
# 'V' - Veckovis
# 'M' - Månadsvis

# VAR: Den variablen som de olika plottarna ska visa
# 'Driftområde' - Varje plot visar antalet för ett driftområde, kategorierna är då summerade
# 'Kategori' - Varje plot visar antalet för en kategori, driftområdena är då summerade

# TYP2: Nivån av VAR
#______TYP2 - (TYP = Driftområde)__________
# 'Sickla - 130'                      
# 'Uppsala - 161'             
# 'Malmö - 140'                     
# 'Medborgarplatsen/Liljeholmen - 113'
# 'Hagastaden/City - 111'
# 'Kista/Sundbyberg - 120'            
# 'Slakthusområdet/Proppen - 114'     
# 'Slussen - 110'       
# 'Göteborg/Lindholmen - 145'

#______TYP2 - (TYP = Kategori)__________
# 'Byggnad invändigt'     
# 'Inneklimat'           
# 'Allmänt'                  
# 'Elsystem'                    
# 'Tele, passage och datasystem'
# 'VVS'
# 'Byggnad utvändigt'
# 'Transportsystem'           
# 'Utemiljö'             
# 'Vitvaror och Tvättstuga'
# 'Styr- och Övervakningssystem'

#____________print_data_________________
# TRUE = Skapar en data.frame med data och plot
# FALSE = Skapar endast en plot


T.S_plot <- function(TYP, TYP2, TID, print_data){
  ### Plockar ut olika datamaterial beroende på om det är driftområde eller kategori som undersöks
  if(TYP=="Driftområde"){
    # Filtrerar ut data för det aktuella driftområdet
    data_filter<-data[data$Driftområde==TYP2,]
    
  } else if (TYP=="Kategori"){
    # Filtrerar ut data för den aktuella kategorin
    data_filter<-data[data$Kategori==TYP2,]
  }
  
  
  ### Skapar olika aggregeringsvariabeler beroende på vilken tidsenhet som valts
  if(TID=="D"){
    # Skapar "facit". En vektor med alla dagar från första till sista i materialet.
    facit<-seq(from=ymd(tail(data,1)$Datum), to=ymd(head(data,1)$Datum), by='days')
    # Skapar aggregeringsvariabel. Vektorn med dagar. 
    agg<-data_filter$Datum
    
  } else if (TID=="V"){
    # Skapar "facit". Skapar först en vektor med alla dagar från första till sista i materialet.
    fac<-seq(from=ymd(tail(data,1)$Datum), to=ymd(head(data,1)$Datum), by='days')
    # Sedan tas datumet för varje måndag i datamaterialet ut.
    facit<-unique(get_date(week=isoweek(fac), year=year(fac)))
    # Skapar aggregeringsvariabel. Vektor med alla veckor
    agg<-get_date(week=isoweek(data_filter$Datum), year=year(data_filter$Datum))
    
  } else if (TID=="M"){
    # Skapar "facit". Skapar först en vektor med alla dagar från första till sista i materialet.
    fac<-seq(from=ymd(tail(data,1)$Datum), to=ymd(head(data,1)$Datum), by='days')
    # Sedan tas den första dagen i varje månad i datamaterialet ut.
    facit<-unique(ym(paste0(year(fac), "-", month(fac))))
    # Skapar aggregeringsvariabel. Vektor med alla månader.
    agg<-ym(paste0(year(data_filter$Datum), "-", month(data_filter$Datum)))
  }
  
  
  #### Aggeregering
  # Lägger till en variabel som kan summeras vid aggregering.
  data_filter$count<-1
  # Aggregerar uppdelat på variablen "agg" som tidigare skapades.
  aggregate_data<-aggregate(data_filter$count, by=list(agg), FUN=sum)
  # Lägger till kolumnnamn.
  colnames(aggregate_data)<-c("Tid", "Antal")
  
  # Skapar en vektor med de datumen som saknas i materialet.
  time_missing<-facit[-match(ymd(aggregate_data$Tid),facit)]
  
  # Gör om aggregate_data datumen till datum
  aggregate_data$Tid<-ymd(aggregate_data$Tid)
  
  
  ### Om det finns ett saknat datum ska 0:or adderas till data
  if(length(time_missing)>0){
    # Skapar en dataframe med de saknade datumen och 0:or
    data_missing<-data.frame(Tid=time_missing, Antal=0)
    # Lägger ihop materialen och skapar den slutgiltiga data.framen
    full_data<-rbind(aggregate_data, data_missing)
    # Sorterar
    data_sort<-full_data[order(full_data$Tid),]
    # Skriver ut ett infomeddelande.
    print(paste0(round(nrow(data_missing)/nrow(data_sort),2)*100, "% 0:r i ",TYP2))
  } else {
    # Om inte data har saknade datum så skrivs materialet över utan att lägga till 0:or
    data_sort<-aggregate_data
    # Skriver ut ett infomeddelande.
    print("Data hade inga 0:or")
  }
  
  # Skapar det färdiga materialet för utskrift och plot
  ret<-data.frame(Tid=data_sort$Tid,
                  Antal=data_sort$Antal)
  
  # Formaterar datumen
  ret$Tid <- ymd(ret$Tid)
  

  # Skapar plot
  tid_plot <- ggplot(ret[(nrow(ret)-60):nrow(ret),], aes(x=Tid, y=Antal))+ 
    geom_line()+theme_bw()+ 
    labs(title=paste0(TYP2))+
    ylab('Antal ärenden') +
    theme(panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  print(tid_plot)
  return(tid_plot)
  
  ### Om en data.frame med data ska retuneras. 
  if (print_data==TRUE){
    return(ret)
  }
}



#### 3. Skapar plottar - dagsnivå ####
#### 3.1 Plottar för kategorier med uppdelning dagar ####
for (i in 1:11) {
  kat <- unique(data$Kategori)[i]
  name <- paste0("tid_plt",i)
  assign(name,T.S_plot(TYP="Kategori", TYP2=kat, TID="D", print_data="FALSE"))
}

plot_kat_day <- ggarrange(tid_plt1,tid_plt2,tid_plt3,tid_plt4,
                          tid_plt5,tid_plt6,tid_plt7,tid_plt8,
                          tid_plt9,tid_plt10,tid_plt11,
                          ncol=4,nrow=3)

#### 3.2 Plottar för driftområden med uppdelning dagar ####
for (i in 1:9) {
  drift <- unique(data$Driftområde)[i]
  name <- paste0("tid_plt",i)
  assign(name,T.S_plot(TYP="Driftområde", TYP2=drift, TID="D", print_data="FALSE"))
}

plot_drift_day <- ggarrange(tid_plt1,tid_plt2,tid_plt3,tid_plt4,
                          tid_plt5,tid_plt6,tid_plt7,tid_plt8,
                          tid_plt9,
                          ncol=3,nrow=3)



#### 4. Skapar plottar - veckonivå ####
#### 4.1 Plottar för kategorier med uppdelning veckor ####
for (i in 1:11) {
  kat <- unique(data$Kategori)[i]
  name <- paste0("tid_plt",i)
  assign(name,T.S_plot(TYP="Kategori", TYP2=kat, TID="V", print_data="FALSE"))
}

#Skapar en gemensam plot för alla kategorier
plot_kat_week <- ggarrange(tid_plt1,tid_plt2,tid_plt3,tid_plt4,
                           tid_plt5,tid_plt6,tid_plt7,tid_plt8,
                           tid_plt9,tid_plt10,tid_plt11,
                           ncol=4,nrow=3)


#### 4.2 Plottar för driftområden med uppdelning veckor ####
for (i in 1:9) {
  drift <- unique(data$Driftområde)[i]
  name <- paste0("tid_plt",i)
  assign(name,T.S_plot(TYP="Driftområde", TYP2=drift, TID="V", print_data="FALSE"))
}

plot_drift.week <- ggarrange(tid_plt1,tid_plt2,tid_plt3,tid_plt4,
                            tid_plt5,tid_plt6,tid_plt7,tid_plt8,
                            tid_plt9,
                            ncol=3,nrow=3)



#### 5. Skapar plottar - Månadsnivå ####
#### 5.1 Plottar för kategorier med uppdelning på månad ####
for (i in 1:11) {
  kat <- unique(data$Kategori)[i]
  name <- paste0("tid_plt",i)
  assign(name,T.S_plot(TYP="Kategori", TYP2=kat, TID="M", print_data="FALSE"))
}

# Skapar en gemensam plot för alla kategorier
plot_kat.month <- ggarrange(tid_plt1,tid_plt2,tid_plt3,tid_plt4,
                            tid_plt5,tid_plt6,tid_plt7,tid_plt8,
                            tid_plt9,tid_plt10,tid_plt11,
                            ncol=4,nrow=3)

#### 5.2 Plottar för driftområden med uppdelning månad ####
for (i in 1:9) {
  drift <- unique(data$Driftområde)[i]
  name <- paste0("tid_plt",i)
  assign(name,T.S_plot(TYP="Driftområde", TYP2=drift, TID="M", print_data="FALSE"))
}

plot_drift.month <- ggarrange(tid_plt1,tid_plt2,tid_plt3,tid_plt4,
                            tid_plt5,tid_plt6,tid_plt7,tid_plt8,
                            tid_plt9,
                            ncol=3,nrow=3)



#### 6. Tidsserieplotfunktion 2 - driftområde och kategori ####

T.S_plot.komb <- function(Driftområde, Kategori, TID) {
  data_komb <- data
  # Lägger till en variabel som kan summeras vid aggregering.
  data_komb$count<-1
  ### Skapar olika aggregeringsvariabeler beroende på vilken tidsenhet som valts
  if (TID=="D"){
    fac<-seq(from=ymd(tail(data,1)$Datum), to=ymd(head(data,1)$Datum), by='days')
    #Plockar ut unika driftområden
    drift <- unique(data$Driftområde)
    #Plockar ut unika ärendekategorier
    kat <- unique(data$Kategori)
    #Skapar alla unika kombinationer av driftområde,kategori och veckor
    komb <- expand.grid(drift,kat,fac)
    #Skapar ett facit för att sedan hitta saknade värden
    facit <- paste(komb$Var1,komb$Var2,komb$Var3, sep="separera")
    # Aggregerar uppdelat på variablen "agg" som tidigare skapades.
    aggregate_data<-aggregate(data_komb$count, by=list(data_komb$Driftområde,
                                                       data_komb$Kategori,
                                                       data_komb$Datum), FUN=sum)
  } else if (TID=="V"){
    ###____ Skapar ett facit för att kunna hitta veckor utan ärenden____###
    fac<-seq(from=ymd(tail(data,1)$Datum), to=ymd(head(data,1)$Datum), by='days')
    # Sedan tas datumet för varje vecka (start måndag) mellan start och slut i datamaterialet.
    weeks<-unique(get_date(week=isoweek(fac), year=year(fac)))
    #Plockar ut unika driftområden
    drift <- unique(data$Driftområde)
    #Plockar ut unika ärendekategorier
    kat <- unique(data$Kategori)
    #Skapar alla unika kombinationer av driftområde,kategori och veckor
    komb <- expand.grid(drift,kat,weeks)
    #Skapar ett facit för att sedan hitta saknade värden
    facit <- paste(komb$Var1,komb$Var2,komb$Var3, sep="separera")
    # Alla observationer per vecka i datamaterialet
    weeks_data <- get_date(week=isoweek(data$Datum), year=year(data$Datum))
    # Aggregerar uppdelat på variablen "agg" som tidigare skapades.
    aggregate_data<-aggregate(data_komb$count, by=list(data_komb$Driftområde,
                                                       data_komb$Kategori,
                                                       weeks_data), FUN=sum)
  } else if (TID=="M"){
    ###____ Skapar ett facit för att kunna hitta månader utan ärenden____###
    fac<-seq(from=ymd(tail(data,1)$Datum), to=ymd(head(data,1)$Datum), by='days')
    # Sedan tas datumet för varje månad (start måndag) mellan start och slut i datamaterialet.
    months<-unique(ym(paste0(year(fac), "-", month(fac))))
    #Plockar ut unika driftområden
    drift <- unique(data$Driftområde)
    #Plockar ut unika ärendekategorier
    kat <- unique(data$Kategori)
    #Skapar alla unika kombinationer av driftområde,kategori och veckor
    komb <- expand.grid(drift,kat,months)
    #Skapar ett facit för att sedan hitta saknade värden
    facit <- paste(komb$Var1,komb$Var2,komb$Var3, sep="separera")
    # Alla observationer per månad i datamaterialet
    months_data <- ym(paste0(year(data$Datum), "-", month(data$Datum)))
    # Aggregerar uppdelat på variablen "agg" som tidigare skapades.
    aggregate_data<-aggregate(data_komb$count, by=list(data_komb$Driftområde,
                                                       data_komb$Kategori,
                                                       months_data), FUN=sum)
  }
  # Lägger till kolumnnamn.
  colnames(aggregate_data)<-c("Driftområde","Kategori","Tid", "Antal")
  
  # Skapar aggregeringsvariabel. Vektor med alla ob från datamaterialet
  kontroll_data<-paste(aggregate_data$Driftområde,
                       aggregate_data$Kategori,
                       aggregate_data$Tid,sep="separera")
  #Undersöker om det finns några 
  komb_missing<-facit[-match(kontroll_data,facit)]
  
  komb_missing <- data.frame(do.call("rbind", (strsplit(komb_missing,"separera"))),
                             Antal=0)
  # Lägger till kolumnnamn.
  colnames(komb_missing)<-c("Driftområde","Kategori","Tid", "Antal")
  #Skapr en data med alla veckor, inkluderat med saknade värden
  full_data<-rbind(aggregate_data, komb_missing)
  #Gör om variabeln Tid till en tidsvariabel
  full_data$Tid<-ymd(full_data$Tid)
  # Sorterar den fullständiga datan
  data_sort<-full_data[order(full_data$Tid),]
  ### Plockar ut olika datamaterial beroende på om det är driftområde eller kategori som undersöks
  data_unik <- data_sort[data_sort$Driftområde==Driftområde&data_sort$Kategori==Kategori,]
  #Skapar en ny dataframe för att få tydligare radnummer
  data_unik <- data.frame(data_unik$Driftområde,
                          data_unik$Kategori,
                          data_unik$Tid,
                          data_unik$Antal)
  colnames(data_unik)<-c("Driftområde","Kategori","Tid", "Antal")

  # Skapar plot
  tid_plot_komb <- ggplot(data_unik[(nrow(data_unik)-60):nrow(data_unik),], aes(x=Tid, y=Antal))+ 
    geom_line()+theme_bw()+ 
    labs(title=paste0("Tidsserieplot över ",Driftområde,":",Kategori))+
    ylab('Antal ärenden') +
    theme(panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  print(tid_plot_komb)
  
}

#### 6.1 Plots för sickla ####
# Funktionen kan användas för alla drifområden, här är ett exempel för sickla
for (i in 1:11) {
  kat <- unique(data$Kategori)[i]
  name <- paste0("tid_plt",i)
  assign(name,T.S_plot.komb("Sickla",kat,'D'))
}

plot_sickla.day <- ggarrange(tid_plt1,tid_plt2,tid_plt3,tid_plt4,
                            tid_plt5,tid_plt6,tid_plt7,tid_plt8,
                            tid_plt9,tid_plt10,tid_plt11,
                            ncol=4,nrow=3)


T.S_plot.komb("Sickla - 130","T0 Allmänt",'D')
T.S_plot.komb("Sickla - 130","T0 Allmänt",'M')
T.S_plot.komb("Sickla - 130", "T8 Styr- och Övervakningssystem" ,'V')
T.S_plot.komb("Sickla - 130", "T8 Styr- och Övervakningssystem" ,'M')