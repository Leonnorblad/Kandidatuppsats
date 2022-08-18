#______________________ Innehåll ___________________________
# 1. Laddar in paket och data
# 2. Rensar orginaldata
# 3. Aggregerar på olika tidsintervall


#### 1. Laddar in paket och data ####
require(lubridate)
require(ggplot2)
require(aweek)
library(ggpubr)
require(stringr)

# Läser in omogen data
data <- read.csv2("sammanslagen_data.csv", encoding = "UTF-8")

### 2. Rensar orginaldata ####
# Tar bort mallområde
data <- data[!data$Driftområde=="Mallområde - 000",]

# Väljer endast de utförare som vi är intresserade att analysera
data <- data[data$Utförare=="Drifttekniker/Fastighetsskötare" |
             data$Utförare=="Fastighetsskötare" |
             data$Utförare=="Hagastaden Drifttekniker" |
             data$Utförare=="Bostadsförvaltare Malmö"|
             data$Utförare=="Teknisk förvaltare"|
             data$Utförare=="Garanti"|
             data$Utförare=="Kontor- och bostadsförvaltare Malmö" |
             data$Utförare=="Drifttekniker Slakthusområdet"|
             data$Utförare=="Teknisk samordnare Sickla"|
             data$Utförare=="Floormanager Malmö"|
             data$Utförare=="Förvaltare"|
             data$Utförare=="Kontor- och bostadsförvaltare Kista"|
             data$Utförare=="Drift - och säkerhetsansvarig"|
             data$Utförare=="Kontor- och bostadsförvaltare Uppsala",]

# Tar bort de ärendetyperna vi inte är intresserade att analysera
data <- data[!data$Ärendetyp=="Internt / Proaktivt (Tillsyn och skötsel)",]

# Tar bort saknade värden i kategorivariabeln
data <- data[!data$Kategori=="",]

# Plockar ut de intressanta variablerna och döper om dem
data <- data.frame(ID=data$X.U.FEFF.ID.,
                 Driftområde=data$Driftområde,
                 Kategori=data$Kategori,
                 Datum=data$Registrerat)

# Undersöker vilken observation som är den första på den första dagen 
head(data[data$Datum=="2015-11-02",],1)

# Undersöker vilken observation som är den sista på den sista dagen 
tail(data[data$Datum=="2021-11-02",],1)

# Plockar ut alla obs från och med 2011-11-01
data <- data[data$Datum>"2015-11-01",]

# Ändrar om namnen för driftområden och kategorier
data$Driftområde <- substring(data$Driftområde, 1,nchar(data$Driftområde)-6)
data$Kategori <- substring(data$Kategori,4)
# Skapar en csv med bearbetad data
#write.csv(data,".../bearbetad_data.csv", row.names = FALSE)


#### 3. Aggregerar på olika tidsintervall ####
# TID - Den önskade tidsaggregeringen
# D = dag
# V = vecka
# M = månad

Aggregate_data <- function(TID) {
  data_komb <- read.csv("bearbetad_data.csv")
  
  # Lägger till en variabel som kan summeras vid aggregering.
  data_komb$count <- 1
  fac <- seq(from=ymd(tail(data,1)$Datum), to=ymd(head(data,1)$Datum), by='days')
  
  # Plockar ut unika driftområden
  drift <- unique(data$Driftområde)
  
  # Plockar ut unika ärendekategorier
  kat <- unique(data$Kategori)
  
  # Skapar olika aggregeringsvariabeler beroende på vilken tidsenhet som valts
  if (TID=="D"){
    # Skapar alla unika kombinationer av driftområde,kategori och dagar
    komb <- expand.grid(drift,kat,fac)
    # Ska senare aggregera på dagar
    agg <- data_komb$Datum
    
  } else if (TID=="V"){
    ###____ Skapar ett facit för att kunna hitta veckor utan ärenden____###
    # Sedan tas datumet för varje vecka (start måndag) mellan start och slut i datamaterialet.
    weeks <- unique(get_date(week=isoweek(fac), year=year(fac)))
    #Skapar alla unika kombinationer av driftområde,kategori och veckor
    komb <- expand.grid(drift,kat,weeks)
    # Ska senare aggregera på veckor
    agg <- get_date(week=isoweek(data$Datum), year=year(data$Datum))
    
  } else if (TID=="M"){
    ###____ Skapar ett facit för att kunna hitta månader utan ärenden____###
    # Sedan tas datumet för varje månad (start måndag) mellan start och slut i datamaterialet.
    months <- unique(ym(paste0(year(fac), "-", month(fac))))
    # Skapar alla unika kombinationer av driftområde,kategori och månader
    komb <- expand.grid(drift,kat,months)
    # Ska senare aggregera på månader
    agg <- ym(paste0(year(data$Datum), "-", month(data$Datum)))
  }
  
  # Aggregerar uppdelat på variablen "agg" som tidigare skapades.
  aggregate_data <- aggregate(data_komb$count, by=list(data_komb$Driftområde,
                                                     data_komb$Kategori,
                                                     agg), FUN=sum)
  # Skapar ett facit för att sedan hitta saknade värden
  facit <- paste(komb$Var1,komb$Var2,komb$Var3, sep="separera")
  # Lägger till kolumnnamn.
  colnames(aggregate_data) <- c("Driftområde","Kategori","Tid", "Antal")
  
  # Skapar aggregeringsvariabel. Vektor med alla obs från datamaterialet
  kontroll_data <- paste(aggregate_data$Driftområde,
                       aggregate_data$Kategori,
                       aggregate_data$Tid,sep="separera")
  #Undersöker om det finns några 
  komb_missing <- facit[-match(kontroll_data,facit)]
  
  komb_missing <- data.frame(do.call("rbind", (strsplit(komb_missing,"separera"))), Antal=0)
  
  # Lägger till kolumnnamn.
  colnames(komb_missing) <- c("Driftområde","Kategori","Tid", "Antal")
  # Skapar en data med alla veckor, inkluderat med saknade värden
  full_data <- rbind(aggregate_data, komb_missing)
  # Gör om variabeln Tid till en tidsvariabel
  full_data$Tid <- ymd(full_data$Tid)
  # Sorterar den fullständiga datan
  data_sort <- full_data[order(full_data$Tid),]
  return(data_sort)
}

# Skapar csv filer
#write.csv(Aggregate_data('D'),".../day_data.csv", row.names = FALSE)
#write.csv(Aggregate_data('V'),".../week_data.csv", row.names = FALSE)
#write.csv(Aggregate_data('M'),".../month_data.csv", row.names = FALSE)
