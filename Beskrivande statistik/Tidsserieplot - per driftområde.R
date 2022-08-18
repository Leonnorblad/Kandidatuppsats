####______________________________________Innehåll____________________________________####
# Skapar först en funktion som tar fram antalet ärenden per dag för ett önskat driftområde,
# plottar sedan ärendeflödet tillsammans med ett 30 dagars glidande medelvärde.

# Laddar in paket
require(lubridate)
require(tidyquant)
require(ggplot2)

# Läser in data
day_data <- read.csv("day_data.csv", fileEncoding = "Windows-1252")
data <- read.csv("bearbetad_data.csv", fileEncoding = "Windows-1252")

# Skapar funktion
data_func_drift <- function(Area){
  # Skapar en vektor med alla dagar mellan de undersökta datumen
  days <- seq(from=ymd(head(day_data,1)$Tid), to=ymd(tail(day_data,1)$Tid), by='days')
  # Plockar ut alla rader i det aktuella Driftområdet
  Area_data <- data[data$Driftområde==Area,]
  # Räknar varje rad som ett ärende (behövs vid aggregering)
  Area_data$count <- 1
  # Aggregerar på datum
  aggregate_data <- aggregate(Area_data$count, by=list(Area_data$Datum), FUN=sum)
  colnames(aggregate_data) <- c("Datum", "Antal")
  # Tar reda på vilka datum som saknas
  days_missing <- days[-match(ymd(unique(aggregate_data$Datum)),days)]
  # Skapar data med de som saknas
  data_missing <- data.frame(Datum=days_missing, Antal=0)
  # Gör dagarna i aggregate till dagar
  aggregate_data$Datum <- ymd(aggregate_data$Datum)
  # Lägger de dagar som saknas
  complete_data <- rbind(aggregate_data, data_missing)
  # Sorterar
  data_sort <- complete_data[order(complete_data$Datum),]
  # Gör om till datum
  data_sort$Datum <- ymd(data_sort$Datum)
  return(data_sort)
}
# Alternativ i funktionen
unique(data$Driftområde)

# För sickla
driftomr <- "Sickla" # Ändra denna för andra driftområden
# Skapar datamaterial med hjälp av funktionen
data_drift <- data_func_drift(driftomr)

# Plottar (Exportera för ett bättre resultat)
ggplot(data_drift, aes(x=Datum, y = Antal)) + geom_line(size=0.9) + theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0,max(data_drift$Antal)+1)) +
  geom_ma(ma_fun = SMA, n = 30, color="red", size=0.9) + 
  labs(title = paste("Antalet ärenden per dag i", driftomr),
       subtitle = paste("Från", head(day_data,1)$Tid, "till", tail(day_data,1)$Tid)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 16),
        plot.subtitle = element_text(hjust=0.5, size=18),
        axis.text = element_text(size=14))
# Svart linje = Antalet ärenden per dag
# Röd linje = 30 dagars glidande medelvärde

  