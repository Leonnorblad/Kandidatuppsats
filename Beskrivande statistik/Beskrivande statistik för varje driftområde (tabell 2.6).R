#__________ Innehåll ___________________________________
# Skapar tabell 2.6 som innehåller beskrivande statistik
# för respektive driftområde.
#_______________________________________________________

# Laddar in data och paket
library(xtable)
day_data <- read.csv("day_data.csv", fileEncoding = "Windows-1252")

# Aggregerar ärenden utifrån driftområde och tid
listan <- aggregate(day_data$Antal, by=list(day_data$Driftområde,
                                            day_data$Tid),
                    FUN=sum)

# Byter kolumnnamn
colnames(listan) <- c("Driftområde", "Tid", "Antal")

# Skapar en tom data.frame
summary_table <- data.frame(Driftområde=rep(0,9),
                            Max_antal = rep(0,9),
                            Median = rep(0,9),
                            Mean = rep(0,9),
                            Mode = rep(0,9))

# Loopar över att ta fram max,min och medelvärdet
for (i in 1:9) {
  drift <- unique(day_data$Driftområde)[i]
  summary_table[i,1] <- drift
  summary_table[i,2] <-  max(listan[listan$Driftområde==drift,3])
  summary_table[i,3] <- median(listan[listan$Driftområde==drift,3])
  summary_table[i,4] <- mean(listan[listan$Driftområde==drift,3])
  #Beräknar typvärdet
  table_drift <- data.frame(table(listan[listan$Driftområde==drift,3]))
  if (as.numeric(table_drift[which.max(table_drift[,2]),1])==1) {
    summary_table[i,5] <- 0
  }
  else {
    summary_table[i,5] <- table_drift[which.max(table_drift[,2]),1]
  }
}
# Tabellen
summary_table

# Skapa tabell till pdf
xtable(summary_table)           