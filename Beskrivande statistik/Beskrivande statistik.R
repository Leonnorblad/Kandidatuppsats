#_____________________________________________Innehåll____________________________________________
# Denna kod har används för att beräkna den beskrivande statistiken som presenteras i uppsatsen.
# 1. Laddar paket och data
# ________________________________________________________________________________________________
# 2. Enkel visualisering
# ________________________________________________________________________________________________
# 3. Enkel beskrivande statistik
# ________________________________________________________________________________________________
# 4. Stapeldiagram
# - 4.1 Stapeldiagram över totala antalet ärenden för repsektive driftområde
# - 4.2 Stapeldiagram över totala antalet ärenden för repsektive ärendekategori
# ________________________________________________________________________________________________
# 5. Tabeller med antal och andel
# - 5.1 Totala antalet ärenden för respektive ärendekategorier (med andel)
# - 5.2 Totala antalet ärenden uppdelat på driftområde (med andel)
# ________________________________________________________________________________________________
# 6. Frekvenstabell för kombination av driftområde och ärendekategori
# ________________________________________________________________________________________________
# 7. Nollor i data 
# - 7.1 Funktion som tar fram en korstabell med andelen 0:or i data
# - 7.2 Andelen 0:or uppdelat på driftområde
# ________________________________________________________________________________________________


#### 1. Laddar in paketen som krävs ####
load_package<-function(){
  paket<- c("dplyr","tsibble", "tibble", "lubridate", "xgboost", "caret",
            "plyr","ggplot2","forecast","fastDummies","data.table","scales",
            "xtable")                                                        # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])]   # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                  # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                   # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()


# Läser in data
day_data <- read.csv("day_data.csv",  fileEncoding = "windows-1258")
data <- read.csv("bearbetad_data.csv", fileEncoding = "windows-1258")
# Tar fram totala antalet ärenden i per dag för alla driftområden
all_plot_data <- aggregate(day_data$Antal, by=list(Datum=day_data$Tid), FUN=sum)
colnames(all_plot_data)[2]<-"Antal"



#### 2. Enkel visualisering för alla driftområden för hela tidsperioden ####
ggplot(data=all_plot_data, aes(y=Antal, x=Datum, group=1)) + geom_line()



### 3. Enkel beskrivande statistik ####
# Antalet unika driftområden. Tabell 2.1
length(unique(data$Driftområde))

# Antalet unika kategorier. Tabell 2.2
length(unique(data$Kategori))

# Antalet unika anmälda serviceärenden
nrow(data)



#### 4. Stapeldiagram ####
#### 4.1 Stapeldiagram över totala antalet ärenden för repsektive driftområde ####
# Skapar en "räknare"
data$count <- 1
# Tar fram det totala antalet ärenden för repsektive driftområde
plot_data <- aggregate(data$count,
                     FUN=sum,
                     by=list(data$Driftområde))
# Skapar plot
ggplot(plot_data, aes(reorder(Group.1, -x), x)) + geom_col(fill="#b7a075") + theme_bw() +
  coord_flip() + ylab("Antal ärenden") + xlab("") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,15500), breaks = seq(from=0, to=15000, by=2000)) +
  theme(axis.title = element_text(size=17),
        axis.text = element_text(size=16),
        plot.title = element_text(size=18, face = "bold", hjust=0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggtitle("Totala antalet ärenden för respektive driftområde")



#### 4.2 Stapeldiagram över totala antalet ärenden för repsektive ärendekategori ####
# Tar fram det totala antalet ärenden för repsektive ärendekategori
plot_data2 <- aggregate(data$count,
                      FUN=sum,
                      by=list(data$Kategori))
# Skapar plot
ggplot(plot_data2, aes(reorder(Group.1, -x), x)) + geom_col(fill="#b7a075") + theme_bw() +
  coord_flip() + ylab("Antal ärenden") + xlab("") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,13000), breaks = seq(from=0, to=13000, by=3000)) +
  theme(axis.title.x = element_text(size=14),
        axis.text = element_text(size=15),
        plot.title = element_text(size=18, face = "bold", hjust=0.5),
        axis.title = element_text(size=14),
        panel.grid.major.y = element_blank()) +
  ggtitle("Fördelningen över antalet äreden uppdelat på ärendetyper")


#### 5. Tabeller med antal och andel ####
#### 5.1 Totala antalet ärenden för respektive ärendekategorier (med andel) ####
colnames(plot_data2) <- c("Kategori", "Antal_ärdenden")
# Ordnar tabellen efter storlek
plot_data2 <- plot_data2[order(plot_data2$Antal_ärdenden),]
# Tar fram andelen
plot_data2$Andel <- (plot_data2$Antal_ärdenden/sum(plot_data2$Antal_ärdenden))*100
# Lägger till en total
sum_plotdata2 <- data.frame(Kategori="Totalt",
                          Antal_ärdenden=sum(plot_data2$Antal_ärdenden),
                          Andel=sum(plot_data2$Andel))
# Sammanställer tabellen
x_table2 <- rbind(plot_data2, sum_plotdata2)
# Skapar tabell
xtable(x_table2)

#### 5.2 Totala antalet ärenden uppdelat på driftområde (med andel) ####
T1 <- aggregate(data$count, FUN=sum, by=list(data$Driftområde))
colnames(T1) <- c("Driftområde", "Antal")
T1$Andel <- (T1$Antal/sum(T1$Antal))*100
# Lägger till en total
sum_T1 <- data.frame(Driftområde="Totalt",
                     Antal_ärdenden=sum(T1$Antal),
                     Andel=sum(T1$Andel))
# Sammanställer tabellen
x_table3 <- rbind(T1, sum_T1)
# Skapar tabell
xtable(x_table3)

#### 6. Frekvenstabell för kombination av driftområde och ärendekategori ####
aggregate(data$count, by=list(data$Driftområde,
                              data$Kategori), FUN=sum)

#### 7. Nollor i data ####
#### 7.1 Funktion som tar fram en korstabell med andelen 0:or i data ####
andel_nollor <- function(Data){
  # Alla 0:or i materialet
  noll <- Data[Data$Antal==0,]
  # Antalet dagar i matrialet
  fac<-seq(from=ymd(tail(data,1)$Datum), to=ymd(head(data,1)$Datum), by='days')
  # Andelen 0:or
  round(table(noll$Driftområde,noll$Kategori)/length(fac),2)
}
andel_nollor(day_data)



##### 7.2 Andelen 0:or uppdelat på driftområde ####
# Tar fram antalet ärenden för driftområde och dag
dat_drift <- aggregate(day_data$Antal, by=list(day_data$Driftområde, day_data$Tid), FUN=sum)
colnames(dat_drift) <- c("Driftområde", "Datum", "Antal")

# Alla 0:or i materialet
noll <- dat_drift[dat_drift$Antal==0,]
# Antalet dagar i matrialet
fac <- seq(from=ymd(head(dat_drift,1)$Datum), to=ymd(tail(dat_drift,1)$Datum), by="days")
# Antalet 0:or
antal_0or <- table(noll$Driftområde)
# Andelen 0:or
andel_0or <- (table(noll$Driftområde)/length(fac))*100
# Sammanställer tabell
T1$andel_0or <- andel_0or
# Ordnar tabell
T1 <- T1[order(T1$Antal),]
# Skapar tabell
xtable(T1)
