#_______________________________ Innehåll _______________________________
# 1. Laddar data och paket
#________________________________________________________________________
# 2. Figur 2.4 Genomsnittligt antal ärenden per dag uppdelat på månad,
#    för alla driftområden, nov 2015-nov 2021
#________________________________________________________________________
# 3. Figur 2.5 Genomsnittligt antal ärenden per dag uppdelat på månad,
#    för respektive driftområde, nov 2015-nov 2021
#________________________________________________________________________
# 4. Figur 2.6 Genomsnittligt antal ärenden per dag uppdelat på veckodag,
#    för alla driftområden, nov 2015-dec 2021
#________________________________________________________________________
# 5. Figur 2.7 Genomsnittligt antal ärenden per dag uppdelat på veckodag,
#    för respektive driftområde, nov 2015-dec 2021
#________________________________________________________________________


# Aktiverar paket
load_package <- function(){
  paket<- c("fpp3", "forecast", "ggpubr", "ggplot2","tsibbledata","tsbox",
            "tikzDevice","RColorBrewer", "lubridate") # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])] # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                 # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()

# Laddar data
day_data <- read.csv("day_data.csv", fileEncoding = "Windows-1252")

# Tar fram antalet ärenden varje dag för varje driftområde
drift_day <- aggregate(day_data$Antal,
                       by=list(day_data$Driftområde,
                               day_data$Tid),
                       FUN=sum)
colnames(drift_day) <- c("Driftområde", "Datum", "Antal")
drift_day$Datum <- as.Date(drift_day$Datum, format="%Y-%m-%d")



#### 2. Figur 2.4 Genomsnittligt antal ärenden per dag uppdelat på månad, för alla driftområden, nov 2015-nov 2021 ####
# Tar fram antalet ärenden per dag med tillhörande månad och år.
sum_day_month_year <- aggregate(day_data$Antal, by=list(day_data$Tid,
                                                        lubridate::month(day_data$Tid, label=TRUE),
                                                        year(day_data$Tid)),
                                FUN=sum)

# Tar bort december som bara har en dag
sum_day_month_year <- sum_day_month_year[-nrow(sum_day_month_year),]
colnames(sum_day_month_year)<-c("Datum", "Månad", "År", "Antal")
# Beräknar det genomsnittliga antalet ärenden för varje månad (och år)
mean_month_year <- aggregate(sum_day_month_year$Antal,
                           by = list(sum_day_month_year$Månad,
                                            sum_day_month_year$År),
                           FUN=mean)

colnames(mean_month_year)<-c("Månad", "År", "Antal")
mean_month_year$År<-as.character(mean_month_year$År)

# Justerar texten så att inte etiketterna överlappar
below <- c("2018", "2016")
above <- c("2020")
none <- c("2019", "2021", "2017")

under_over1 <- mean_month_year %>% 
  dplyr::mutate(label_below = ifelse(År %in% below, År, ""),
                label_above = ifelse(År %in% above, År, ""),
                label_none = ifelse(År %in% none, År, ""))

# Skapar plot
mean_month_year %>% ggplot() +
  geom_line(aes(x=Månad,y=Antal,color=År, group=År), size=1)+
  scale_colour_grey() +
  labs(y="Antal ärenden",
       title="Genomsnittligt antal ärenden per dag uppdelat på månad",
       subtitle="för alla driftområden, nov 2015-nov 2021",
       x="") + 
  scale_y_continuous(breaks=seq(20,60,5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=15),
        plot.subtitle = element_text(hjust=0.5, size=16),
        panel.grid.minor = element_blank()) +
  geom_point(aes(y=Antal, x=Månad), size=1) +
  geom_text(data = filter(under_over1, Månad=="jan"),
            aes(label = label_below, x = Månad, y = Antal, color = År,
                hjust = 1.05, vjust=0.9),show.legend = FALSE, size=4.5) +
  geom_text(data=filter(under_over1, Månad=="jan"),
            aes(label = label_above, x = Månad, y = Antal, color = År,
                hjust = 1.05, vjust=-0.2),show.legend = FALSE, size=4.5) +
  geom_text(data=filter(under_over1, Månad=="jan"),
            aes(label = label_none, x = Månad, y = Antal, color = År,
                hjust = 1.05, vjust=0.35), show.legend = FALSE, size=4.5)




#### 3. Figur 2.5 Genomsnittligt antal ärenden per dag uppdelat på månad, för respektive driftområde, nov 2015-nov 2021 ####
# Tar fram antalet ärenden varje dag för varje driftområde
drift_month <- aggregate(day_data$Antal, by=list(day_data$Tid,
                                               day_data$Driftområde),
                       FUN=sum)

colnames(drift_month) <- c("Datum", "Driftområde", "Antal")

# Lägger till månaden som en egen variabel
drift_month$Månad <- lubridate::month(drift_month$Datum, label=TRUE)

# Tar bort december som bara har en dag
drift_month <- drift_month %>% filter(Datum < "2021-12-01")

# Tar fram det genomsnittliga antalet ärenden per månad och driftområde.
one_per_month_drift <- aggregate(drift_month$Antal, 
                               by=list(drift_month$Månad,
                                       drift_month$Driftområde), FUN=mean)
colnames(one_per_month_drift)<-c("Månad", "Driftområde", "Antal")

# Färger i plot
col_drift <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
         "#F0E442", "#0072B2", "#D55E00", "#CC79A7", '#e01616')

# Alla driftområden
unique(day_data$Driftområde)

# Lägger till 'D' och siffra för att enklare urskilja driftområdena i diagrammet
one_per_month_drift[one_per_month_drift$Driftområde=="Göteborg/Lindholmen",2]<-"D1 Göteborg/\nLindholmen"
one_per_month_drift[one_per_month_drift$Driftområde=="Hagastaden/City",2]<-"D2 Hagastaden/City"
one_per_month_drift[one_per_month_drift$Driftområde=="Kista/Sundbyberg",2]<-"D3 Kista/Sundbyberg"
one_per_month_drift[one_per_month_drift$Driftområde=="Malmö",2]<-"D4 Malmö"
one_per_month_drift[one_per_month_drift$Driftområde=="Medborgarplatsen/Liljeholmen",2]<-"D5 Medborgarplatsen/\nLiljeholmen"
one_per_month_drift[one_per_month_drift$Driftområde=="Sickla",2]<-"D6 Sickla"
one_per_month_drift[one_per_month_drift$Driftområde=="Slakthusområdet/Proppen",2]<-"D7 Slakthusområdet/\nProppen"
one_per_month_drift[one_per_month_drift$Driftområde=="Slussen",2]<-"D8 Slussen"
one_per_month_drift[one_per_month_drift$Driftområde=="Uppsala",2]<-"D9 Uppsala"

# Justerar etiketterna så att de inte överlappar
below2 <- c("D3", "D2")
above2 <- c("D5", "D9")
none2 <- c("D6", "D4", "D8", "D7", "D1")

under_over2 <- one_per_month_drift %>% 
  dplyr::mutate(label_below = ifelse(substr(one_per_month_drift$Driftområde, start=0, stop=2) %in% below2, substr(one_per_month_drift$Driftområde, start=0, stop=2), ""),
                label_above = ifelse(substr(one_per_month_drift$Driftområde, start=0, stop=2) %in% above2, substr(one_per_month_drift$Driftområde, start=0, stop=2), ""),
                label_none = ifelse(substr(one_per_month_drift$Driftområde, start=0, stop=2) %in% none2, substr(one_per_month_drift$Driftområde, start=0, stop=2), ""))

# Skapar plot
one_per_month_drift %>% ggplot() +
  geom_line(aes(x=Månad, y=Antal, color=Driftområde, group=Driftområde), size=1)+
  scale_color_manual(values=col_drift) + 
  labs(y="Antal ärenden",
       title="Genomsnittligt antal ärenden per dag uppdelat på månad",
       subtitle="för respektive driftområde, nov 2015-nov 2021",
       x="") +
  theme_bw() +
  scale_y_continuous(breaks=seq(1,8,1)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=15),
        plot.subtitle = element_text(hjust=0.5, size=16),
        panel.grid.minor = element_blank(),
        legend.key.height = unit(1, "cm")) +
  geom_point(aes(y=Antal, x=Månad), size=1) +
  geom_text(data=filter(under_over2, Månad=="jan"),
            aes(label = label_below, x = Månad, y = Antal, color = Driftområde,
                hjust = 1.15, vjust=0.8), show.legend = FALSE, size=4.5) +
  geom_text(data=filter(under_over2, Månad=="jan"),
            aes(label = label_above, x = Månad, y = Antal, color = Driftområde,
                hjust = 1.15, vjust=-0.1), show.legend = FALSE, size=4.5) +
  geom_text(data=filter(under_over2, Månad=="jan"),
            aes(label = label_none, x = Månad, y = Antal, color = Driftområde,
                hjust = 1.15, vjust=0.45), show.legend = FALSE, size=4.5)




#### 4. Figur 2.6 Genomsnittligt antal ärenden per dag uppdelat på veckodag, för alla driftområden, nov 2015-dec 2021 ####
# Tar fram antalet ärenden ärenden per veckodag, år och vecka
T1 <- aggregate(day_data$Antal, by=list(Veckodag=lubridate::wday(day_data$Tid, label=TRUE, abbr=FALSE, week_start=1),
                                      År=year(day_data$Tid),
                                      Vecka=isoweek(day_data$Tid)),
              FUN=sum)
# Tar fram det genomsnittliga antalet ärenden per veckodag och år.
T2 <- aggregate(T1$x, by=list(T1$Veckodag, T1$År), FUN=mean)
colnames(T2)<-c("Veckodag", "År", "Antal")
T2$År<-as.character(T2$År)

# Justerar etiketterna så att de inte överlappar
below3 <- c("2021")
above3 <- c("2020")
none3 <- c("2018", "2017", "2016", "2015", "2019")

under_over3 <- T2 %>% 
  dplyr::mutate(label_below = ifelse(År %in% below3, År, ""),
                label_above = ifelse(År %in% above3, År, ""),
                label_none = ifelse(År %in% none3, År, ""))


# Skapar plot
T2 %>% ggplot() +
  geom_line(aes(x=Veckodag,y=Antal,color=År, group=År), size=1)+
  scale_colour_grey() + 
  labs(y="Antal ärenden",
       title="Genomsnittligt antal ärenden per dag uppdelat på veckodag",
       subtitle="för alla driftområden, nov 2015-dec 2021",
       x="") +
  theme_bw() +
  scale_y_continuous(breaks=seq(10,60,10)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=14),
        plot.subtitle = element_text(hjust=0.5, size=16),
        panel.grid.minor = element_blank()) +
  geom_point(aes(y=Antal, x=Veckodag), size=1) +
  geom_text(data=filter(under_over3, Veckodag=="måndag"),
            aes(label = label_above, x = Veckodag, y = Antal, color = År,
                hjust = 1.15, vjust= 1),show.legend = FALSE, size=4.7) +
  geom_text(data=filter(under_over3, Veckodag=="måndag"),
            aes(label = label_below, x = Veckodag, y = Antal, color = År,
                hjust = 1.15, vjust= -0.5),show.legend = FALSE, size=4.7 ) +
  geom_text(data=filter(under_over3, Veckodag=="måndag"),
            aes(label = label_none, x = Veckodag, y = Antal, color = År,
                hjust = 1.15, vjust=0.3), show.legend = FALSE, size=4.7)



#### 5. Figur 2.7 Genomsnittligt antal ärenden per dag uppdelat på veckodag, för respektive driftområde, nov 2015-dec 2021 ####
# Tar fram det genomsnittliga antalet ärenden per veckodag och driftområde
one_per_day_drift <- aggregate(drift_day$Antal, 
                             by=list(lubridate::wday(drift_day$Datum, label=TRUE, week_start = 1, abbr = FALSE),
                                     drift_day$Driftområde), FUN=mean)

colnames(one_per_day_drift)<-c("Veckodag", "Driftområde", "Antal")

# Lägger till 'D' och siffra för att enklare urskilja driftområdena i diagrammet
unique(day_data$Driftområde)
one_per_day_drift[one_per_day_drift$Driftområde=="Göteborg/Lindholmen",2]<-"D1 Göteborg/\nLindholmen"
one_per_day_drift[one_per_day_drift$Driftområde=="Hagastaden/City",2]<-"D2 Hagastaden/City"
one_per_day_drift[one_per_day_drift$Driftområde=="Kista/Sundbyberg",2]<-"D3 Kista/Sundbyberg"
one_per_day_drift[one_per_day_drift$Driftområde=="Malmö",2]<-"D4 Malmö"
one_per_day_drift[one_per_day_drift$Driftområde=="Medborgarplatsen/Liljeholmen",2]<-"D5 Medborgarplatsen/\nLiljeholmen"
one_per_day_drift[one_per_day_drift$Driftområde=="Sickla",2]<-"D6 Sickla"
one_per_day_drift[one_per_day_drift$Driftområde=="Slakthusområdet/Proppen",2]<-"D7 Slakthusområdet/\nProppen"
one_per_day_drift[one_per_day_drift$Driftområde=="Slussen",2]<-"D8 Slussen"
one_per_day_drift[one_per_day_drift$Driftområde=="Uppsala",2]<-"D9 Uppsala"

# Justerar etiketterna så att de inte överlappar
below4 <- c("D5")
above4 <- c("D2")
none4 <- c("D6", "D4", "D3", "D9", "D8", "D1", "D7")

under_over4 <- one_per_day_drift %>% 
  dplyr::mutate(label_below = ifelse(substr(one_per_day_drift$Driftområde, start=0, stop=2) %in% below4, substr(one_per_day_drift$Driftområde, start=0, stop=2), ""),
                label_above = ifelse(substr(one_per_day_drift$Driftområde, start=0, stop=2) %in% above4, substr(one_per_day_drift$Driftområde, start=0, stop=2), ""),
                label_none = ifelse(substr(one_per_day_drift$Driftområde, start=0, stop=2) %in% none4, substr(one_per_day_drift$Driftområde, start=0, stop=2), ""))
# Skapar plot
one_per_day_drift %>% ggplot() +
  geom_line(aes(x=Veckodag,y=Antal,color=Driftområde, group=Driftområde), size=1) +
  scale_color_manual(values=col_drift) +
  labs(y="Antal ärenden",
       title="Genomsnittligt antal ärenden per dag uppdelat på veckodag",
       subtitle="för respektive driftområde, nov 2015-dec 2021",
       x="") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,10,1)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=15),
        plot.subtitle = element_text(hjust=0.5, size=16),
        panel.grid.minor = element_blank(),
        legend.key.height = unit(1, "cm")) +
  geom_point(aes(y=Antal, x=Veckodag), size=1) +
  geom_text(data=filter(under_over4, Veckodag=="måndag"),
            aes(label = label_below, x = Veckodag, y = Antal, color = Driftområde,
                hjust = 1.15, vjust= 1), show.legend = FALSE, size=4.7) + 
  geom_text(data=filter(under_over4, Veckodag=="måndag"),
            aes(label = label_above, x = Veckodag, y = Antal, color = Driftområde,
                hjust = 1.15, vjust= -0.3),show.legend = FALSE, size=4.7 ) +
  geom_text(data=filter(under_over4, Veckodag=="måndag"),
            aes(label = label_none, x = Veckodag, y = Antal, color = Driftområde,
                hjust = 1.15, vjust=0.3), show.legend = FALSE, size=4.7)

