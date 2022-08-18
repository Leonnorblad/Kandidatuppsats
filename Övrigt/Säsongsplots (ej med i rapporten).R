####_____________________________Innehåll___________________________####
# 1. Laddar paket och data
# ____________________________________________________________________
# 2. Säsongsplot - Månadsdata
# - 2.1 Per månad under ett år
# - 2.2 Genomsnittligt per månad under ett år
# ____________________________________________________________________
# 3. Säsongsplot - Veckodata
# - 3.1 Per vecka under ett år
# - 3.2 Genomsnittligt per vecka under ett år
# ____________________________________________________________________
# 4. Säsongsplot - Dagsdata
# - 4.1 Per dag under en vecka
# - 4.2 Genomsnittligt per dag under en vecka
# - 4.3 Konfidensintervall för varje veckodag
# - 4.4 Boxplot för varje veckodag
# - 4.5 Konfidensintervall för varje veckodag uppdelat på driftområden
# - 4.6 Konfidensintervall för varje veckodag uppdelat på kategorier
# ____________________________________________________________________

##### 1. Laddar paket och data ####
load_package<-function(){
  paket<- c("fpp3", "forecast", "ggpubr", "ggplot2",
            "tsibbledata", "dplyr", "Rmisc", "Rcpp",
            "RColorBrewer") # Paket som krävs
  not_installed <- paket[!(paket %in% installed.packages()[ , "Package"])]           # Undersöker vilka som inte finns
  if(length(not_installed)) install.packages(not_installed)                          # Installerar de som inte finns
  invisible(lapply(paket, library, character.only = TRUE))                           # Aktiverar paket 
  print("Laddat och klart!")
}
load_package()

# Laddar data
month_data <- read.csv("month_data.csv")
week_data <- read.csv("week_data.csv")
day_data <- read.csv("day_data.csv")
day_data$Tid <- as.Date(day_data$Tid, format="%Y-%m-%d")
week_data$Tid <- as.Date(week_data$Tid, format="%Y-%m-%d")

#### 2. Säsongsplot - Månadsdata ####
#### 2.1 Per månad under ett år ####
# Tar med den data som har hela månader:
month_data <- month_data[month_data$Tid>="2016-01-01" & month_data$Tid<"2021-11-01",]
# Tar fram totala antalet ärenden för varje månad
month_data_drift<-aggregate(month_data$Antal, by=list(month_data$Tid, month_data$Driftområde), FUN=sum)
colnames(month_data_drift)<-c("Datum", "Driftområde", "Antal")
month_antal<-aggregate(month_data_drift$Antal, by=list(month_data_drift$Datum), FUN=mean)
colnames(month_antal)<-c("Datum", "Antal")
# Skapar tidsserieobjekt
mean_month_ts<-ts(month_antal$Antal, start = c(2016,01), end=c(2021,11), frequency = 12)
# Skapar säsongsplot
ggseasonplot(mean_month_ts, year.labels=TRUE, year.labels.left=TRUE) +
  labs(y="Antal ärenden", x="", title="Genomsnittligt antal ärenden per månad",
       subtitle="För alla driftområden jan 2016-nov 2021") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title.y = element_text(size = 18),
        plot.subtitle = element_text(hjust=0.5, size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
#### Alternativ ####
B1<-aggregate(month_data$Antal, by=list(month(month_data$Tid, label=TRUE),
                                      year(month_data$Tid),
                                      month_data$Driftområde),
              FUN=sum)
B2<-aggregate(B1$x, by=list(B1$Group.1, B1$Group.2), FUN=mean)
colnames(B2)<-c("Månad", "År", "Antal")
B2$År<-as.character(B2$År)
B2 %>% ggplot() +
  geom_line(aes(x=Månad,y=Antal,color=År, group=År))+
  labs(y="Genomsnittligt antal",
       title="Genomsnittligt antal ärenden per månad",
       subtitle="För alla driftområden jan 2016-dec 2021",
       x="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=14),
        plot.subtitle = element_text(hjust=0.5, size=16)) +
  geom_point(aes(y=Antal, x=Månad), size=1)


#### 2.2 Genomsnittligt per månad under ett år ####
# Beräknar genomsnittligt antal ärenden per månad 
one_per_month<-aggregate(month_data$Antal, by=list(month(month_data$Tid, label=TRUE)), FUN=sum)
colnames(one_per_month)<-c("Datum", "Antal")
one_per_month$Antal<-one_per_month$Antal
# Skapar tidsserieobjekt
one_per_month_ts<-ts(one_per_month$Antal, frequency = 12)
# Skapar säsongsplot
ggseasonplot(one_per_month_ts) +
  labs(y="Antal ärenden", x="Månad", title="Säsongsplot total antalet ärenden per månad") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=16),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=12),
        legend.position = "none") + geom_hline(yintercept=mean(one_per_month_ts))

#### Genomsnitt per månad uppdelat på driftområde ####
drift_month<-aggregate(month_data$Antal, by=list(month_data$Driftområde, month_data$Tid),
                       FUN=sum)
colnames(drift_month)<-c("Driftområde", "Datum", "Antal")
one_per_month_drift<-aggregate(drift_month$Antal, 
                             by=list(month(drift_month$Datum, label=TRUE),
                                     drift_month$Driftområde), FUN=mean)
colnames(one_per_month_drift)<-c("Månad", "Driftområde", "Antal")

one_per_month_drift %>% ggplot() +
  geom_line(aes(x=Månad, y=Antal, color=Driftområde, group=Driftområde))+
  labs(y="Genomsnittligt antal",
       title="Genomsnittligt antal ärenden per månad",
       subtitle="Uppdelat på driftområden jan 2016-nov 2021",
       x="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=14),
        plot.subtitle = element_text(hjust=0.5, size=16)) +
  geom_point(aes(y=Antal, x=Månad), size=1)



#### 3. Säsongsplot - Veckodata ####
#### 3.1 Per vecka under ett år ####
week_data<-week_data[week_data$Tid<"2021-11-01",]
# Tar fram totala antalet ärenden för varje vecka
all_week<-aggregate(week_data$Antal, by=list(week_data$Tid), FUN=sum)
colnames(all_week)<-c("Datum", "Antal")
# Skapar tidsserieobjekt
all_week_ts<-ts(all_week$Antal, start = decimal_date(ymd("2015-11-02")), frequency = 52)
# Skapar säsongsplot
ggseasonplot(all_week_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Antal ärenden") + xlab("Vecka") +
  ggtitle("Totala antalet ärenden per vecka uppdelat på år") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=16),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=12))


#### 3.2 Genomsnittligt per vecka under ett år ####
# Beräknar genomsnittligt antal ärenden per vecka 
one_per_week<-aggregate(all_week$Antal, by=list(isoweek(all_week$Datum)), FUN=mean)
colnames(one_per_week)<-c("Datum", "Antal")
# Skapar tidsserieobjekt
one_per_week_ts<-ts(one_per_week$Antal, frequency = 52)
# Skapar säsongsplot
ggseasonplot(one_per_week_ts) +
  ylab("Antal ärenden") +
  ggtitle("Genomsnittligt antal ärenden per vecka") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=16),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=12),
        legend.position = "none") +
  geom_hline(yintercept=mean(one_per_week_ts))



#### 4. Säsongsplot - Dagsdata ####
#### 4.1 Per dag under en vecka ####
# Tar fram totala antalet ärenden för varje dag
day_data<-day_data[day_data$Tid>="2016-01-01" & day_data$Tid<"2021-11-01",]
all_day<-aggregate(day_data$Antal, by=list(day_data$Tid), FUN=sum)
colnames(all_day)<-c("Datum", "Antal")
all_day$Datum <- as.Date(all_day$Datum, format="%Y-%m-%d")
all_day_tib<-as_tsibble(all_day)
# Skapar säsongsplot
all_day_tib %>% gg_season(Antal, period="week") + theme(legend.position = "none") +
  labs(y="Antal", title="Antalet ärenden uppdelat på veckodag", x="",
       subtitle="För alla driftområden jan 2021-dec 2021") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.position = "none",
        plot.subtitle = element_text(hjust=0.5, size=18))
#### testar ####
T1<-aggregate(day_data$Antal, by=list(wday(day_data$Tid, label=TRUE, abbr=FALSE, week_start=1),
                                      year(day_data$Tid),
                                      isoweek(day_data$Tid)),
                                   FUN=sum)
T2<-aggregate(T1$x, by=list(T1$Group.1, T1$Group.2), FUN=mean)
colnames(T2)<-c("Veckodag", "År", "Antal")
T2$År<-as.character(T2$År)
T2 %>% ggplot() +
  geom_line(aes(x=Veckodag,y=Antal,color=År, group=År))+
  labs(y="Genomsnittligt antal",
       title="Genomsnittligt antal ärenden",
       subtitle="per veckodag och år, jan 2016-dec 2021",
       x="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=14),
        plot.subtitle = element_text(hjust=0.5, size=16)) +
  geom_point(aes(y=Antal, x=Veckodag), size=1)


#### 4.2 Genomsnittligt per dag under en vecka ####
# Beräknar genomsnittligt antal ärenden per veckodag
one_per_day<-aggregate(all_day$Antal, by=list(wday(all_day$Datum, label=TRUE, week_start = 1)), FUN=mean)
colnames(one_per_day)<-c("Datum", "Antal")
# För att kunna plotta måste varje observation vara ett datum därför skapas en vecka
# som sedan blir tilldelad varje medelvärde
måndag<-as.Date("2022-03-14", format="%Y-%m-%d")
söndag<-as.Date("2022-03-20", format="%Y-%m-%d")
vecka<-(seq(from=måndag, to=söndag, by="days"))
one_per_day$Datum<-vecka
one_per_day_tib<-as_tsibble(one_per_day)
# Skapar säsongsplot
one_per_day_tib %>% gg_season(Antal, period="week", pal="black") + theme(legend.position = "none") +
  labs(y="Antal", title="Genomsnittligt antal ärenden per veckodag", x="") +
  theme_bw() +
  scale_y_continuous(limits = c(0,60), breaks = seq(from=0, to=60, by=10)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=16),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=12),
        legend.position = "none") +
  geom_point(aes(y=Antal), size=2)


#### Genomsnittligt per dag under en vecka uppdelat på driftområde ####
one_per_day_drift<-aggregate(drift_day$Antal, 
                             by=list(wday(drift_day$Datum, label=TRUE, week_start = 1, abbr = FALSE),
                                     drift_day$Driftområde), FUN=mean)
colnames(one_per_day_drift)<-c("Veckodag", "Driftområde", "Antal")

one_per_day_drift %>% ggplot() +
  geom_line(aes(x=Veckodag,y=Antal,color=Driftområde, group=Driftområde)) +
  labs(y="Genomsnittligt antal",
       title="Genomsnittligt antal ärenden per veckodag och driftområde",
       subtitle="jan 2021-dec 2021",
       x="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=14),
      plot.subtitle = element_text(hjust=0.5, size=16)) +
  geom_point(aes(y=Antal, x=Veckodag), size=1)


#### 4.3 Konfidensintervall för varje veckodag ####
# Beräkar medelvärde, LCL och UCL för totala antalet ärenden per veckodag
all_day_CI <- all_day %>%
  group_by(Veckodag=wday(all_day$Datum, label=TRUE, week_start = 1, abbr = FALSE) ) %>%
  dplyr::summarise(Medel = mean(Antal), 
                   UCL = CI(Antal)[1], 
                   LCL = CI(Antal)[3])
# Skapar säsongsplot
all_day_CI %>%
  ggplot(aes(x = Veckodag, y = Medel)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position = "dodge", width=0.8) +
  geom_point(aes(y=Medel), size=2) +
  labs(y="Antal", title="Genomsnittligt antal ärenden uppdelat på veckodag", x="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=16),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=12),
        legend.position = "none")


#### 4.4 Boxplot för varje veckodag ####
week_days<-wday(all_day$Datum, label=TRUE, week_start = 1, abbr = FALSE)
ggplot(all_day, aes(x=week_days, y=Antal), group=week_days) +
  geom_boxplot(color="black", fill="#b7a075") +
  labs(x="", title = "Antalet ärenden uppdelat på veckodag") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=18),
        axis.title = element_text(size = 15),
        axis.text = element_text(size=14))


#### 4.5 Konfidensintervall för varje veckodag uppdelat på driftområden ####
# Tar fram antalet ärenden för varje driftområde och dag
drift_day<-aggregate(day_data$Antal, by=list(day_data$Driftområde, day_data$Tid), FUN=sum)
colnames(drift_day)<-c("Driftområde", "Datum", "Antal")
# Beräkar medelvärde, LCL och UCL för totala antalet ärenden per veckodag och driftområde
drift_day_CI<-drift_day %>%
  group_by(Veckodag = wday(drift_day$Datum, label=TRUE, week_start = 1, abbr = FALSE),
           Driftområde = drift_day$Driftområde) %>%
  dplyr::summarise(Medel = mean(Antal), 
                   UCL = CI(Antal)[1], 
                   LCL = CI(Antal)[3])
# Skapar säsongsplot uppdelat på driftområden
drift_day_CI %>%
  ggplot(aes(x = Veckodag, y = Medel, fill=Driftområde)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position = "dodge")



#### 4.6 Konfidensintervall för varje veckodag uppdelat på kategorier ####
# Tar fram antalet ärenden för varje kategori och dag
kat_day<-aggregate(day_data$Antal, by=list(day_data$Kategori, day_data$Tid), FUN=sum)
colnames(kat_day)<-c("Kategori", "Datum", "Antal")
# Beräkar medelvärde, LCL och UCL för totala antalet ärenden per veckodag och kategori
kat_day_CI<-kat_day %>%
  group_by(Veckodag = wday(kat_day$Datum, label=TRUE, week_start = 1),
           Kategori = kat_day$Kategori) %>%
  dplyr::summarise(Medel = mean(Antal), 
                   UCL = CI(Antal)[1], 
                   LCL = CI(Antal)[3])
# Skapar säsongsplot uppdelat på kategorier
kat_day_CI %>%
  ggplot(aes(x = Veckodag, y = Medel, fill=Kategori)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position = "dodge")

