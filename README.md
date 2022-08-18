# Kandidatuppsats - Prognostisering av serviceärenden
### En jämförelse av statistiska metoder för tidsseriemodellering av ärendeflödet i hyresfastigheter
#### Av Leonard Persson Norblad och Isabella Roos
Uppsatsen skrevs våren 2022 för fastighetsbolaget Atrium Ljungberg. Syftet var att optimera personalfördelningen av Atrium Ljungbergs drifttekniker.
Eftersom mängden personal som behövs beror på antalet inkommande ärenden så användes historisk data av anmälda serviceärenden för att skapa prediktiva modeller som prognostiserade ärendeflödet.
I studien jämfördes statistisk tidsseriemodellering SARIMA, dynamisk regression (linjär regression och SARIMA) samt maskininlärningsmetoden XGBoost. Fulltext finns som pfd.



## Abstract
In order to keep a high standard in real estates owned by Atrium Ljungberg, they give their tenants the
possiblity to communicate when there is an issue with the property they are renting. By optimising the staff
according to the volume of real estate matters, the processing time can be minimised and the company can
avoid over- and understaffing. This could result in more satisfied tenants and less workload for the caretakers.
With the help of statistical methods the number of real estates matters could be forecasted. The purpose of
this thesis was to build statisitcal models that could forecast the number of real estate matters in different
operational areas, for the next 14 days.

The data that were used for analysis contained observations between november 2015 and december 2021 for
the nine different operational areas. Three different types of models were compared to find the most optimal
one for each operational area. The three types that were used was ARIMA, Dynamic regression models and
XGBoost. The models were compared with RMSE and the one with the lowest RMSE for most forecast
horizons was chosen as the best one. After the nine different models were chosen they were evaluated with
MAE which measures the mean absolute error. This measurement is appropriate to use when studying the
average number of errors made by the model.

The outcome showed that there were small differences in RMSE between the models within the different
operational areas, which made it somewhat difficult to find the most ideal one. The conclusion was that
XGBoost preformed best in five of the nine areas, and the dynamic regression model including two types
of dummyvariables performed best in two of the areas. In the two remaining areas the dynamic regression
model with one type of dummyvariabel and SARIMA model performed best.

