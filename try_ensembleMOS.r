#install.packages("ensembleMOS")
# https://github.com/cran/ensembleBMA
# https://github.com/cran/ensembleMOS
#install.packages("chron")
#install.packages("evd")
library(ensembleMOS)

data("ensBMAtest", package = "ensembleBMA")
ensMemNames = c("gfs","cmcg","eta","gasp","jma","ngps","tcwb","ukmo")
obs = paste("PCP24", "obs", sep = ".")
ens = paste("PCP24", ensMemNames, sep = ".")
prcpTestData = ensembleData(forecasts = ensBMAtest[,ens],
                             dates = ensBMAtest[,"vdate"],
                             observations = ensBMAtest[,obs],
                             station = ensBMAtest[,"station"],
                             forecastHour = 48,
                             initializationTime = "00")
plot(prcpTestData)
trainingDays = 25
dates = seq(as.Date("2007-12-27"), by = "day", length.out = 7)
dates = format(dates, "%Y%m%d%H")
prcpTestFitCSG0 = ensembleMOScsg0(prcpTestData, trainingDays = trainingDays, dates = dates)
BScore = brierScore(prcpTestFitCSG0, ensembleData = prcpTestData,
           thresholds = 0)
crpsValues = crps(prcpTestFitCSG0, prcpTestData)
CRPS_mean = colMeans(crpsValues)

emosForc = quantileForecast( prcpTestFitCSG0, prcpTestData, dates = dates,  
                             quantiles = c(.1, .25, .5, .75, .9))

lastOnlyTrain = (trainingDays+1)*2
obsFitted = prcpTestData[-c(1:lastOnlyTrain), "observations"]
plot(emosForc[,1], type='l', ylim=c(-0.1, 2.))
for (c in colnames(emosForc[, -1])){
  lines(emosForc[,c], type='l')
}
points(obsFitted, pch=4)

prcpTestFitBMA = ensembleBMAnormal(prcpTestData, trainingDays = trainingDays, dates = dates)
BScoreBMA = brierScore(prcpTestFitBMA, ensembleData = prcpTestData,
                        thresholds = 0)
crpsValuesBMA = crps(prcpTestFitBMA, prcpTestData)
CRPS_mean_BMA = colMeans(crpsValuesBMA)

bmaForc = quantileForecast( prcpTestFitBMA, prcpTestData, dates = dates,  
                             quantiles = c(.1, .25, .5, .75, .9))

plot(bmaForc[,1], type='l', ylim=c(-0.1, 2.))
for (c in colnames(bmaForc[, -1])){
  lines(bmaForc[,c], type='l')
}
points(obsFitted, pch=4)

