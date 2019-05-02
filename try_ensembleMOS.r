#install.packages("ensembleMOS")
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
prcpTestFitCSG0 = ensembleMOScsg0(prcpTestData, trainingDays = 25,
                                   dates = "2008010100")
BScore = brierScore(prcpTestFitCSG0, ensembleData = prcpTestData,
           thresholds = 0)
crpsValues <- crps(prcpTestFitCSG0, prcpTestData)
CRPS_mean = colMeans(crpsValues)
