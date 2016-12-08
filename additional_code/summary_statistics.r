# load data
rm(list=ls())
library(ggplot2)
library(data.table)
load('J:/Project/Evaluation/GAVI/Mozambique/pcv_impact/data/output/prepped_data_2003_leadin.rdata')

# number of observations
summary(inputData$moyr)
length(unique(inputData$moyr))

# total cases observed
sum(inputData$ipd_cases)
sum(inputData$xrcp_cases, na.rm=TRUE)

# mean cases per month
inputData[, lapply(.SD, mean, na.rm=TRUE), .SDcols=c('ipd_cases', 'ipd_pcv10_serotype_cases', 'ipd_non_pcv10_serotype_cases', 'xrcp_cases')]
inputData[, lapply(.SD, sd, na.rm=TRUE), .SDcols=c('ipd_cases', 'ipd_pcv10_serotype_cases', 'ipd_non_pcv10_serotype_cases', 'xrcp_cases')]

# compare the mean before and after
inputData[, cat:='1. Before PCV']
inputData[moyr>as.Date('2013-04-01'), cat:='2. After PCV']
lmFit1 = lm(ipd_cases ~ cat, inputData)
lmFit2 = lm(ipd_pcv10_serotype_cases ~ cat, inputData)
lmFit3 = lm(ipd_non_pcv10_serotype_cases ~ cat, inputData)
lmFit4 = lm(xrcp_cases ~ cat, inputData)
outPut1 = predict(lmFit1, newdata=newData, interval='confidence')
outPut2 = predict(lmFit2, newdata=newData, interval='confidence')
outPut3 = predict(lmFit3, newdata=newData, interval='confidence')
outPut4 = predict(lmFit4, newdata=newData, interval='confidence')
outPut = data.table(rbind(outPut1, outPut2, outPut3, outPut4))
outPut[, period:=c('before', 'after')]
outPut[, outcome:=rep(c('All IPD', 'PCV10 Serotype IPD', 'Non-PCV10 Serotype IPD', 'XRCP'), each=2)]
outPut = melt(outPut, id.vars=c('period','outcome'))
outPut = dcast.data.table(outPut, outcome ~ period + variable)
outPut = outPut[, lapply(.SD, round, 2), .SDcols=names(outPut)[names(outPut)!='outcome'], by='outcome']
outPut[, ('Before PCV'):=as.character(paste0(before_fit, ' (', before_lwr, '-', before_upr, ')'))]
outPut[, ('After PCV'):=as.character(paste0(after_fit, ' (', after_lwr, '-', after_upr, ')'))]
outPut

# cases averted between 2014 and June 2016
load('J:/Project/Evaluation/GAVI/Mozambique/pcv_impact/data/output/bma_results_3_piece.rdata')
allIPDdata = bmaResults[[1]]$data
expectedCases = allIPDdata[moyr>=as.Date('2013-04-01'), sum(ipd_cases_pred_cf)]
expectedCasesLower = allIPDdata[moyr>=as.Date('2013-04-01'), sum(ipd_cases_pred_cf_lower)]
expectedCasesUpper = allIPDdata[moyr>=as.Date('2013-04-01'), sum(ipd_cases_pred_cf_upper)]
observedCases = allIPDdata[moyr>=as.Date('2013-04-01'), sum(ipd_cases)]
ipdAverted = data.table('cases_averted'=expectedCases-observedCases, 'lower'=expectedCasesLower-observedCases, 'upper'=expectedCasesUpper-observedCases)

pcv10IPDdata = bmaResults[[2]]$data
expectedCases = pcv10IPDdata[moyr>=as.Date('2013-04-01'), sum(ipd_pcv10_serotype_cases_pred_cf)]
expectedCasesLower = pcv10IPDdata[moyr>=as.Date('2013-04-01'), sum(ipd_pcv10_serotype_cases_pred_cf_lower)]
expectedCasesUpper = pcv10IPDdata[moyr>=as.Date('2013-04-01'), sum(ipd_pcv10_serotype_cases_pred_cf_upper)]
observedCases = pcv10IPDdata[moyr>=as.Date('2013-04-01'), sum(ipd_pcv10_serotype_cases)]
pcv10STAverted = data.table('cases_averted'=expectedCases-observedCases, 'lower'=expectedCasesLower-observedCases, 'upper'=expectedCasesUpper-observedCases)

allAverted = rbind(ipdAverted, pcv10STAverted)
allAverted = round(allAverted)
allAverted$outcome=c('All IPD', 'PCV10 Serotype IPD')
allAverted
