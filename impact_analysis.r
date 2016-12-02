# -----------------------------------------------
# David Phillips
#
# 3/1/2016
# Analysis of PCV impact using Manhica DSS data
# -----------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(readxl)
library(reshape2)
library(MASS)
library(stats4)
library(ggplot2)
# ------------------


# --------------------------------------------------------------
# Files, directories and settings

# change to code directory
codeDir = 'C:/local/mixed-methods-analysis/pcv_impact/code/'
setwd(codeDir)

# load functions
source(paste0(codeDir, 'prepData.r'))
source(paste0(codeDir, 'its.r'))
source(paste0(codeDir, 'bma.r'))
# source(paste0(codeDir, 'cpbma.r'))
source(paste0(codeDir, 'graph.r'))

# root input/output directory
root = 'J:/Project/Evaluation/GAVI/Mozambique/pcv_impact/'

# graph files
itsOutcomeFile = paste0(root, 'output/its_results_slope.pdf')
bmaFile = paste0(root, 'output/bma_results_slope.pdf')

# list of outcome variables
outcomes = c('ipd_cases', 'ipd_pcv10_serotype_cases', 
				'ipd_non_pcv10_serotype_cases', 'xrcp_cases')
# --------------------------------------------------------------


# ----------------------------------------
# Load/prep data
inputData = prepData(paste0(root, 'data'))
# ----------------------------------------


# ------------------------------------------------------------------------------------------------------------------------
# Execute analysis

# basic ITS across outcomes with two slopes
cutpoints = as.Date(c('010413', '010114'), '%d%m%y')
itsOutcomeResults = vector('list', length(outcomes)) 
for(o in seq(length(outcomes))) {
	itsOutcomeResults[[o]] = its(data=inputData, outcome=outcomes[o], cutpoint=cutpoints, 
									slope=TRUE, newEffectDate=as.Date('2016-06-01'))
}

# basic ITS across cut points
firstCut = cutpoints[1]
lastCut = cutpoints[2]
cutpoints = seq(from=firstCut, to=lastCut, by='month')
itsCutpointResults = vector('list', length(cutpoints)) 
for(o in seq(length(outcomes))) {
	for(c in seq(length(cutpoints))) {
		itsCutpointResults[[((length(cutpoints)*(o-1))+c)]] = its(data=inputData, 
																	outcome=outcomes[[o]], 
																	cutpoint=cutpoints[c], 
																	slope=TRUE,
																	newEffectDate=as.Date('2016-06-01'))
	}
}

# basic ITS across cutpoints with two slopes
# cutpoints = as.Date(combn(cutpoints, 2), origin='1970-01-01')
# itsCutpointResults = vector('list', ncol(cutpoints)) 
# for(c in seq(ncol(cutpoints))) {
	# itsCutpointResults[[c]] = its(data=inputData, outcome='ipd_pcv10_serotype_cases', 
									# cutpoint=cutpoints[,c], slope=TRUE, newEffectDate=as.Date('2016-06-01'))
# }

# BMA of ITS across cut points
bmaResults = vector('list', length(outcomes))
for(o in seq(length(outcomes))) bmaResults[[o]] = bma(itsCutpointResults[((length(cutpoints)*(o-1))+1):(length(cutpoints)*o)])
# ------------------------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------------------
# Graph

# basic ITS by outcome
pdf(itsOutcomeFile, height=6, width=10)
for(o in seq(length(outcomes))) plot(graph(itsOutput=itsOutcomeResults[[o]], quarterly=TRUE))
dev.off()

# BMA
pdf(bmaFile, height=6, width=10)

# graph bma result
for(o in seq(length(outcomes))) plot(graph(itsOutput=bmaResults[[o]], quarterly=TRUE))

# # graph bma weights, effects and uncertainty
# tmpData = copy(bmaResults$stats)
# tmpData[, effect:=100-(exp(effect)*100)]
# setnames(tmpData, c('weight', 'effect', 'effect_se'), c('Model Weight', '% Reduction', 'Effect Standard Error'))
# tmpData = melt(tmpData, id.vars='cutpoint', measure.vars=c('Model Weight', '% Reduction', 'Effect Standard Error'))
# ggplot(tmpData, aes(y=value, x=cutpoint)) +
		# geom_line() + geom_point() + facet_wrap(~variable, scales='free') +
		# labs(title='BMA Weights', y='Weight (Uniform Prior)', x='Cutpoint') + theme_bw()
		
# # graph individual results that went into bma
# for(c in seq(length(itsCutpointResults))) plot(graph(itsOutput=itsCutpointResults[[c]], quarterly=TRUE))
dev.off()
# --------------------------------------------------------------------------------------------------------------
