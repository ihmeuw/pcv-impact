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


# --------------------------------------------------------------------------------------
# Settings

# 1. cutpoints - (date vector) one or two dates (not tested for all use cases with 1 cutpoint)
#	* current accepted usage: '010413', '010114'
# 2. slope - (logical) whether to have an intercept shift at the cutpoints or a slope shift
#	* current accepted usage: TRUE
# 3. new_effect_date - date at which to compute the effect size. NULL defaults to cutpoint 1
#	* current accepted usage: '2016-06-01'
# 4. bma_dual - (logical) whether to run BMA on ITS models that use two or one cutpoints
#	* current accepted usage: FALSE
# 5. run_name - (character) extra information to describe this run. Alters file names
#	* current accepted usage: ''
# 6. graphITS - (logical) whether to write a pdf containing graphs from the basic ITS
# 7. graphBMA - (logical) whether to write a pdf containing graphs from the BMA
# 8. graphBMADiagnostics - (logical) whether to write lots of other BMA graphs to the same pdf (superseded by graphBMA)

cutpoints = as.Date(c('010413', '010114'), '%d%m%y')
slope = TRUE
new_effect_date = as.Date('2016-06-01')
bma_dual = TRUE
run_name = '_3peice_w_modeluncertainty'
graphITS = FALSE
graphBMA = TRUE
graphBMADiagnostics = FALSE
# --------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Files, directories

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

# output data files
itsOutputFile = paste0(root, 'data/output/its_results', run_name, '.rdata')
bmaOutputFile = paste0(root, 'data/output/its_results', run_name, '.rdata')

# graph files
itsFile = paste0(root, 'visualizations/its_results', run_name, '.pdf')
bmaFile = paste0(root, 'visualizations/bma_results', run_name, '.pdf')

# list of outcome variables
outcomes = c('ipd_cases', 'ipd_pcv10_serotype_cases', 
				'ipd_non_pcv10_serotype_cases', 'xrcp_cases')
				
# sequence and combinatorics of cutpoints
firstCut = cutpoints[1]
lastCut = cutpoints[2]
cutpointSeries = seq(from=firstCut, to=lastCut, by='month')
cutpointCombinatorics = as.Date(combn(cutpointSeries, 2), origin='1970-01-01')
# ----------------------------------------------------------------------------


# ----------------------------------------------
# Load/prep data
inputData = prepData(paste0(root, 'data/input'))
# ----------------------------------------------


# -----------------------------------------------------------------------------------------
# Execute analysis

# basic ITS across outcomes with two cutpoints
itsOutcomeResults = vector('list', length(outcomes)) 
for(o in seq(length(outcomes))) {
	itsOutcomeResults[[o]] = its(data=inputData, outcome=outcomes[o], cutpoint=cutpoints, 
									slope=slope, newEffectDate=new_effect_date)
}

# basic ITS across all possible single cutpoints 
# (within the window defined by the first and last cutpoints)
if (!bma_dual) { 
	itsCutpointResults1 = vector('list', length(cutpointSeries)*length(outcomes)) 
	i=1
	for(o in seq(length(outcomes))) {
		for(c in seq(length(cutpointSeries))) {		
			# run ITS on the current cutpoint
			itsCutpointResults1[[i]] = its(data=inputData, outcome=outcomes[[o]], 
				cutpoint=cutpointSeries[c], slope=slope, newEffectDate=new_effect_date)
			i=i+1
		}
	}
}

# basic ITS across all possible pairs of cutpoints 
# (within the window defined by the first and last cutpoints)
if (bma_dual) { 
	itsCutpointResults2 = vector('list', ncol(cutpointCombinatorics)*length(outcomes))
	i=1
	for(o in seq(length(outcomes))) {
		for(c in seq(ncol(cutpointCombinatorics))) {
			itsCutpointResults2[[i]] = its(data=inputData, outcome=outcomes[[o]], 
				cutpoint=cutpointCombinatorics[,c], slope=slope, newEffectDate=new_effect_date)
			i=i+1
		}
	}
}

# BMA of ITS across cut points (single or dual controlled by settings)
bmaResults = vector('list', length(outcomes))
if (!bma_dual) bmaInput = itsCutpointResults1
if (bma_dual) bmaInput = itsCutpointResults2
for(o in seq(length(outcomes))) { 
	# indices of combinatorics for this outcome
	if (!bma_dual) { 
		i1 = (o-1)*length(cutpointSeries) + 1
		i2 = o*length(cutpointSeries)
	}
	if (bma_dual) { 
		i1 = (o-1)*ncol(cutpointCombinatorics) + 1
		i2 = o*ncol(cutpointCombinatorics)
	}
	
	# average models for the current outcome
	bmaResults[[o]] = bma(bmaInput[i1:i2])
}
# -----------------------------------------------------------------------------------------


# ------------------------------------------
# Save output data
save(itsOutcomeResults, file=itsOutputFile)
save(bmaResults, file=bmaOutputFile)
# ------------------------------------------


# --------------------------------------------------------------------------------------------------------------
# Graph

# basic ITS by outcome
if (graphITS) { 
	pdf(itsFile, height=6, width=10)
	for(o in seq(length(outcomes))) plot(graph(itsOutput=itsOutcomeResults[[o]], quarterly=FALSE))
	dev.off()
}

# BMA
if (graphBMA) { 
	pdf(bmaFile, height=6, width=10)

	# graph bma result
	for(o in seq(length(outcomes))) plot(graph(itsOutput=bmaResults[[o]], quarterly=FALSE))

	if(graphBMADiagnostics) { 
		# graph bma weights, effects and uncertainty
		tmpData = copy(bmaResults$stats)
		tmpData[, effect:=100-(exp(effect)*100)]
		setnames(tmpData, c('weight', 'effect', 'effect_se'), c('Model Weight', '% Reduction', 'Effect Standard Error'))
		tmpData = melt(tmpData, id.vars='cutpoint', measure.vars=c('Model Weight', '% Reduction', 'Effect Standard Error'))
		ggplot(tmpData, aes(y=value, x=cutpoint)) +
				geom_line() + geom_point() + facet_wrap(~variable, scales='free') +
				labs(title='BMA Weights', y='Weight (Uniform Prior)', x='Cutpoint') + theme_bw()
				
		# graph individual results that went into bma
		for(c in seq(length(itsCutpointResults))) plot(graph(itsOutput=itsCutpointResults[[c]], quarterly=TRUE))
	}

	dev.off()
}
# --------------------------------------------------------------------------------------------------------------
