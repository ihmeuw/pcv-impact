# -----------------------------------------------
# David Phillips
#
# 3/1/2016
# Analysis of PCV impact using Manhica DSS data
# -----------------------------------------------

# --------------------------------------------------------------------------------------------------------------------
# Arguments
# 1. cutpoints - (date vector) one or two dates (not tested for all use cases with 1 cutpoint)
# 2. slope - (logical) whether to have an intercept shift at the cutpoints or a slope shift
# 3. new_effect_date - date at which to compute the effect size. NULL defaults to cutpoint 1
# 4. bma_dual - (logical) whether to run BMA on ITS models that use two or one cutpoints
# 5. run_name - (character) extra information to describe this run. Alters file names
# 6. saveITS - (logical) whether to save output from the basic ITS
# 7. saveBMA - (logical) whether to save output from the BMA
# 8. saveBMADiagnostics - (logical) whether to write lots of other BMA graphs to the same pdf (superseded by graphBMA)
# 9. quarterly - (logical) whether to display average cases per quarter (TRUE) or total cases per month
# 10. rePrepData - (logical) whether to re-run the prep code or just load the file from the last run
# 11. leadInDate - (date) any data before this date will be dropped (if rePrepData==TRUE)
# --------------------------------------------------------------------------------------------------------------------

# wrap as a function (arguments will over-ride settings below)
impactAnalysis = function(cutpoints=as.Date(c('2013-04-01', '2014-01-01')), slope=TRUE, 
							new_effect_date=as.Date('2016-06-01'), bma_dual=FALSE, 
							run_name='', saveITS=FALSE, saveBMA=TRUE, saveBMADiagnostics=FALSE, 
							quarterly=TRUE, rePrepData=FALSE, leadInDate=as.Date('2008-01-01')) {
	
	# --------------------------------------------------------------
	# Assign arguments globally (don't hate)
	args = c('cutpoints', 'slope', 'new_effect_date', 'bma_dual', 
			'run_name', 'saveITS', 'saveBMA', 'saveBMADiagnostics',
			'quarterly', 'rePrepData', 'leadInDate')
	for(arg in args)  assign(arg, get(arg), envir=globalenv())
	# --------------------------------------------------------------
	
	
	# ------------------------
	# Set up R
	rm(list=ls()[!ls() %in% 
		c('args', args)])
	library(data.table)
	library(readxl)
	library(reshape2)
	library(MASS)
	library(stats4)
	library(ggplot2)
	# ------------------------
	
	
	# ----------------------------------------------------------------------------
	# Files, directories
	
	# change to code directory
	if (Sys.info()[1]=='Windows') codeDir = 'C:/local/mixed-methods-analysis/pcv_impact/code/'
	if (Sys.info()[1]!='Windows') codeDir = './'
	setwd(codeDir)
	
	# load functions
	source(paste0(codeDir, 'prepData.r'))
	source(paste0(codeDir, 'its.r'))
	source(paste0(codeDir, 'bma.r'))
	# source(paste0(codeDir, 'cpbma.r'))
	source(paste0(codeDir, 'graph.r'))
	
	# root input/output directory
	j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
	root = paste0(j, '/Project/Evaluation/GAVI/Mozambique/pcv_impact/')
	
	# prepped data file
	preppedDataFile = paste0(root, 'data/output/prepped_data.rdata')
	
	# output data files
	itsOutputFile = paste0(root, 'data/output/its_results', run_name, '.rdata')
	bmaOutputFile = paste0(root, 'data/output/bma_results', run_name, '.rdata')
	
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
	
	
	# -------------------------------------------------------------------------------
	# Load/prep data
	if (rePrepData) inputData = prepData(paste0(root, 'data/input'), preppedDataFile)
	if (!rePrepData) load(preppedDataFile)
	# -------------------------------------------------------------------------------
	
	
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
	
	
	# --------------------------------------------------------------------------------------------------------------
	# Graph
	
	# basic ITS by outcome
	if (saveITS) { 
		pdf(itsFile, height=6, width=10)
		for(o in seq(length(outcomes))) plot(graph(itsOutput=itsOutcomeResults[[o]], quarterly=quarterly))
		dev.off()
	}
	
	# BMA
	if (saveBMA) { 
		pdf(bmaFile, height=6, width=10)
		
		# graph bma result
		for(o in seq(length(outcomes))) plot(graph(itsOutput=bmaResults[[o]], quarterly=quarterly))
		
		if(saveBMADiagnostics) { 
			for(o in seq(length(outcomes))) { 
				# graph bma weights, effects and uncertainty
				tmpData = copy(bmaResults[[o]]$stats)
				tmpData[, effect:=100-(exp(effect)*100)]
				setnames(tmpData, c('weight', 'effect', 'effect_se'), c('Model Weight', '% Reduction', 'Effect Standard Error'))
				idVars = 'cutpoint'
				if (bma_dual) idVars = c(idVars, 'cutpoint2')
				tmpData = melt(tmpData, id.vars=idVars, measure.vars=c('Model Weight', '% Reduction', 'Effect Standard Error'))
				
				if (!bma_dual) {
					p = ggplot(tmpData, aes(y=value, x=cutpoint)) +
						geom_point() + facet_wrap(~variable, scales='free') +
						labs(title='BMA Weights', y='Weight (Uniform Prior)', x='Cutpoint') + theme_bw()
				}
				if (bma_dual) {
					p = ggplot(tmpData, aes(y=value, x=cutpoint2, color=cutpoint)) +
						geom_point() + facet_wrap(~variable, scales='free') +
						labs(title='BMA Weights', y='Weight (Uniform Prior)', x='Window End') + theme_bw()
				}
				print(p)
				
				# graph individual results that went into bma
				for(c in seq(length(bmaInput))) plot(graph(itsOutput=bmaInput[[c]], quarterly=quarterly))
			}
		}
		
		dev.off()
	}
	# --------------------------------------------------------------------------------------------------------------
	
	
	# ------------------------------------------------------
	# Save and return output data
	if (saveITS) save(itsOutcomeResults, file=itsOutputFile)
	if (saveBMA) save(bmaResults, file=bmaOutputFile)
	if (saveITS & !saveBMA) return(itsOutcomeResults)
	if (saveBMA) return(bmaResults)
	# ------------------------------------------------------
}
