# ---------------------------------------------------------
# David Phillips
#
# 12/4/2016
# Sensitivity analysis
# Assembles output from multiple runs of impact_analysis.r
# ---------------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
# -------------------


# -----------------------------------------------------------------------------------------
# Files, directories, settings and lists

# load impact analysis function
source('impact_analysis.r')

# settings
run_name = 'lead_in_variants_2piece'
ind_run_name = '_lead_in'
reRunModels = FALSE

# change to code directory
if (Sys.info()[1]=='Windows') codeDir = 'C:/local/mixed-methods-analysis/pcv_impact/code/'
if (Sys.info()[1]!='Windows') codeDir = './'
setwd(codeDir)

# root input/output directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
root = paste0(j, '/Project/Evaluation/GAVI/Mozambique/pcv_impact/')

# graph files
graphFile = paste0(root, 'visualizations/sensitivity_', run_name, '.pdf')

# output data files (from impact_analysis.r)
outputFileStub = paste0(root, 'data/output/bma_results')
# -----------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Run impact_analysis.r on varying windows

# store window end dates
endDates = seq(from=as.Date('2013-05-01'), to=as.Date('2016-04-01'), by='quarter')

# store lead in start dates
endDates = seq(from=as.Date('2003-01-01'), to=as.Date('2012-01-01'), by='quarter')

# run for each date
if (reRunModels) { 
	modelOutput = lapply(endDates, function(endDate) { 
		bmaResults = impactAnalysis(leadInDate=endDate, 
										run_name=paste0(as.character(endDate), ind_run_name), 
										rePrepData=TRUE)
		return(bmaResults)
	})
}

# load all files from a previous run
if (!reRunModels) { 
	files = paste0(outputFileStub, endDates, ind_run_name, '.rdata')
	modelOutput = lapply(files, function(x) { 
		load(x)
		return(bmaResults)
	})
}
# -------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# Prep data

# assemble effect estimates into a workable data table
assembleData = function(objName) { 
	output = data.table()
	for(run in seq(length(modelOutput))) {
		for(outcome in seq(length(modelOutput[[run]]))) {
			tmp = modelOutput[[run]][[outcome]][[objName]]
			tmpOut = modelOutput[[run]][[outcome]]$outcome
			tmp = data.table(tmp)
			tmp[, run:=endDates[run]]
			tmp[, outcome:=tmpOut]
			output = rbind(output, tmp, fill=TRUE)
		}
	}
	return(output)
}
effectSizes = assembleData('effect_size')

# prep effect sizes/labels
effectSizes[, est:=c('Estimate', 'Upper', 'Lower', 'se')]
effectSizes = effectSizes[est!='se']
effectSizes[, id:=seq_len(.N)]
effectSizes[, run_id:=year(run)+((month(run)-1)/12)]
effectSizes[, effect:=(exp(effect)*100)-100]
effectSizes[outcome=='ipd_cases', outcome_label:='All IPD Cases']
effectSizes[outcome=='ipd_pcv10_serotype_cases', outcome_label:='PCV10 Serotypes']
effectSizes[outcome=='ipd_non_pcv10_serotype_cases', outcome_label:='Non-PCV10 Serotypes']
effectSizes[outcome=='xrcp_cases', outcome_label:='All X-Ray Confirmed Cases']
effectSizes[effect>250, effect:=NA]
effectSizes[effect< -250, effect:=NA]

# assemble fitted values into a workable data table
fittedValues = assembleData('data')

# prep fitted values/labels
fittedValues[, id:=seq_len(.N)]
fittedValues[, run_id:=year(run)+((month(run)-1)/12)]
fittedValues[outcome=='ipd_cases', outcome_label:='All IPD Cases']
fittedValues[outcome=='ipd_pcv10_serotype_cases', outcome_label:='PCV10 Serotypes']
fittedValues[outcome=='ipd_non_pcv10_serotype_cases', outcome_label:='Non-PCV10 Serotypes']
fittedValues[outcome=='xrcp_cases', outcome_label:='All X-Ray Confirmed Cases']
fittedValues[outcome=='ipd_cases', est:=ipd_cases_pred]
fittedValues[outcome=='ipd_pcv10_serotype_cases', est:=ipd_pcv10_serotype_cases_pred]
fittedValues[outcome=='ipd_non_pcv10_serotype_cases', est:=ipd_non_pcv10_serotype_cases_pred]
fittedValues[outcome=='xrcp_cases', est:=xrcp_cases_pred]
fittedValues[outcome=='ipd_cases', cases:=ipd_cases]
fittedValues[outcome=='ipd_pcv10_serotype_cases', cases:=ipd_pcv10_serotype_cases]
fittedValues[outcome=='ipd_non_pcv10_serotype_cases', cases:=ipd_non_pcv10_serotype_cases]
fittedValues[outcome=='xrcp_cases', cases:=xrcp_cases]

# collapse to quarter level
quarterData = fittedValues[,c('outcome','moyr','cases'),with=F]
quarterData[month(moyr)<4, moyr:=as.Date(paste0('0102', year(moyr)), '%d%m%Y')]
quarterData[month(moyr)>=4 & month(moyr)<7, moyr:=as.Date(paste0('0105', year(moyr)), '%d%m%Y')]
quarterData[month(moyr)>=7 & month(moyr)<10, moyr:=as.Date(paste0('0108', year(moyr)), '%d%m%Y')]
quarterData[month(moyr)>=10, moyr:=as.Date(paste0('0111', year(moyr)), '%d%m%Y')]
quarterData = quarterData[, list(mean=mean(cases)), by=c('outcome', 'moyr')]
fittedValues = merge(fittedValues, quarterData, c('outcome', 'moyr'), all=TRUE)
fittedValues[, cases:=mean]
# -----------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------
# Graph

# graph settings
colors1 = brewer.pal(3, 'Paired')
colors2 = brewer.pal(6, 'GnBu')[-1]

# open pdf
pdf(graphFile, width=10, height=6)

# graph effect sizes
p = ggplot(effectSizes[est=='Estimate'], aes(y=effect, x=run_id)) + 
	geom_hline(yintercept=0, color='red') + 
	geom_point(, alpha=.6) + 
	geom_point(data=effectSizes[est=='Lower'], alpha=.6) + 
	geom_point(data=effectSizes[est=='Upper'], alpha=.6) + 
	geom_smooth(se=FALSE, aes(color='Estimate'), size=1.25) + 
	geom_smooth(data=effectSizes[est=='Lower'], se=FALSE, aes(color='Upper/Lower'), size=1.25) + 
	geom_smooth(data=effectSizes[est=='Upper'], se=FALSE, aes(color='Upper/Lower'), size=1.25) + 
	facet_wrap(~outcome_label, scales='free_y') + 
	scale_color_manual('', breaks=c('Estimate', 'Upper/Lower'), values=c(colors1[2], colors1[1])) + 
	labs(title='Effect Size with Varying Lead-In Period', y='Effect Size (% Change)', x='Lead-In Period (Start)') + 
	theme_bw()
print(p)
	
# graph fitted values
p = ggplot(fittedValues, aes(y=est, x=moyr, color=run_id, group=run_id)) + 
	geom_line() + 
	geom_point(aes(y=cases), color='#2D358E') + 
	geom_vline(xintercept=as.numeric(as.Date('2013-04-01')), linetype=5, color='#C0C0C0') +
	annotate('text', label='PCV Introduction', x=as.Date('2013-04-01'), y=Inf, hjust=1.1, size=3, hjust=1, vjust=-.25, angle=90) +
	facet_wrap(~outcome_label, scales='free_y') + 
	labs(title='Fitted Values with Varying Lead-In Period', y='Expected Cases', x='') + 
	scale_color_gradientn('Lead-In\nPeriod\n(Start)', colors=colors2) + 
	theme_bw()
print(p)

# graph fitted values for PCV10 serotypes only
p = ggplot(fittedValues[outcome=='ipd_pcv10_serotype_cases'], aes(y=est, x=moyr, color=run_id, group=run_id)) + 
	geom_line() + 
	geom_point(aes(y=cases), color='#2D358E') + 
	geom_vline(xintercept=as.numeric(as.Date('2013-04-01')), linetype=5, color='#C0C0C0') +
	annotate('text', label='PCV Introduction', x=as.Date('2013-04-01'), y=Inf, hjust=1.1, size=3, hjust=1, vjust=-.25, angle=90) +
	labs(title='Fitted Values with Varying Lead-In Period\nPCV10 Serotypes', y='Expected Cases', x='') + 
	scale_color_gradientn('Lead-In\nPeriod\n(Start)', colors=colors2) + 
	theme_bw()
print(p)

# close pdf
dev.off()
# ---------------------------------------------------------------------------------------------------------
