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

# settings
run_name = 'window_variants_3peice'

# change to code directory
if (Sys.info()[1]=='Windows') codeDir = 'C:/local/mixed-methods-analysis/pcv_impact/code/'
if (Sys.info()[1]!='Windows') codeDir = './'
setwd(codeDir)

# root input/output directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
root = paste0(j, '/Project/Evaluation/GAVI/Mozambique/pcv_impact/')

# graph files
graphFile = paste0(root, 'visualizations/sensitivity_', run_name, '.pdf')

# run names for output files (from impact_analysis) to load in here
input_names = c('3peice_narrow', '3peice_mednarrow', '3peice_medium', 
				'3peice_medwide', '3peice_wide', '3peice_max3')

# output data files (from impact_analysis.r)
outputFileStub = paste0(root, 'data/output/bma_results_')
# -----------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# Load/prep data

# load all files
modelOutput = lapply(paste0(outputFileStub, input_names, '.rdata'), function(x) { 
	load(x)
	return(bmaResults)
})

# assemble effect estimates into a workable data table
effectSizes = data.table()
for(run in seq(length(modelOutput))) {
	for(outcome in seq(length(modelOutput[[run]]))) {
		tmp = modelOutput[[run]][[outcome]]$effect_size
		tmpOut = modelOutput[[run]][[outcome]]$outcome
		tmp = data.table(tmp)
		tmp[, run:=input_names[run]]
		tmp[, outcome:=tmpOut]
		effectSizes = rbind(effectSizes, tmp)
	}
}

# prep effect sizes/labels
effectSizes[, est:=c('Estimate', 'Upper', 'Lower', 'se')]
effectSizes = effectSizes[est!='se']
effectSizes[, run_id:=match(run, input_names)]
effectSizes[, effect:=(exp(effect)*100)-100]
effectSizes[outcome=='ipd_cases', outcome_label:='All IPD Cases']
effectSizes[outcome=='ipd_pcv10_serotype_cases', outcome_label:='PCV10 Serotypes']
effectSizes[outcome=='ipd_non_pcv10_serotype_cases', outcome_label:='Non−PCV10 Serotypes']
effectSizes[outcome=='xrcp_cases', outcome_label:='All X−Ray Confirmed Cases']

# assemble fitted values into a workable data table

# -----------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------
# Graph

# graph settings
colors = brewer.pal(3, 'Paired')
smoothFormula = y ~ poly(x, 4) + poly(x, 3) + poly(x, 2)

# graph effect sizes
ggplot(effectSizes[est=='Estimate'], aes(y=effect, x=run_id)) + 
	geom_hline(yintercept=0, color='red') + 
	geom_smooth(se=FALSE, method='lm', formula=smoothFormula, color=colors[2]) + 
	geom_smooth(data=effectSizes[est=='Lower'], se=FALSE, method='lm', formula=smoothFormula, color=colors[1]) + 
	geom_smooth(data=effectSizes[est=='Upper'], se=FALSE, method='lm', formula=smoothFormula, color=colors[1]) + 
	geom_point() + 
	geom_point(data=effectSizes[est=='Lower']) + 
	geom_point(data=effectSizes[est=='Upper']) + 
	facet_wrap(~outcome_label, scales='free_y') + 
	labs(title='Effect Size At Varying Window Width', y='Effect Size (% Change)', x='Window Width') + 
	theme_bw()
# ---------------------------------------------------------------------------------------------------------
