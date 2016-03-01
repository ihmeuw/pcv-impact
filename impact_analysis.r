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
library(MASS)
library(stats4)
library(ggplot2)
# ------------------


# -----------------------------------------------------------
# Files, directories and settings

# change to code directory
codeDir = 'H:/local/mixed-methods-analysis/pcv_impact/code/'
setwd(codeDir)

# load functions
source(paste0(codeDir, 'prepData.r'))
source(paste0(codeDir, 'its.r'))
# source(paste0(codeDir, 'bma.r'))
# source(paste0(codeDir, 'cpbma.r'))
# source(paste0(codeDir, 'graph.r'))

# root input/output directory
root = 'J:/Project/Evaluation/GAVI/Mozambique/pcv_impact/'

# graph file
graphFile = paste0(root, 'output/pcv_impact.pdf')

# list of outcome variables
outcomes = c('ipd_cases', 'ipd_pcv10_serotype_cases', 
				'ipd_non_pcv10_serotype_cases', 'xrcp_cases')
# -----------------------------------------------------------


# ------------------------------------
# Load/prep data
data = prepData(paste0(root, 'data'))
# ------------------------------------


# -------------------------------------------------------------------------------------------------------------------
# Execute analysis

# basic ITS
cutpoints = as.Date(c('010413', '010114'), '%d%m%y')
itsResults = vector('list', length(outcomes)) 
for(o in seq(length(outcomes))) {
	itsResults[[o]] = its(data=data, outcome=outcomes[o], cutpoint=cutpoints, slope=FALSE)
}

# full BMS
# -------------------------------------------------------------------------------------------------------------------


# -----------------------------------------------
# Set up to graph
# -----------------------------------------------


# -----------------------------------------------
# Graph
# -----------------------------------------------
