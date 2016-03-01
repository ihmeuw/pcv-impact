# ---------------------------------------------------------
# David Phillips
#
# 3/1/2016
# Code that loads/preps data. 
# Inputs:  
# * dir - directory where data live
# 
# Outputs:  
# * data - all datasets in 'prepped' format (see readme)
# ---------------------------------------------------------


# Define function
prepData = function(dir=NULL) {
	
	# ---------------------------------------------------------------------
	# Handle inputs
	
	# test
	if (is.null(dir)) stop('Must provide directory')
	
	# data files
	ipdFile = paste0(dir, '/ipd_monthly_rates_final_cism_Mozambique.xls')
	xrcpFile = paste0(dir, '/xrcp_monthly_rates_final_cism_mozambique.xls')
	
	# more tests
	if (!file.exists(ipdFile)) stop(paste(ipdFile, 'not found'))
	if (!file.exists(xrcpFile)) stop(paste(xrcpFile, 'not found'))
	# ---------------------------------------------------------------------

	
	# --------------------------------------------------------------------------------------------
	# Load data
	
	# load and convert to data tables
	ipdData = data.table(read_excel(ipdFile))
	xrcpData = data.table(read_excel(xrcpFile))
	
	# test unique identifiers
	test = length(unique(ipdData$Month))==nrow(ipdData)
	if (!test) stop('Month does not uniquely identify rows in IPD data')
	test = length(unique(xrcpData$Month))==nrow(xrcpData)
	if (!test) stop('Month does not uniquely identify rows in IPD data')
	mismatches = xrcpData$Month[!xrcpData$Month %in% ipdData$Month]
	if (length(mismatches>0)) stop('There are month-years in XRCP data that aren\'t in IPD data')
	
	# merge IPD and XRCP together
	data = merge(ipdData, xrcpData, 'Month', all=TRUE)
	# --------------------------------------------------------------------------------------------
	
	
	# -----------------------------------------------------------------------------
	# Prep data
	
	# variable names
	newNames = c('moyr', 'ipd_cases', 'ipd_exposure', 'ipd_pcv10_serotype_cases', 
						'ipd_pcv13_serotype_cases', 'xrcp_cases', 'xrcp_exposure')
	setnames(data, names(data), newNames)
	
	# date format
	data[, moyr:=as.Date(paste0('01', moyr), '%d%B%Y')]
	
	# make alternative outcomes
	data[, ipd_non_pcv10_serotype_cases:=ipd_cases-ipd_pcv10_serotype_cases]
	# -----------------------------------------------------------------------------
	
	
	# ---------------
	# Return output
	return(data)
	# ---------------
}
