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

# to do
# - more tests on input data (variable names, order etc)

# Define function
prepData = function(dir=NULL, outFile=NULL) {
	
	# ------------------------------------------------------------
	# Handle inputs
	
	# test
	if (is.null(dir)) stop('Must provide directory')
	
	# data files
	ipdFile = paste0(dir, '/ipd_monthly_rates.xls')
	vtFile = paste0(dir, '/vt_ipd_monthly_rates.xls')
	xrcpFile = paste0(dir, '/xrcp_monthly_rates.xls')
	
	# more tests
	if (!file.exists(ipdFile)) stop(paste(ipdFile, 'not found'))
	if (!file.exists(vtFile)) stop(paste(vtFile, 'not found'))
	if (!file.exists(xrcpFile)) stop(paste(xrcpFile, 'not found'))
	# ------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------
	# Load data
	
	# load and convert to data tables
	ipdData = data.table(read_excel(ipdFile))
	vtData = data.table(read_excel(vtFile))
	xrcpData = data.table(read_excel(xrcpFile))
	
	# test unique identifiers
	test = length(unique(ipdData$month))==nrow(ipdData)
	if (!test) stop('Month does not uniquely identify rows in IPD data')
	test = length(unique(vtData$month))==nrow(vtData)
	if (!test) stop('Month does not uniquely identify rows in IPD data')
	test = length(unique(xrcpData$month))==nrow(xrcpData)
	if (!test) stop('Month does not uniquely identify rows in IPD data')
	mismatches = xrcpData$Month[!xrcpData$month %in% ipdData$month]
	if (length(mismatches>0)) stop('There are month-years in XRCP data that aren\'t in IPD data')
	
	# drop XRCP zeroes that represent no surveillance
	xrcpData = xrcpData[c(1:25, 31:43, 71:106, 119:142)]
	
	# merge IPD, VT and XRCP together
	data = merge(ipdData, vtData, 'month', all=TRUE)
	data = merge(data, xrcpData, 'month', all=TRUE)
	# --------------------------------------------------------------------------------------------
	
	
	# -----------------------------------------------------------------------------
	# Prep data
	
	# variable names
	newNames = c('moyr', 'ipd_cases', 'ipd_exposure', 'ipd_pcv10_serotype_cases', 
						'ipd_pcv10_serotype_exposure', 'xrcp_cases', 'xrcp_exposure')
	setnames(data, names(data), newNames)
	
	# date format
	data[, moyr:=as.Date(paste0('01', moyr), '%d%B%Y')]
	
	# make alternative outcomes
	data[, ipd_non_pcv10_serotype_cases:=ipd_cases-ipd_pcv10_serotype_cases]
	data[, ipd_non_pcv10_serotype_exposure:=ipd_exposure-ipd_pcv10_serotype_exposure]
	
	# drop data prior to 2008 according to field team request
	data = data[moyr>=leadInDate]
	# -----------------------------------------------------------------------------
	
	
	# ----------------------
	# Return/save output
	inputData = data
	save(inputData, file=outFile)
	return(data)
	# ----------------------
}
