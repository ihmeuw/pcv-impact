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
prepData = function(dir=NULL, outFile=NULL) {
	
	# ------------------------------------------------------------------------
	# Handle inputs
	
	# test
	if (is.null(dir)) stop('Must provide directory')
	
	# data files
	ipdFile = paste0(dir, '/ipd_monthly_rates.xls')
	vtFile = paste0(dir, '/vt_ipd_monthly_rates.xls')
	nonvtFile = paste0(dir, '/nonvt_ipd_monthly_rates.xls')
	xrcpFile = paste0(dir, '/xrcp_monthly_rates.xls')
	
	# test for files
	files = c('ipdFile', 'vtFile', 'nonvtFile', 'xrcpFile')
	for(f in files) if (!file.exists(get(f))) stop(paste(get(f), 'not found'))
	# ------------------------------------------------------------------------
	
	
	# ---------------------------------------------------------------------------------------------------------
	# Load/test data
	
	# load and convert to data tables
	ipdData = data.table(read_excel(ipdFile))
	vtData = data.table(read_excel(vtFile))
	nonvtData = data.table(read_excel(nonvtFile))
	xrcpData = data.table(read_excel(xrcpFile))
	
	# run tests
	dataObjects = c('ipdData', 'vtData', 'nonvtData', 'xrcpData')
	for (d in dataObjects) { 
		# test unique identifiers
		tmp = copy(get(d))
		test = length(unique(tmp$month))==nrow(tmp)
		if (!test) stop(paste('Month does not uniquely identify rows in', tmp, 'data'))
		
		# test variable names
		varNames = c('month', 'case', 'tar')
		if (!all(varNames %in% names(tmp))) stop(paste('Missing variables from', tmp, 'data'))

		# test variable classes
		classes = c('character','numeric','numeric')
		if (!all(sapply(tmp, class)==classes)) stop(paste('Incorrect variable classes in', tmp, 'data'))
		
		# test variable values
		if (any(is.na(as.Date(paste0('01', tmp$month), '%d%B%Y')))) stop(paste('Invalid date in', tmp, 'data'))
		if (max(tmp$case)>300) stop(paste(tmp, 'data has an incidence count that is an order of magnitude too high')) 
		if (max(tmp$tar)>5000000) stop(paste(tmp, 'data has a TAR that is an order of magnitude too high')) 
		if (max(tmp$tar)<7000) stop(paste(tmp, 'data has a TAR that is an order of magnitude too low')) 
	}
	# ---------------------------------------------------------------------------------------------------------
	
	
	# -----------------------------------------------------------------------------
	# Prep data
	
	# drop XRCP zeroes that represent no surveillance
	xrcpData = xrcpData[c(1:25, 31:43, 71:106, 119:142)]
	
	# merge IPD, VT, non-VT and XRCP together
	data = merge(ipdData, vtData, 'month', all=TRUE, suffixes=c('ipd','vt'))
	data = merge(data, nonvtData, 'month', all=TRUE)
	data = merge(data, xrcpData, 'month', all=TRUE, suffixes=c('nonvt','xrcp'))
	
	# variable names
	newNames = c('moyr', 'ipd_cases', 'ipd_exposure', 'ipd_pcv10_serotype_cases', 
						'ipd_pcv10_serotype_exposure','ipd_non_pcv10_serotype_cases', 
						'ipd_non_pcv10_serotype_exposure', 'xrcp_cases', 'xrcp_exposure')
	setnames(data, names(data), newNames)
	
	# date format
	data[, moyr:=as.Date(paste0('01', moyr), '%d%B%Y')]
	
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
