# ---------------------------------------------------------------------------------------------------------------------------------------------
# David Phillips
#
# 3/2/2016
# Function that carries out Bayesian model averaging over multiple interrupted time series analyses.  
# Inputs:  
# * itsResults - list of lists. standard output from multiple runs of its.r

# Outputs (in a list):  
# * data        - the input data object with six new columns: [outcome]_pred, [outcome]_pred_upper, [outcome]_pred_lower, [outcome]_cf, [outcome]_cf_upper, [outcome]_cf_lower,
# * outcome     - character. name of the outcome variable
# * cutpoint    - date object containing the time point or points (up to 2) of intervention
# * effect size - a data frame containing the intercept shift associated with intervention, including uncertainty
# ---------------------------------------------------------------------------------------------------------------------------------------------


# To do
# - make this work with a list of mixed outcomes
# - tests


# Define function
bma = function(itsResults) {
	
	# ---------------------------------------------------------------------------------
	# Handle inputs
	
	# isolate the outcome variables in the argument (currently only handles one)
	outcome = itsResults[[1]]$outcome
	upperVar = paste0(outcome, '_pred_upper')
	lowerVar = paste0(outcome, '_pred_lower')
	predVar = paste0(outcome, '_pred')
	predSeVar = paste0(outcome, '_pred_se')
	
	# isolate the datasets/gof in the argument into one big data table
	data = data.table(NULL)
	for(d in seq(length(itsResults))) {
		data = rbind(data, itsResults[[d]]$data, fill=TRUE)
		if (d==1) data[, gof:=itsResults[[d]]$gof]
		if (d!=1) data[is.na(gof), gof:=itsResults[[d]]$gof]
	}
	
	# isolate gof/cutpoints,effect sizes into a separate data table
	stats = data.table(NULL)
	for(d in seq(length(itsResults))) {
		tmp = data.table(cutpoint=itsResults[[d]]$cutpoint, 
							effect=as.numeric(itsResults[[d]]$effect_size[1,]), 
							effect_se=as.numeric(itsResults[[d]]$effect_size[4,]), 
							gof=itsResults[[d]]$gof)
		stats = rbind(stats, tmp)
	}
	# ---------------------------------------------------------------------------------

	
	# -----------------------------------------------------------------------------------------------------------
	# Average
	
	# estimate weights under uniform prior
	data[, weight:=exp(-.5*(gof-min(gof)))]
	stats[, weight:=exp(-.5*(gof-min(gof)))]
	
	# average stats
	effect_size = stats[, list(effect=mean(effect, weight=weight))]
	
	# average predictions
	meanData = data[, lapply(.SD, mean, weight=weight, na.rm=TRUE), by='moyr', 
				.SDcols=names(data)[!names(data) %in% c('moyr', 'weight', 'gof')]]
	
	# estimate effect size standard errors including model variance 
	# not between-model variance, but squared error from the midpoint estimate according to Dan Weinberger's SAS code
	stats[, model_variance:=(effect-as.numeric(effect_size))^2]
	effect_size = cbind(effect_size, stats[, list(se=mean(effect_se^2+model_variance, weight=weight))])
	
	# effect size uncertainty intervals
	effect_size[, upper:=effect+1.95996*se]
	effect_size[, lower:=effect-1.95996*se]
	effect_size = melt(effect_size[, c('effect', 'lower', 'upper', 'se'), with=F], value.name='effect')
	effect_size$variable = NULL
	
	# estimate prediction standard errors
	tmpData = meanData[, c('moyr', predVar), with=FALSE]
	setnames(tmpData, predVar, 'mean')
	data = merge(data, tmpData, 'moyr')
	data[, model_variance:=(log(get(predVar))-log(mean))^2]
	
	# prediction uncertainty intervals
	meanSe = data[, list(se=mean(get(predSeVar), weight=weight)), by='moyr'] # no model uncertainty (within-model variance only)
	# meanSe = data[, list(se=sqrt(mean(get(predSeVar)^2+model_variance, weight=weight))), by='moyr'] # w/ model uncertainty
	meanData = merge(meanData, meanSe, 'moyr')
	meanData[, (upperVar):=exp(log(get(predVar))+1.95996*se)]
	meanData[, (lowerVar):=exp(log(get(predVar))-1.95996*se)]
	# -----------------------------------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------------------------
	# Return output
	return(list('data'=meanData, 'outcome'=outcome, 
				'cutpoint'=as.Date(c(min(cutpoints), max(cutpoints)), origin='1970-01-01'), 
				'effect_size'=effect_size, 'stats'=stats, newEffectDate=itsResults[[1]]$newEffectDate))
	# -------------------------------------------------------------------------------------------------
}
