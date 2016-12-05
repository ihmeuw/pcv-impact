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
		tmp = data.table(cutpoint=itsResults[[d]]$cutpoint[1], 
							effect=as.numeric(itsResults[[d]]$effect_size[1,]), 
							effect_se=as.numeric(itsResults[[d]]$effect_size[4,]), 
							gof=itsResults[[d]]$gof)
		if (length(itsResults[[d]]$cutpoint)>1) tmp[, cutpoint2:=itsResults[[d]]$cutpoint[2]]
		stats = rbind(stats, tmp)
	}
	# ---------------------------------------------------------------------------------
	
	
	# -----------------------------------------------------------------------------------------------------------
	# Average
	
	# estimate weights under uniform prior
	data[, weight:=exp(-.5*(gof-min(gof)))]
	stats[, weight:=exp(-.5*(gof-min(gof)))]
	
	# average predictions
	meanData = suppressWarnings(data[, lapply(.SD, mean, weight=weight, na.rm=TRUE), by='moyr', 
				.SDcols=names(data)[!names(data) %in% c('moyr', 'weight', 'gof')]])
			
	# estimate prediction standard errors
	tmpData = meanData[, c('moyr', predVar), with=FALSE]
	setnames(tmpData, predVar, 'mean')
	data = merge(data, tmpData, 'moyr')
	data[, model_variance:=(log(get(predVar))-log(mean))^2]
	
	# prediction uncertainty intervals
	meanSe = suppressWarnings(data[, list(se=mean(get(predSeVar), weight=weight)), by='moyr']) # no model uncertainty (within-model variance only)
	# meanSe = data[, list(se=sqrt(mean(get(predSeVar)^2+model_variance, weight=weight))), by='moyr'] # w/ model uncertainty
	meanData = merge(meanData, meanSe, 'moyr')
	meanData[, (upperVar):=exp(log(get(predVar))+1.95996*se)]
	meanData[, (lowerVar):=exp(log(get(predVar))-1.95996*se)]
	
	# recompute effect size using prediction interval instead of mean standard error
	cf = log(meanData[moyr==newEffectDate][[paste0(outcome,'_pred_cf')]])
	effect = log(meanData[moyr==newEffectDate][[paste0(outcome, '_pred')]]) - cf
	effect_lower = log(meanData[moyr==newEffectDate][[paste0(outcome, '_pred_upper')]]) - cf
	effect_upper = log(meanData[moyr==newEffectDate][[paste0(outcome, '_pred_lower')]]) - cf
	effect_se = (effect_upper-effect)/1.95996
	effect_size = data.table('effect'=c(effect, effect_upper, effect_lower, effect_se))
	# -----------------------------------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------------------------
	# Return output
	return(list('data'=meanData, 'outcome'=outcome, 
				'cutpoint'=as.Date(c(min(cutpoints), max(cutpoints)), origin='1970-01-01'), 
				'effect_size'=effect_size, 'stats'=stats, newEffectDate=itsResults[[1]]$newEffectDate))
	# -------------------------------------------------------------------------------------------------
}
