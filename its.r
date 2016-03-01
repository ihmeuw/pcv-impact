# ---------------------------------------------------------------------------------------------------------------------------------------------
# David Phillips
#
# 3/1/2016
# Function that carries out interrupted time series analysis  
# Inputs:  
# * data     - data table object in 'prepped' format (see below)
# * outcome  - character. name of the outcome variable
# * cutpoint - date object containing the time point or points (up to 2) of intervention
# * slope    - logical. TRUE indicates that an interaction term (or terms) should be used to estimate a different slope before/after intervetion

# Outputs (in a list):  
# * data        - the input data object with six new columns: [outcome]_pred, [outcome]_pred_upper, [outcome]_pred_lower, [outcome]_cf, [outcome]_cf_upper, [outcome]_cf_lower,
# * effect size - a data frame containing the intercept shift associated with intervention, including uncertainty
# * gof         - goodness of fit based on BIC
# ---------------------------------------------------------------------------------------------------------------------------------------------


# To do
# - confirm that end-start = the last value of daysDuringIntervention, and that it repeats for all post intervention time points


# Define function
its = function(data=NULL, outcome=NULL, cutpoint=NULL, slope=NULL) {
	
	# ------------------------------------------------------------------------------
	# Handle inputs
	
	# test
	for(arg in c('data', 'outcome', 'cutpoint', 'slope')) {
		if (is.null(get(arg))) stop(paste('Must provide', arg))
	}
	
	# essential variables
	formulaVars = c(outcome, 'moyr')
	if (!all(formulaVars %in% names(data))) stop('Essential variables not in data')
	
	# cutpoint(s)
	C = length(cutpoint)
	if (C>2) stop('ITS not set up to handle more than 2 cutpoints')
	if (C==1) start = end = cutpoint
	if (C==2) { 
		start = cutpoint[1]
		end = cutpoint[2]
		duration = (end-start)[[1]]
	}
	# ------------------------------------------------------------------------------

	
	# -------------------------------------------------------------------------------------------
	# Set up/run regression
	
	# generate ITS variables
	data[, preIntervention:=moyr<=start]
	data[, postIntervention:=moyr>=end]
	data[, daysPostIntervention:=moyr-end]
	data[daysPostIntervention<0, daysPostIntervention:=0]
	if (C==2) data[, duringIntervention:=moyr>start & moyr<end]
	if (C==2) data[, daysDuringIntervention:=moyr-start]
	if (C==2) data[daysDuringIntervention<0, daysDuringIntervention:=0]
	if (C==2) data[postIntervention==TRUE, daysDuringIntervention:=duration] # CHECK THIS 
	
	# store formula
	f = as.formula(paste(paste(formulaVars, collapse=' ~ '), '+ postIntervention'))
	if (slope) f = as.formula(paste(f, '+ daysPostIntervention'))
	if (slope & C==2) f = as.formula(paste(f, '+ daysDuringIntervention + daysPostIntervention'))
	
	# run regression
	fit = glm.nb(f, data)
	# -------------------------------------------------------------------------------------------
	

	# -----------------------------------------------------------------------------------------------
	# Predict
	
	# store predictions
	preds = predict(fit, type='link', se.fit=TRUE)
	
	# exponentiate/include uncertainty and add to data
	data[, (paste0(outcome,'_pred')):=exp(preds$fit)]
	data[, (paste0(outcome,'_pred_upper')):=exp(preds$fit+1.95996*preds$se.fit)]
	data[, (paste0(outcome,'_pred_lower')):=exp(preds$fit-1.95996*preds$se.fit)]
	
	# linearly interpolate intervention period if two cutpoints are specified but slope isn't
	if(!slope & C==2) {
		# expected value
		lmFit = lm(as.formula(paste0(outcome, '_pred ~ moyr')), data[moyr==start | moyr==end])
		interpolation = predict(lmFit, newdata=data[duringIntervention==TRUE])
		data[duringIntervention==TRUE, (paste0(outcome,'_pred')):=interpolation]
		
		# upper
		lmFit = lm(as.formula(paste0(outcome, '_pred_upper ~ moyr')), data[moyr==start | moyr==end])
		interpolation = predict(lmFit, newdata=data[duringIntervention==TRUE])
		data[duringIntervention==TRUE, (paste0(outcome,'_pred_upper')):=interpolation]
		
		# lower
		lmFit = lm(as.formula(paste0(outcome, '_pred_lower ~ moyr')), data[moyr==start | moyr==end])
		interpolation = predict(lmFit, newdata=data[duringIntervention==TRUE])
		data[duringIntervention==TRUE, (paste0(outcome,'_pred_lower')):=interpolation]
	}
	# -----------------------------------------------------------------------------------------------
	
	
	# ---------------------------------------------------------------------------------
	# Predict counterfactual
	
	# modify coefficients so effect of intervention is zero
	cfFit = copy(fit)
	cfFit$coefficients[3:length(cfFit$coefficients)] = 0
	
	# store counterfactual predictions
	cfPreds = predict(cfFit, type='link', se.fit=TRUE)
	
	# exponentiate/include uncertainty and add to data
	data[, (paste0(outcome,'_pred_cf')):=exp(cfPreds$fit)]
	data[, (paste0(outcome,'_pred_cf_upper')):=exp(cfPreds$fit+1.95996*cfPreds$se.fit)]
	data[, (paste0(outcome,'_pred_cf_lower')):=exp(cfPreds$fit-1.95996*cfPreds$se.fit)]	
	# ---------------------------------------------------------------------------------
	
	
	# ---------------------------------------------------------------------------------------------
	# Store effect size and goodness of fit
	
	# effect size
	effect_size = data.table('effect'=exp(cbind(coef(fit), confint(fit))['postInterventionTRUE',]))
	
	# format effect size to be human readable
	effect_size = round(effect_size*100, 1)
	if (effect_size$effect[1]<100) {
		effect_size[, interpretation:='% Reduction']
		effect_size[, effect:=100-effect]
		effect_size[, estimate:=c('Estimate', 'Upper', 'Lower')]
	} else {
		effect_size[, interpretation:='% Increase']
		effect_size[, effect:=effect-100]
		effect_size[, estimate:=c('Estimate', 'Lower', 'Upper')]
	}
	
	# GoF
	gof = BIC(fit)
	# ---------------------------------------------------------------------------------------------
	
	
	# -------------------------------------------
	# Clean up
	
	# remove ITS variables
	data$preIntervention = NULL
	data$postIntervention = NULL
	data$daysPostIntervention = NULL
	if (C==2) data$duringIntervention = NULL
	if (C==2) data$daysDuringIntervention = NULL
	# -------------------------------------------
		
	
	# -------------------------------------------------------------
	# Return output
	return(list('data'=data, 'effect_size'=effect_size, 'gof'=gof))
	# -------------------------------------------------------------
}