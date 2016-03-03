# ----------------------------------------------------------------------------------------
# David Phillips
#
# 3/1/2016
# Function that produces a time series graph from the results of its.r, bma.r or cpbma.r.
# Inputs:
# * itsOutput - list. standard output from its.r
# * quarterly - logical. whether points should be averaged by quarter

# Outputs:  
# * p - a ggplot graph
# ----------------------------------------------------------------------------------------


# Define function
graph = function(itsOutput=NULL, quarterly=TRUE) {
	
	# ---------------------------------------------------------------------------------
	# Handle inputs
	
	# 'unpack' arguments
	for(arg in names(itsOutput)) assign(arg, itsOutput[[arg]])
	
	# test
	for(arg in c('data', 'outcome')) {
		if (is.null(get(arg))) stop(paste('Must provide', arg))
	}
	
	# essential variables
	essentialVars = c(outcome, 'moyr', paste0(outcome, '_pred'), 
						paste0(outcome, '_pred_upper'), paste0(outcome, '_pred_lower'), 
						paste0(outcome, '_pred_cf'), paste0(outcome, '_pred_cf_upper'), 
						paste0(outcome, '_pred_cf_lower'))
	if (!all(essentialVars %in% names(data))) stop('Essential variables not in data')
	
	# cutpoint(s)
	C = length(cutpoint)
	if (C>2) stop('ITS not set up to handle more than 2 cutpoints')
	if (C==1) start = end = cutpoint
	if (C==2) { 
		start = cutpoint[1]
		end = cutpoint[2]
	}
	
	# identify if the input was bma or not based on the contents (could be better)
	is_bma = 'stats' %in% names(itsOutput)	
	# ---------------------------------------------------------------------------------
	
	
	# ----------------------------------------------------------------------------------------------------
	# Average to quarter level if specified
	graphData = copy(data)
	if (quarterly) {
	
		# collapse to quarter level
		quarterData = copy(graphData)
		quarterData[month(moyr)<4, moyr:=as.Date(paste0('0102', year(moyr)), '%d%m%Y')]
		quarterData[month(moyr)>=4 & month(moyr)<7, moyr:=as.Date(paste0('0105', year(moyr)), '%d%m%Y')]
		quarterData[month(moyr)>=7 & month(moyr)<10, moyr:=as.Date(paste0('0108', year(moyr)), '%d%m%Y')]
		quarterData[month(moyr)>=10, moyr:=as.Date(paste0('0111', year(moyr)), '%d%m%Y')]
		quarterData = quarterData[, list(mean=mean(get(outcome))), by='moyr']
		
		# merge and replace
		graphData = merge(graphData, quarterData, 'moyr', all=TRUE)
		graphData[, (outcome):=mean]
	}
	# ----------------------------------------------------------------------------------------------------
	
	
	# ----------------------------------------------------------------------------------------------------
	# Formatting
	
	# format effect size to be human readable
	effect_size = exp(effect_size)*100
	if (effect_size$effect[1]<100) {
		effect_size[, interpretation:='% Reduction']
		effect_size[, effect:=100-effect]
		effect_size[, estimate:=c('Estimate', 'Upper', 'Lower')]
	} else {
		effect_size[, interpretation:='% Increase']
		effect_size[, effect:=effect-100]
		effect_size[, estimate:=c('Estimate', 'Lower', 'Upper')]
	}
	effect_size = data.frame(effect_size)
	effect_size$effect = round(effect_size$effect, 1)

	# rename variables so aes_string isn't necessary
	setnames(graphData, c(outcome, paste0(outcome,'_pred'), paste0(outcome,'_pred_upper'), 
							paste0(outcome,'_pred_lower'), paste0(outcome,'_pred_cf')), 
							c('observed', 'trend', 'upper', 'lower', 'cf'))
	# ----------------------------------------------------------------------------------------------------
	
	
	# ----------------------------------------------------------------------------------------------------------------
	# Graph settings

	# colors
	ciColor = '#E0E0E0'
	cfColor = '#FAA61A'
	lineColor = 'black'
	pointColor = '#2D358E'
	annoColor = '#C0C0C0'

	# title
	title = 'PCV Impact'
	if (outcome=='ipd_cases') title=paste0(title, '\nAll IPD Cases')
	if (outcome=='ipd_pcv10_serotype_cases') title=paste0(title, '\nPCV10 Serotypes')
	if (outcome=='ipd_non_pcv10_serotype_cases') title=paste0(title, '\nNon-PCV10 Serotypes')
	if (outcome=='xrcp_cases') title=paste0(title, '\nAll X-Ray Confirmed Cases')
	
	# axis labels
	if (quarterly) ylab = 'Means cases per month'
	if (!quarterly) ylab = 'Cases'
	if (quarterly) xlab = 'Quarter'
	if (!quarterly) xlab = 'Month'
	
	# annotation text
	annotation = paste0(effect_size[1,1], effect_size[1,2], '\n95% CI: ', 
						effect_size[effect_size$estimate=='Lower',1], '% to ', 
						effect_size[effect_size$estimate=='Upper',1], '%')
						
	# annotation location
	if (effect_size[1,2]=='% Reduction') annotationY = min(graphData$observed, na.rm=TRUE)
	if (effect_size[1,2]!='% Reduction') annotationY = max(graphData$observed, na.rm=TRUE)
	annotationX = start-450
	annotationLocation = data.frame(x=annotationX, y=annotationY, xend=end, yend=graphData$trend[graphData$moyr==end])
	if (start==end) label1 = label2 = 'PCV Introduction'
	if (start!=end) label1 = 'PCV Introduction'
	if (start!=end) label2 = 'PCV Routinization'
	if (start!=end & is_bma) label1 = 'Effect Window (start)'
	if (start!=end & is_bma) label2 = 'Effect Window (end)'
	# ----------------------------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------------------------------------
	# Actually make graph
	p = ggplot(graphData, aes(x=moyr)) +			
			# add geoms
			geom_ribbon(aes(ymax=upper, ymin=lower, fill='95% CI')) + 
			geom_line(aes(y=cf, color='Counterfactual'), linetype=2) +
			geom_line(aes(y=trend, color='Trend')) + 
			geom_point(aes(y=observed, color='Observed Cases', linetype='Observed Cases')) + 
			
			# control colors
			scale_fill_manual('', breaks=c('95% CI'), values=c(ciColor)) + 
			scale_color_manual('', breaks=c('Counterfactual', 'Trend', 'Observed Cases'), values=c(cfColor, pointColor, lineColor)) + 
			
			# override legend shapes
			guides(color=guide_legend(override.aes=list(shape=c(NA,NA,16), linetype=c(2,1,0)))) +
			
			# annotate the effect size
			geom_segment(aes(x=x, y=y, xend=xend, yend=yend), data=annotationLocation, color=annoColor) +
			annotate('text', label=annotation, x=annotationX, y=annotationY, size=3, vjust=.5, hjust=1) +
			
			# place vertical line(s) at cutpoint(s)
			geom_vline(xintercept=as.numeric(start), linetype=5, color=annoColor) +
			geom_vline(xintercept=as.numeric(end), linetype=5, color=annoColor) +
			annotate('text', label=label1, x=start, y=max(graphData$observed, na.rm=TRUE), size=3, hjust=1, vjust=-.25, angle=90) +
			annotate('text', label=label2, x=end, y=max(graphData$observed, na.rm=TRUE), size=3, hjust=1, vjust=-.25, angle=90) +
			
			# label and theme
			labs(title=title, y=ylab, x=xlab) +
			theme_bw()
			p
	# --------------------------------------------------------------------------------------------------------------------------------
	
	
	# --------------
	# Return output	
	return(p)
	# --------------
}
