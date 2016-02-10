** Regression function
local y `1'
local stub `2'
local interpolate `3'
local strat `4'

n dis "----------------"
n dis "`y'"
n dis "----------------"

** regress `stub'
if ("`strat'"=="") n nbreg `y' moyr post_introduction
if ("`strat'"!="") { 
	gen interaction = post_introduction * `strat'
	n nbreg `y' moyr post_introduction `strat' interaction
	n dis "Effect of PCV on eligible children:"
	n lincom post_introduction + interaction
}

** predict
predictnl `stub'_pred = exp(xb(#1)), ci(`stub'_pred_lower `stub'_pred_upper)

** predict counterfactual
predictnl `stub'_pred_cf = exp(_b[_cons] + _b[moyr]*moyr)

** interpolate over study period
if (`interpolate') {
	replace `stub'_pred = . if during_introduction
	replace `stub'_pred_lower = . if during_introduction
	replace `stub'_pred_upper = . if during_introduction
	if ("`strat'"=="") ipolate `stub'_pred moyr, gen(tmp)
	if ("`strat'"!="") bysort `strat': ipolate `stub'_pred moyr, gen(tmp)
	replace `stub'_pred = tmp
	drop tmp
	if ("`strat'"=="") ipolate `stub'_pred_lower moyr , gen(tmp)
	if ("`strat'"!="") bysort `strat': ipolate `stub'_pred_lower moyr , gen(tmp)
	replace `stub'_pred_lower = tmp
	drop tmp
	if ("`strat'"=="") ipolate `stub'_pred_upper moyr, gen(tmp)
	if ("`strat'"!="") bysort `strat': ipolate `stub'_pred_upper moyr, gen(tmp)
	replace `stub'_pred_upper = tmp
	drop tmp
}

** store primary coefficient/CI as variable
gen `stub'_effect = exp(_b[post_introduction])
gen `stub'_effect_upper = exp(_b[post_introduction]+1.96*_se[post_introduction])
gen `stub'_effect_lower = exp(_b[post_introduction]-1.96*_se[post_introduction])
