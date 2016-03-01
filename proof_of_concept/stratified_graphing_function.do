** Graphing function, stratified by a sub-group (meant for eligibility)

local y `1'
local stub `2'
local title `3'
local subtitle `4'
local texty `5'
local line1 `6'
local line2 `7'
local strat `8'

** graph settings
local settings

** x range
sum moyr
local min = floor(`r(min)')
local max = ceil(`r(max)')
local xlabel xlabel(`min'(2)`max')
local settings `settings' `xlabel'

** added lines
local lines xline(`line1', lcolor(gs12) lpattern(longdash))
if ("`line2'"!="") local lines `lines' xline(`line2', lcolor(gs12) lpattern(longdash))
local settings `settings' `lines'

** report colors
local acolor color(gs14) lwidth(none)
local mcolor mcolor("45 53 142")
local lcolor lcolor(black)
local cfcolor lcolor("250 166 26")

* ** alternate colors
* local acolor fcolor("166 206 227") lcolor("166 206 227")
* local mcolor mcolor("31 120 180")
* local cfcolor lcolor("227 26 28")
local settings `settings' `lcolor'

** titles
local title title("`title'") subtitle("`subtitle'")
local ytitle ytitle("Mean cases per month")
local xtitle xtitle("Quarter")
local settings `settings' `ytitle' `xtitle'

** legend
local legend legend(order(2 4 3 1) label(1 "95% CI") label(2 "Observed cases") labe(3 "Counterfactual") label(4 "Trend") col(4) symxsize(*.5))
local settings `settings' `legend'

** added text for line labels
levelsof month_year if moyr==`line1', local(label1) clean
levelsof month_year if moyr==`line2', local(label2) clean
local label1 = subinstr("`label1'", "20", " 20", .)
local label2 = subinstr("`label2'", "20", " 20", .)
local linelabels text(`texty' `line1' "`label1'", orientation(vertical) placement(7) size(small)) text(`texty' `line2' "PCV Introduction", orientation(vertical) placement(7) size(small))

* ** added text for effect label
* levelsof `stub'_effect, local(b) clean
* levelsof `stub'_effect_upper, local(u) clean
* levelsof `stub'_effect_lower, local(l) clean
* local b = `b'*100
* local u = `u'*100
* local l = `l'*100
* sum moyr if `y'!=. & `stub'_pred!=.
* local texty2 = `texty'*.9
* sum moyr
* local adj1 = `r(sd)'/4.25
* local adj2 = `r(sd)'/2
* if (`b'<100) local decrease 1
* else local decrease 0
* if (`decrease') local adj2 = `adj2'/1.5
* local line1_adj = `line1'-`adj1'
* if (`decrease') {
	* local b = 100-`b'
	* local u = 100-`u'
	* local l = 100-`l'
	* local b : di %3.1f `b'
	* local u : di %3.1f `u'
	* local l : di %3.1f `l'
	* foreach v in b u l {
		* while(substr("``v''",-1,1)=="0") {
			* local `v' = substr("``v''",1,length("``v''")-1)
		* }
		* if (substr("``v''",-1,1)==".") local `v' = substr("``v''",1,length("``v''")-1)
	* }
	* local coefText text(0 `line1_adj' "`b'% Reduction" "95% CI: `u'% to `l'%", placement(9) size(vsmall))
* }
* if (!`decrease') {
	* local b = `b'-100
	* local u = `u'-100
	* if (`l'<100) local l = 0-(100-`l')
	* if (`l'>100) local l = `l'-100
	* local b : di %3.1f `b'
	* local u : di %3.1f `u'
	* local l : di %3.1f `l'
	* foreach v in b u l {
		* while(substr("``v''",-1,1)=="0") {
			* local `v' = substr("``v''",1,length("``v''")-1)
		* }
		* if (substr("``v''",-1,1)==".") local `v' = substr("``v''",1,length("``v''")-1)
	* }
	* local coefText text(`texty2' `line1_adj' "`b'% Increase" "95% CI: `l'% to `u'%", placement(11) size(vsmall))
* }

* ** indicator line for effect label
* if (`decrease') gen tmpy = 0.1 in 1
* if (!`decrease') gen tmpy = `texty2' in 1
* levelsof `stub'_pred if moyr == `line1', local(tmpy2) clean
* replace tmpy = `tmpy2' in 2
* gen tmpx = `line1'-`adj2' in 1
* replace tmpx = `line1' in 2
* local indicatorLine line tmpy tmpx, lcolor(gs8) lwidth(vvthin)

** fix up counterfactual so it's not visible underneath the main line
sum moyr if pre_introduction
replace `stub'_pred_cf = . if moyr<`r(max)'-.1

** fix up stratification variable
cap drop tmp
gen tmp = "PCV-Eligible" if `strat'==1
replace tmp = "PCV-Ineligible" if `strat'==0
drop `strat'
rename tmp `strat'

** graph IPD
tw rarea `stub'_pred_lower `stub'_pred_upper moyr, `acolor' || `indicatorLine' || scatter `y' moyr, `mcolor' || line `stub'_pred_cf moyr, `cfcolor' lpattern(shortdash) || line `stub'_pred moyr, `settings' by(`strat', `title' note("")) subtitle(,bfcolor(gs15)) `linelabels' `coefText'

capture drop tmp*
