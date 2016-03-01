** --------------------------------------------------------
** David Phillips
**
** 11/17/2015
** Simple regression analysis to test impact of PCV
** --------------------------------------------------------


** -----------------------------------------------------------
** Set up stata
clear
capture restore, not
set more off, perm
set out p
if (c(os)=="Unix") global j /home/j
else global j J:
do "$j/Usable/Tools/ADO/pdfmaker_Acrobat11.do"
** -----------------------------------------------------------


** ------------------------------------------------------------------------------------
** Files and directories

** directory
local dir "$j/Project/Evaluation/GAVI/Mozambique/pcv_impact/"

** input file
local inFile "`dir'/data/ipd_monthly_rates_final_cism_Mozambique.xls"

** output for graphs
local graphOutFile "`dir'/output/sensitivity_graphs.pdf"
** ------------------------------------------------------------------------------------------


** -------------------------------------------------------------------------------
** Load/prep data

** load
import excel "`inFile'", clear firstrow

** format date
gen year = substr(Month,-4,.)
destring year, replace
replace Month = proper(Month)
rename Month month_year
gen month = .
local m 1
foreach mon in `c(Mons)' {
	replace month = `m' if substr(month_year,1,3)=="`mon'"
	local m = `m'+1
}
gen moyr = ((month-1)/12)+year

** make alternate outcomes
gen not_pcv10 = IPDcases - PCV10VT

** -------------------------------------------------------------------------------


** -------------------------------------------------------------------------------
** Run regression and predict

** start place to store estimates
preserve
clear
set obs 1
gen foo=.
tempfile coefs
save `coefs', replace
restore

** loop over cutpoints between June 2013 and Oct 2014
levelsof month_year if moyr<=2014.75 & moyr>=2012, local(month_years)
foreach month_year of local month_years {
	n dis "`month_year'"

	** store month-year name
	levelsof moyr if month_year=="`month_year'", local(moyr) clean
	* local moyr = `moyr'-.0001

	** make indicator
	capture drop post_introduction
	gen post_introduction = moyr>`moyr'

	** regress pcv10
	nbreg PCV10VT moyr post_introduction

	** store estimates
	preserve
	clear
	set obs 1
	gen trend = _b[moyr]
	gen trend_stderr = _se[moyr]
	gen treatment_effect = _b[post_introduction]
	gen treatment_effect_stderr = _se[post_introduction]
	gen moyr = `moyr'
	gen month_year = "`month_year'"
	append using `coefs'
	save `coefs', replace
	restore
}
** -------------------------------------------------------------------------------


** -------------------------------------------------------------------------------
** Graph

** set up
use `coefs', clear
drop foo
gen trend_pvalue = 2*normal(-abs(trend/trend_stderr))
gen treatment_pvalue = 2*normal(-abs(treatment_effect/treatment_effect_stderr))

** common graph settings
local settings ytitle("Value") xtitle("Date of Cut-Point") xline(2013.75, lpattern(longdash) lcolor(gs12)) nodraw

** graph
tw line trend moyr, sort title("Secular Trend ({&beta}{sub:1})") `settings' name(trend, replace)
tw line treatment_effect moyr, sort title("Treatment Effect ({&beta}{sub:2})") text(0 2013.75 "PCV Initiation", orientation(vertical) placement(9) size(small)) `settings' name(treatment, replace)
tw line trend_pvalue moyr, sort title("Trend P-Value") `settings' name(trendp, replace)
tw line treatment_pvalue moyr, sort title("Treatment Effect P-Value") `settings' name(treatmentp, replace)
graph combine trend treatment trendp treatmentp, title("Sensitivity to Cut-Point")
graph export "`graphOutFile'", replace as(pdf)
** -------------------------------------------------------------------------------
