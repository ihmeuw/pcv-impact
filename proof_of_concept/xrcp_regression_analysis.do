** ------------------------------------------------------------------------------------------
** David Phillips
**
** 11/18/2015
** Simple regression analysis to test impact of PCV on X-ray confirmed pneumonia
** ------------------------------------------------------------------------------------------


** -----------------------------------------------------------
** Set up stata
clear
set more off, perm
capture log close _all
set out e
if (c(os)=="Unix") global j /home/j
else global j J:
do "$j/Usable/Tools/ADO/pdfmaker_Acrobat11.do"
** -----------------------------------------------------------


** ------------------------------------------------------------------------------------------
** Files and directories

** code directory
local codeDir "H:/local/pcv_impact/code/"
if (c(os)=="Unix") local codeDir "./"

** directory
local dir "$j/Project/Evaluation/GAVI/Mozambique/pcv_impact/"

** input file
local inFile "`dir'/data/xrcp_monthly_rates_final_cism_mozambique.xls"

** output for regressions
local regOutFile "`dir'/output/xrcp_regression_output.txt"
log using "`regOutFile'", replace text

** output for graphs
local graphOutFile "`dir'/output/xrcp_graphs"

** regression function
local regFunction "`codeDir'/regression_function.do"

** graphing function
local graphFunction "`codeDir'/graphing_function.do"
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

** hack labels
expand 2 in 1, gen(newobs)
foreach var of varlist * {
	cap replace `var' = . if newobs
	cap replace `var' = "" if newobs
}
replace month_year = "Apr2013" if newobs
replace moyr = 2013.25 if newobs
drop newobs

** make special regression variables
local introduction_start = 2013.25
local introduction_end = 2014
gen pre_introduction = moyr<`introduction_start'
gen during_introduction = moyr>=`introduction_start' & moyr<=`introduction_end'
gen post_introduction = moyr>`introduction_end'
gen time_before_intervention = moyr
gen time_during_intervention = moyr - `introduction_start'
replace time_during_intervention = 0 if time_during_intervention<0
gen time_since_inervention = moyr - `introduction_end'
replace time_since_inervention = 0 if time_since_inervention<0
** -------------------------------------------------------------------------------


** -----------------------------------------
** Run regression and predict

** regress XRCP
do "`regFunction'" XRCPcases xrcp 1
** -----------------------------------------


** -------------------------------------------------------------------------------
** Graph data points at quarterly level

** save estimates at month level
tempfile month
save `month', replace

** collapse data to month
replace moyr = year if month<4
replace moyr = year+(1/4) if month>=4 & month<7
replace moyr = year+(2/4) if month>=7 & month<10
replace moyr = year+(3/4) if month>=10
collapse (mean) XRCPcases, by(moyr) fast
tempfile quarter
save `quarter', replace

** merge it back on
use `month', clear
drop XRCPcases 
merge 1:1 moyr using `quarter', nogen
** -------------------------------------------------------------------------------


** --------------------------------------------------------------------------------------------------------------------------------------
** Graph all IPD

do "`graphFunction'" "XRCPcases" "xrcp" "PCV Introduction and XRCP Cases" "" "15" "`introduction_end'" "`introduction_start'"

** export
pdfstart using "`graphOutFile'.pdf"
pdfappend
pdffinish
capture rm "`graphOutFile'.log"
** --------------------------------------------------------------------------------------------------------------------------------------
