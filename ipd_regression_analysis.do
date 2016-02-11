** --------------------------------------------------------
** David Phillips
**
** 11/17/2015
** Simple regression analysis to test impact of PCV
** --------------------------------------------------------


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


** ------------------------------------------------------------------------------------
** Files and directories

** code directory
local codeDir "H:/local/mixed-methods-analysis/pcv_impact/code/"
if (c(os)=="Unix") local codeDir "./"

** data directory
local dir "$j/Project/Evaluation/GAVI/Mozambique/pcv_impact/"

** input file
local inFile "`dir'/data/ipd_monthly_rates_final_cism_Mozambique.xls"

** output for regressions
local regOutFile "`dir'/output/ipd_regression_output.txt"
log using "`regOutFile'", replace text

** output for graphs
local graphOutFile "`dir'/output/ipd_graphs_regression_interpolation"

** regression function
local regFunction "`codeDir'/regression_function.do"

** graphing function
local graphFunction "`codeDir'/graphing_function.do"
** ------------------------------------------------------------------------------------------


** ---------------------------------------------------------------------------------------------------------
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
** ---------------------------------------------------------------------------------------------------------


** ------------------------------------------------
** Run regression and predict

** regress IPD
do "`regFunction'" IPDcases ipd 1

** regress pcv10
do "`regFunction'" PCV10VT pcv10 1

** regress pcv10
do "`regFunction'" not_pcv10 pcv_other 1
** ------------------------------------------------


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
collapse (mean) IPDcases PCV10VT not_pcv10, by(moyr) fast
tempfile quarter
save `quarter', replace

** merge it back on
use `month', clear
drop IPDcases PCV10VT not_pcv10
merge 1:1 moyr using `quarter', nogen
** -------------------------------------------------------------------------------


** --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
** Graph all IPD

** open pdf
pdfstart using "`graphOutFile'.pdf"

do "`graphFunction'" "IPDcases" "ipd" "PCV Introduction and IPD Cases" "All Cases" "6" "`introduction_end'" "`introduction_start'"
pdfappend

** graph PCV-10 only
do "`graphFunction'" "PCV10VT" "pcv10" "PCV Introduction and IPD Cases" "PCV10 Serotypes" "4" "`introduction_end'" "`introduction_start'"
pdfappend

** graph IPD besides PCV10
do "`graphFunction'" "not_pcv10" "pcv_other" "PCV Introduction and IPD Cases" "Non-PCV10 Serotypes" "3" "`introduction_end'" "`introduction_start'"
pdfappend

** close pdf
pdffinish
capture rm "`graphOutFile'.log"
log close _all
** --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
