** ------------------------------------------------------------------------------------------
** David Phillips
**
** 11/18/2015
** Simple regression analysis to test impact of PCV on X-ray confirmed pneumonia
** Includes eligibility as a covariate
** ------------------------------------------------------------------------------------------


** -----------------------------------------------------------
** Set up stata
clear
set more off, perm
capture log close _all
set out p
if (c(os)=="Unix") global j /home/j
else global j J:
do "$j/Usable/Tools/ADO/pdfmaker_Acrobat11.do"
** -----------------------------------------------------------


** ----------------------------------------------------------------------------------------------
** Files and directories

** code directory
local codeDir "H:/local/pcv_impact/code/"
if (c(os)=="Unix") local codeDir "./"

** directory
local dir "$j/Project/Evaluation/GAVI/Mozambique/pcv_impact/"

** input file
local inFile "`dir'/data/xrcp_mrates_elig.xls"

** output for regressions
local regOutFile "`dir'/output/xrcp_regression_output.txt"
log using "`regOutFile'", replace text

** output for graphs
local graphOutFile "`dir'/output/xrcp_graphs_stratified"

** regression function
local regFunction "`codeDir'/regression_function.do"

** graphing function
local stratifiedGraphFunction "`codeDir'/stratified_graphing_function.do"
** ----------------------------------------------------------------------------------------------


** -------------------------------------------------------------------------------
** Load/prep data

** load
import excel "`inFile'", clear firstrow

** format date
gen year = substr(month,-4,.)
destring year, replace
replace month = proper(month)
rename month month_year
gen month = .
local m 1
foreach mon in `c(Mons)' {
	replace month = `m' if substr(month_year,1,3)=="`mon'"
	local m = `m'+1
}
gen moyr = ((month-1)/12)+year

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

** binary eligibility
gen tmp = eligible=="Yes"
drop eligible
rename tmp eligible

** use all children for both groups (eligible and ineligible) prior to pcv
gen tmp = moyr if eligible
egen first_eligible = min(tmp)
expand 2 if moyr<first_eligible, gen(newobs)
replace eligible=1 if newobs
drop newobs tmp
** -------------------------------------------------------------------------------


** ----------------------------------------------------
** Run regression and predict

** regress XRCP, sneak in eligibility
do "`regFunction'" cases xrcp 1 eligible
** ----------------------------------------------------


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
collapse (mean) cases, by(moyr eligible) fast
tempfile quarter
save `quarter', replace

** merge it back on
use `month', clear
drop cases 
merge 1:1 moyr eligible using `quarter', nogen
** -------------------------------------------------------------------------------


** --------------------------------------------------------------------------------------------------------------------------------------
** Graph all IPD

do "`stratifiedGraphFunction'" "cases" "xrcp" "PCV Introduction and XRCP Cases" "" "15" "`introduction_end'" "`introduction_start'" "eligible"

** export
pdfstart using "`graphOutFile'.pdf"
pdfappend
pdffinish
capture rm "`graphOutFile'.log"
** --------------------------------------------------------------------------------------------------------------------------------------
