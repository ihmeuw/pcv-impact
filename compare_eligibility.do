** -----------------------------------------------------------------------------
** David Phillips
**
** 12/15/2015
** Compare incidence data before and after stratification by eligibility
** -----------------------------------------------------------------------------


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

** directory
local dir "$j/Project/Evaluation/GAVI/Mozambique/pcv_impact/"

** input file
local ipdOrigFile "`dir'/data/ipd_monthly_rates_final_cism_Mozambique.xls"
local xrcpOrigFile "`dir'/data/xrcp_monthly_rates_final_cism_mozambique.xls"
local ipdNewFile "`dir'/data/ipd_mrates_elig.xls"
local xrcpNewFile "`dir'/data/xrcp_mrates_elig.xls"

** output for graphs
local graphOutFile "`dir'/output/eligibility_comparison"
** ----------------------------------------------------------------------------------------------


** -------------------------------------------------------------------------------
** Load/prep data

local f = 1
foreach file in ipdOrigFile xrcpOrigFile ipdNewFile xrcpNewFile {

	** load
	import excel "``file''", clear firstrow

	** format date
	capture rename Month month
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

	if ("`file'"=="ipdOrigFile") {
		rename IPDcases total_cases_`file'
		rename PCV10VT pcv10_cases_`file'
	}
	if ("`file'"=="xrcpOrigFile") rename XRCPcases xrcp_cases_`file'
	if ("`file'"=="ipdNewFile") rename cases pcv10_cases_`file'
	if ("`file'"=="xrcpNewFile") rename cases xrcp_cases_`file'
	
	** collapse if necessary
	collapse (sum) *cases*, by(moyr)
	
	** save
	tempfile `f'
	save ``f'', replace
	local f = `f'+1
}
** -------------------------------------------------------------------------------


** ----------------------------------------------------
** Merge

use `1', clear
foreach f of numlist 2/4 {
	merge 1:1 moyr using ``f'', gen(merge`f')
}
** ----------------------------------------------------


** --------------------------------------------------------------------------------------------------------------------------------------
** Graph
pdfstart using "`graphOutFile'.pdf"

** pcv10
local titles title("IPD (all serotypes)") ytitle("Number of cases in new file") xtitle("Number of cases in original file")
local jitter jitter(2)
local settings mcolor("45 53 142") legend(off) 
local equivalence || function y = x, range(0 8) lcolor(black) lwidth(thin)
scatter pcv10_cases_ipdNewFile total_cases_ipdOrigFile, `titles' `jitter' `settings' `equivalence' name(ipd, replace)

** xrcp
local titles title("XRCP") ytitle("Number of cases in new file") xtitle("Number of cases in original file")
local equivalence || function y = x, range(0 32) lcolor(black) lwidth(thin)
scatter xrcp_cases_xrcpNewFile xrcp_cases_xrcpOrigFile, `titles' `jitter' `settings' `equivalence' name(xrcp, replace)

** combine
graph combine ipd xrcp, title("Comparison of data received Dec. 15 and Nov. 18") note("Jittered to show density", size(vsmall))
pdfappend

** export
pdffinish
capture rm "`graphOutFile'.log"
** --------------------------------------------------------------------------------------------------------------------------------------
