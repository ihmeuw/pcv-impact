** ----------------------------------------------------------------
** David Phillips
**
** 11/17/2015
** Comparison of IPD and X-Ray confirmed pneumonia data
** ----------------------------------------------------------------


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


** ------------------------------------------------------------------------------------------------
** Files and directories

** directory
local dir "$j/Project/Evaluation/GAVI/Mozambique/pcv_impact/"

** input files
local inFileIPD "`dir'/data/ipd_monthly_rates_final_cism_Mozambique.xls"
local inFileXRCP "`dir'/data/xrcp_monthly_rates_final_cism_mozambique.xls"

** output for graphs
local outFile "`dir'/output/ipd_xrcp_comparison"
** ------------------------------------------------------------------------------------------------


** ---------------------------------------------------------------------------
** Load/prep data

** load IPD
import excel "`inFileIPD'", clear firstrow
tempfile ipd
save `ipd', replace

** load xrcp
import excel "`inFileXRCP'", clear firstrow

** merge
merge 1:1 Month using `ipd', keep(1 2 3) nogen

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
sort moyr
** ---------------------------------------------------------------------------


** ------------------------------------------------------------------------------------------------
** Scatterplot 

** open pdf
pdfstart using "`outFile'.pdf"

** graph settings
sum IPDcases XRCPcases
local min = `r(min)'
local max = ceil(`r(max)'/10)*10
cor IPDcases XRCPcases
local rho = round(`r(rho)',.01)
local ovsettings legend(off) title("Invasive vs X-Ray Confirmed Pneumonia") ytitle("IPD Cases") xtitle("XRCP Cases") text(`max' `min' "{&rho} = 0`rho'", placement(5) size(small))

** graph
tw function y=x, lcolor(black) sort lwidth(vthin) range(`min' `max') || scatter IPDcases XRCPcases, mcolor("31 120 180") `ovsettings'
pdfappend
** ------------------------------------------------------------------------------------------------


** ------------------------------------------------------------------------------------------------
** Weighted Scatterplot 

** make rates
gen ipdRate = IPDcases/TimeatRisk*1000
gen xrcpRate = XRCPcases/TimeatRiskdays*1000

** graph settings
sum ipdRate xrcpRate
local min = `r(min)'
local max = ceil(`r(max)'/.1)*.1
cor IPDcases XRCPcases
local rho = round(`r(rho)',.01)
local ovsettings legend(off) title("Invasive vs X-Ray Confirmed Pneumonia") ytitle("IPD Incidence Rate" "(per 1000)") xtitle("XRCP Incidence Rate" "(per 1000)") text(`max' `min' "{&rho} = 0`rho'", placement(5) size(small))

** graph
tw function y=x, lcolor(black) sort lwidth(vthin) range(`min' `max') || scatter ipdRate xrcpRate, mcolor("31 120 180") `ovsettings'
pdfappend
** ------------------------------------------------------------------------------------------------


** ------------------------------------------------------------------------------------------------
** Line graph

** graph settings
local color1 "178 223 138"
local color2 "51 160 44"
local color3 "166 206 227"
local color4 "31 120 180"
sum moyr
local min = floor(`r(min)')
local max = ceil(`r(max)')
local s1settings mcolor("`color2'") connect(l) lcolor("`color1'")
local s2settings mcolor("`color4'") connect(l) lcolor("`color3'")
local ovsettings xlabel(`min'(2)`max') title("Invasive and X-Ray Confirmed Pneumonia") ytitle("Cases") xtitle("Month") legend(order(1 2) label(1 "IPD Cases") label(2 "XRCP Cases") symxsize(*.5))

** line graph with breaks for missing data
tw scatter IPDcases moyr, `s1settings' || scatter XRCPcases moyr if moyr<2006.25, `s2settings' || scatter XRCPcases moyr if moyr>2006.25 & moyr<2013, `s2settings' || scatter XRCPcases moyr if moyr>2013, `s2settings' `ovsettings'
pdfappend
** ------------------------------------------------------------------------------------------------


** ------------------------------------------------------------------------------------------------
** Line graph rates

** graph settings
local color1 "178 223 138"
local color2 "51 160 44"
local color3 "166 206 227"
local color4 "31 120 180"
sum moyr
local min = floor(`r(min)')
local max = ceil(`r(max)')
local s1settings mcolor("`color2'") connect(l) lcolor("`color1'")
local s2settings mcolor("`color4'") connect(l) lcolor("`color3'")
local ovsettings xlabel(`min'(2)`max') title("Invasive and X-Ray Confirmed Pneumonia") ytitle("Incidence Rate" "(per 1000)") xtitle("Month") legend(order(1 2) label(1 "IPD") label(2 "XRCP") symxsize(*.5))

** line graph with breaks for missing data
tw scatter ipdRate moyr, `s1settings' || scatter xrcpRate moyr if moyr<2006.25, `s2settings' || scatter xrcpRate moyr if moyr>2006.25 & moyr<2013, `s2settings' || scatter xrcpRate moyr if moyr>2013, `s2settings' `ovsettings'
pdfappend

** close pdf
pdffinish
capture rm "`outFile'.log"
** ------------------------------------------------------------------------------------------------
