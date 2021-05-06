//**********************************************************************/
/*  Evaluation Task
    Author: Xiling (Celia) Zhu xiling@uchicago.edu
    Date: November 09, 2020 */
/**********************************************************************/

clear all
cap log close
set more off
set varabbrev off

//* [> Set Directories <] */
* Data directory
global path "C:/Users/Zhuxl/Box/projects/ra_code_sample/event_study"
* Data directory
global data "$path/data"
* Figure directory
global fig "$path/output/fig"
* Table directory
global tbl "$path/output/tbl"

//*----------------------------------------------------*/
      /* [>   1.  Summary statistics   <] */
/*----------------------------------------------------*/
import delimited "$data/nlsy79-prepared.csv", clear
label var gender "Gender"

//* [> Count moves across regions and between urban non urban<] */

// Move across regions
* Inspect region variable
codebook region

* Destring `region` to perform subtraction on `region`
destring region, force replace
label def region_lab 1"Region 1" 2"Region 2" 3"Region 3" 4"Region 4"
label values region region_lab

/* Define "moving across US regions":
Within each individual, if the region code at t is different from the region
code at (t-1), then we define the individual moved across regions sometime between
t and (t-1).

To avoid double counting, let year t, as opposed to year (t-1), be the year that
the individual moved.

Since we don't have information of 1978, assume "moving across US regions" in 1979
to be missing. In the same spirit, for those observations that follow a year with
a missing region, assume "moving across US regions" to be missing as well, since we
have no information aout where the individual lived in the prior year.
*/

* Initiate indicator varaible for moving across regions
gen move_region = .
label var move_region "Move across regions"
label def move_lab 1"Moved" 0"Stayed"
label values move_region move_lab

* Apply the rules in the above comment block
bysort i: replace move_region = 0 if region[_n] == region[_n-1] & region != .
bysort i: replace move_region = 1 if region[_n] != region[_n-1] & region != .
bysort i: replace move_region = . if _n == 1 | region[_n-1] == .

// Move between urban and non urban
* Inspect urban variable
codebook urban

/* What do 0, 1, and 2 in `urban` stand for:
We noticed that, excluding "NA", the urban variable has three possible values.
Starting 2000, NLS added the option "Unknown" to the suvey question
"IS R'S CURRENT RESIDENCE URBAN/RURAL?" That's why in our dataset we see three
possible values (excluding "NA") in the otherwise binary variable `urban`.

According to the codebook published by NLS,
https://www.nlsinfo.org/investigator/pages/search?s=NLSY79#
0 means the respondent's current residence is in rural areas, 1 urban, and 2 unkown.

Since "Unknown" gives us no information whether the respondent lived in non-urban or
urban residence, treat it as a missing value.
*/

destring urban, force replace
replace urban = . if urban == 2
label def urban_lab 1"Urban" 0"Non urban"
label values urban urban_lab

/* Define "moving between urban and non-urban":
Within each individual, if the urban code at t is different from the urban
code at (t-1), then we define the individual moved between urban and non-urban areas
sometime between t and (t-1).

To be consistent with the rules we laid out when counting moves across regions,
let year t, as opposed to year (t-1), be the year that the individual moved;
assume "moving between urban and non-urban" in 1979 to be missing; for those
observations that follow a year with a missing `urban`, assume
"moving between urban and non-urban" to be missing for those observations.
*/

* Initiate indicator variable for moving between urban and non urban
gen move_urban = .
label var move_urban "Move between urban and non-urban areas"
label values move_urban move_lab
* Apply the rules in defining "moving between urban and non-urban"
bysort i: replace move_urban = 0 if urban[_n] == urban[_n-1] & urban != .
bysort i: replace move_urban = 1 if urban[_n] != urban[_n-1] & urban != .
bysort i: replace move_urban = . if _n == 1 | urban[_n-1] == .

// count moves
qui estpost su move_region move_urban
esttab using "$tbl/move_counts.tex", ///
  cells("sum(fmt(%12.2gc)) count(fmt(%12.2gc))") replace ///
  nonumber noobs nomtitle booktabs collabel("Count" "Observations") ///
  width(\textwidth) label

/* [> Mean wages, employment, and education attainment in each region and urban/non urban <] */
* Inspect wage and educ
codebook wage
codebook educ

// Prepare wage, employment, and education attainment data to calculate mean
* Wage
destring wage, replace force
label var wage "Wage"
* Employment
gen empl = cond(missing(wage), ., cond(wage > 0, 1, 0))
label var empl "Employment"
label def empl_lab 1"Employed" 0"Unemployed"
label values empl empl_lab
* Education attainment
destring educ, replace force
label var educ "Education attainment"

// Mean by region
eststo clear
eststo: estpost tabstat wage empl educ, statistics(mean sd count) by(region) ///
  nototal listwise
esttab using "$tbl/mean_region.tex", replace ///
cells("wage(fmt(%12.2f %12.2f %12.2gc %12.2f %12.2f %12.2gc %12.2f %12.2f %12.2gc %12.2f %12.2f %12.2gc )) empl educ") ///
  collabel("Wage" "Employment" "Education attainment") ///
  noobs nomtitle nonumber booktabs width(\textwidth)

// Mean by urban
eststo clear
eststo: estpost tabstat wage empl educ, statistics(mean sd count) by(urban) ///
  nototal listwise
esttab using "$tbl/mean_urban.tex", replace ///
  cells("wage(fmt(%12.2f %12.2f %12.2gc %15.4f %15.4f %12.2gc)) empl educ") ///
  collabel("Wage" "Employment" "Education attainment") ///
  noobs nomtitle nonumber booktabs width(\textwidth)

//*----------------------------------------------------*/
      /* [>   2.  Event Studies   <] */
/*----------------------------------------------------*/
// Initialize lag and lead indicator variables
gen urban_lead_0 = 0
gen region_lead_0 = 0
local prefix "urban region"
foreach k of local prefix{
  forvalues i = 1/2{
    gen `k'_lag_`i' = 0
    gen `k'_lead_`i' = 0
}
}

foreach k of local prefix{
  bysort i: replace `k'_lag_2 = 1 if move_`k' == 0 & move_`k'[_n+1] == 0 & move_`k'[_n+2] == 1 & move_`k'[_n+3] == 0 & move_`k'[_n+4] == 0
  bysort i: replace `k'_lag_1 = 1 if `k'_lag_2[_n-1] == 1
  bysort i: replace `k'_lead_0 = 1 if `k'_lag_2[_n-2] == 1
  forvalues i = 1/2{
    bysort i: replace `k'_lead_`i' = 1 if `k'_lead_0[_n-`i'] == 1
  }
}

// Calculate mean and 95% CI
cap drop *_mean_wage*
cap drop *_t
local prefix "urban region"
foreach k of local prefix{
  * Initialize empty columns to store results
  gen `k'_mean_wage = .
  label var `k'_mean_wage "Mean wage by `k'"

  gen `k'_mean_wage_ci_lb = .
  label var `k'_mean_wage_ci_lb "Mean wage CI lower bound by `k'"

  gen `k'_mean_wage_ci_ub = .
  label var `k'_mean_wage_ci_ub "Mean wage CI upper bound by `k'"

  gen `k'_t = .
  forvalues  i = 1/5 {
      replace `k'_t = `i'-3 if _n == `i'
  }
  label var  `k'_t "Years following `k'-move"

  * Calculate mean and 95% CI
  local j = 0
  local tlist `k'_lag_2 `k'_lag_1 `k'_lead_0 `k'_lead_1 `k'_lead_2
  foreach t of local tlist{
    local j = `j'+1
    qui ci means wage if `t' == 1
    replace `k'_mean_wage = r(mean) if _n == `j'
    replace `k'_mean_wage_ci_lb = r(lb) if _n == `j'
    replace `k'_mean_wage_ci_ub = r(ub) if _n == `j'
  }
}

// Plot changes in mean wages for those moved between urban and non urban
sort urban_t
twoway (connected urban_mean_wage urban_t) ///
       (rcap urban_mean_wage_ci_ub urban_mean_wage_ci_lb urban_t), ///
       xtitle("Years relative to the move between urban and non urban", size(small)) ///
       xlabel(-2(1)2, labsize(small) noticks) ///
       ylabel(10000(5000)25000, labsize(small) angle(horizontal) noticks format(%12.0fc)) ///
       ytitle("Mean wage of movers", size(small) xoffset(-1) margin(small)) ///
       legend(label(1 "Mean wage") label(2 "95% CI") size(small)) ///
	   xline(0, lpattern(dash) lcolor(blue)) ///
       graphregion(color(white)) plotregion(fcolor(white))
graph export "$fig/urban_mean_wage.png", replace

// Plot changes in mean wages for those who moved across regions
sort region_t
twoway (connected region_mean_wage region_t) ///
       (rcap region_mean_wage_ci_ub region_mean_wage_ci_lb region_t), ///
       xtitle("Years relative to the move across regions", size(small)) ///
       xlabel(-2(1)2, labsize(small) noticks) ///
       ylabel(10000(5000)25000, labsize(small) angle(horizontal) noticks format(%12.0fc)) ///
       ytitle("Mean wage of movers", size(small) xoffset(-1) margin(small)) ///
       legend(label(1 "Mean wage") label(2 "95% CI") size(small)) ///
	   xline(0, lpattern(dash) lcolor(blue)) ///
       graphregion(color(white)) plotregion(fcolor(white))
graph export "$fig/region_mean_wage.png", replace

//*----------------------------------------------------*/
      /* [>   3.  Comparing movers to stayers   <] */
/*----------------------------------------------------*/
// Create dummies to indicate the directions of move
* orgin
forvalues o = 1/4 {
  * arrival
    forvalues a = 1/4{
      if `o' != `a' {
        gen move_`o'_`a' = .
        bysort i: replace move_`o'_`a' = 1 if region == `a' & region[_n-1] == `o'
        bysort i: replace move_`o'_`a' = 0 if region == `o' & region[_n-1] == `o'
      }
    }
  }

// Create variables to record years of stay
/* Why create variables to record years of stay:
In part 2) event studies, we saw that the movers' mean wage decreased in their
first year of moving (t = 0), but gradually increased in the following years.
If we don't control the years of staying in the same region, we might mistakenly
reach the conclusion that moving decreases wages, and overlooked the fact that
on average, the movers' wages increased and surpassed the original level
in the following years of moving (t=1, 2)
*/

gen stay = 1
replace stay = . if move_region == 1
replace stay = . if missing(move_region)
forvalues r = 1/4{
  bysort i (year) : gen cum_stay_`r' = sum(stay) if region == `r'
}

// Calculate age
replace birth = 1900 + birth
gen age = year - birth
gen agesq = age^2
label var age "Age"
labe var agesq "Age squared"

*-----------------------------------
* Compare the wage of movers to the wage of stayers in both region of orgion
* wage of movers out of region 2 vs. wage of stayers who stayed in region 2
// Define stayers
by i: egen region_min = min(region)
by i: egen region_max = max(region)
gen stayer = cond(missing(region), ., cond(region_min == region_max, 1, 0))
forvalues r = 1/4{
  gen stayer_`r' = cond(missing(stayer), ., cond(stayer == 1 & region == `r', 1, 0))
}

// Define movers
// 1) move from a region (region of origin)
* Originate from region 1
gen move_from_1 = .
replace move_from_1 = 0 if stayer == 1
by i: replace move_from_1 = 1 if sum(move_1_2) != 0 | sum(move_1_3) != 0 | sum(move_1_4) != 0
by i: replace move_from_1 = 0 if missing(move_from_1) & !missing(region)
* Originate from region 2
gen move_from_2 = .
replace move_from_2 = 0 if stayer == 1
by i: replace move_from_2 = 1 if sum(move_2_1) != 0 | sum(move_2_3) != 0 | sum(move_2_4) != 0
by i: replace move_from_2 = 0 if missing(move_from_2) & !missing(region)
* Originate from region 3
gen move_from_3 = .
replace move_from_3 = 0 if stayer == 1
by i: replace move_from_3 = 1 if sum(move_3_1) != 0 | sum(move_3_2) != 0 | sum(move_3_4) != 0
by i: replace move_from_3 = 0 if missing(move_from_3) & !missing(region)
* Originate from region 4
gen move_from_4 = .
replace move_from_4 = 0 if stayer == 1
by i: replace move_from_4 = 1 if sum(move_4_1) != 0 | sum(move_4_2) != 0 | sum(move_4_3) != 0
by i: replace move_from_4 = 0 if missing(move_from_4) & !missing(region)

// 2) move to a region (region of arrival)
* Move to region 1
gen move_to_1 = .
replace move_to_1 = 0 if stayer == 1
by i: replace move_to_1 = 1 if sum(move_2_1) != 0 | sum(move_3_1) != 0 | sum(move_4_1) != 0
by i: replace move_to_1 = 0 if missing(move_to_1) & !missing(region)
* Move to region 2
gen move_to_2 = .
replace move_to_2 = 0 if stayer == 1
by i: replace move_to_2 = 1 if sum(move_1_2) != 0 | sum(move_3_2) != 0 | sum(move_4_2) != 0
by i: replace move_to_2 = 0 if missing(move_to_2) & !missing(region)
* Move to region 3
gen move_to_3 = .
replace move_to_3 = 0 if stayer == 1
by i: replace move_to_3 = 1 if sum(move_1_3) != 0 | sum(move_2_3) != 0 | sum(move_4_3) != 0
by i: replace move_to_3 = 0 if missing(move_to_3) & !missing(region)
* Move to region 4
gen move_to_4 = .
replace move_to_4 = 0 if stayer == 1
by i: replace move_to_4 = 1 if sum(move_1_4) != 0 | sum(move_2_4) != 0 | sum(move_3_4) != 0
by i: replace move_to_4 = 0 if missing(move_to_4) & !missing(region)

* Begin comparison--------------------------------------------------------------
// Compare people who moved FROM region r and stayed at region r
* Wage on ln scale
gen lnwage = ln(wage)
label var lnwage "Log wage" 
* No cluster
eststo clear
forvalues r = 1/4{
  gen origin_`r' = .
  replace origin_`r' = 0 if stayer_`r' == 1
  replace origin_`r' = 1 if move_from_`r' == 1
  label var origin_`r' "Move from region `r'"
  eststo: qui reg lnwage origin_`r' age agesq educ gender
}

esttab using "$tbl/compare_origin_simple.tex", se replace ///
  order(origin_* age) nonumber label ///
  booktabs width(\textwidth)
  
* Cluster on cohort
eststo clear
forvalues r = 1/4{
  eststo: qui reg lnwage origin_`r' age agesq educ gender, vce(cluster birth)
}

esttab using "$tbl/compare_origin_cluster.tex", se replace ///
  order(origin_* age) nonumber label ///
  booktabs width(\textwidth)
  

// Compare people who moved TO region r and stayed at region r

* No cluster
eststo clear
forvalues r = 1/4{
  gen arrival_`r' = .
  replace arrival_`r' = 0 if stayer_`r' == 1
  replace arrival_`r' = 1 if move_to_`r' == 1
  label var arrival_`r' "Move to region `r'"
  eststo: qui reg lnwage arrival_`r' age agesq educ gender
}
esttab using "$tbl/compare_arrival_simple.tex", se replace ///
  order(arrival_* age) nonumber label ///
  booktabs width(\textwidth)
  
 * Cluster on cohort
 eststo clear
 forvalues r = 1/4{
	eststo: qui reg lnwage arrival_`r' age agesq educ gender, vce(cluster birth)
 }
 esttab using "$tbl/compare_arrival_cluster.tex", se replace ///
  order(arrival_* age) nonumber label ///
  booktabs width(\textwidth)
  
