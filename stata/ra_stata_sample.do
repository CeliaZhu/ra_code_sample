capture log close
log using "ra_stata_sample", smcl replace
//_1
clear all
set more off
set varabbrev off

* Set up directories
global data /Users/celiazhu/Box/projects/ra_code_sample/data
global output /Users/celiazhu/Box/projects/ra_code_sample/stata/output
global processed /Users/celiazhu/Box/projects/ra_code_sample/stata/processed
//_2
* Import demographic data
import delimited "$data/demo.csv", clear

* Make sure person_id is uid
duplicates drop
isid person_id
//_3
tab gender, m
replace gender = "F" if gender == "female"
replace gender = "M" if gender == "male"

* Check if gender is consistently coded
assert gender == "M" | gender == "F"

* Save cleaned demographic data
save "$processed/demo_clean.dta", replace
//_4
* Import arrests data
import delimited "$data/case.csv", clear

* Make sure caseid is uid
isid caseid

merge m:1 person_id using "$processed/demo_clean.dta", nogen keep(3)
//_5
  replace address = lower(address)
  keep if strpos(address, ", chicago") > 0
//_6
* Glimpse the `arrest_date` and `bdate` to know their formats.
codebook arrest_date bdate
foreach var of varlist arrest_date bdate{
gen `var'_dt = date(`var', "YMD")
  }
gen age = round((arrest_date_dt - bdate_dt)/365.25,0.1)

* Save cleaned case and demographic data
save "$processed/case_demo_clean.dta", replace
//_7
* Import grade data
import delimited "$data/grades.csv", clear

* Construct 9th and 10th GPA for defendants between the age of 18 and 24
foreach g of varlist gr* {
  gen n_`g' = cond(`g' == "A", 4, ///
          cond(`g' == "B", 3, ///
          cond(`g' == "C", 2, ///
          cond(`g' == "D", 1, ///
          cond(`g' == "F", 0, .)))))
  }

forvalues i = 9/10{
  local grade`i' n_gr`i'_*
  egen gpa`i' = rmean(`grade`i'')
}

* Sanity check on GPAs
su gpa*

* Keep person id and gpa for grade 9 and 10
keep person_id gpa*
isid person_id

* Save cleaned grades data
save "$processed/grades_clean.dta", replace
//_8
use "$processed/case_demo_clean.dta", clear

* The study population should have 25,000 subjects
assert(_N == 25000)

* Create dummies for gender and race.
tab gender, m gen(gender_)
rename gender_1 female
label var female "Female"
rename gender_2 male
label var male "Male"

tab race, m gen(race_)
rename race_1 asian
rename race_2 black
rename race_3 white
label var asian "Asian"
label var black "Black"
label var white "White"

* Label variables for cleaner output.
label var age "Age"
label var prior_arrests "Number of prior arrests"
label var re_arrest "Re-arrested"
label var treat "Enrolled into program"

label define treat_lab 0 "Unenrolled" 1 "Enrolled"
label define male_lab 0"Female" 1"Male"
label define black_lab 0"Non-Black" 1"Black"
label define white_lab 0"Non-White" 1"White"

foreach var of varlist treat male black white {
  label values `var' `var'_lab
}

* Save cleaned data with dummies
save "$processed/analysis_data.dta", replace

* Define a local macro for covariates.
local balancevar "female male asian black white prior_arrests age"
//_9
eststo clear
qui estpost su `balancevar'
esttab using "$output/summary_statistics.tex", replace ///
cells("mean(fmt(2)) sd(fmt(0 0 0 0 0 0 1)) min(fmt(0 0 0 0 0 0 1)) max(fmt(0 0 0 0 0 0 1))") ///
collabel("Mean" "Standard Deviation" "Min" "Max" ) ///
width(\textwidth) nonumber  label
//_10
iebaltab `balancevar', grpvar(treat) ///
vce(robust) savetex("$output/balance_test.tex") replace ///
rowvarlabels pttest ftest fnoobs pftest
//_11
preserve
encode race, gen(n_race)
qui: codebook n_race

gen avg = .
gen ci_low = .
gen ci_high = .

* Calculate means and confidence intervals
qui: mean prior_arrests, over(treat n_race)
matrix M = r(table)
matrix list M

forvalues i = 1/6 {
  if inrange(`i', 1, 3) == 1{
replace avg = M[1, `i'] if treat == 0 & n_race == `i'
replace ci_low = M[5, `i'] if treat == 0 & n_race == `i'
replace ci_high = M[6, `i'] if treat == 0 & n_race == `i'
  }

  if inrange(`i', 4, 6) == 1 {
  replace avg = M[1, `i'] if treat == 1 & n_race == `i'-3
replace ci_low = M[5, `i'] if treat == 1 & n_race == `i'-3
replace ci_high = M[6, `i'] if treat == 1 & n_race == `i'-3
  }
}

* Count observations
forvalues i = 1/6 {
  if inrange(`i', 1, 3) == 1 {
count if treat == 0 & n_race == `i' & !missing(prior_arrests)
  }

  if inrange(`i', 4, 6) == 1 {
  count if treat == 1 & n_race == `i'-3 & !missing(prior_arrests)
   }

   local `i'N = r(N)
 }

* Plot the bar chart
gen treat_race = n_race if treat == 0
replace treat_race = n_race + 4 if treat == 1
twoway (bar avg treat_race if n_race == 1, fcolor(maroon) ///
      fintensity(inten70) lcolor(white) barw(0.7)) ///
    (bar avg treat_race if n_race == 2, fcolor(maroon) ///
      fintensity(inten50) lcolor(white) barw(0.7)) ///
    (bar avg treat_race if n_race == 3, fcolor(maroon) ///
      fintensity(inten30) lcolor(white) barw(0.7)) ///
      (rcap ci_low ci_high treat_race, lcolor(gs5)), ///
    legend(row(1) order(1 "Asian" 2 "Black" 3 "White" 4 "95% C.I.")) ///
    xlabel(2"Unenrolled" 6"Enrolled", noticks) xtitle("Enrollment status") ///
    ylabel(0(1)4) ytitle("Average number of prior arrests", ///
    margin(medium) size(medium)) ///
    title("Number of prior arrests imbalanced" ///
    "between enrolled and unenrolled groups", size(medium)) ///
    note("Number of defendants:" ///
    "Asian unenrolled `1N', Black unenrolled `2N', White unenrolled `3N'" ///
      "Asian enrolled `4N', Black enrolled `5N', White enrolled `6N'") ///
    caption("Source: Cook County State's Attorney's Office", ///
    justification(left) size(vsmall) linegap(0.8) position(5) span) ///
    graphregion( color(white) ) plotregion(fcolor(white))

graph export "$output/prior_arrests.png", replace
restore
//_12
// Get propensity score
* Let female and asian be base groups
local covlist "male black white prior_arrests age"
qui logit treat `covlist'
predict pscore, pr

// Inverse probability weighting (IPW)
* Weight for average treatment effect (ATE)
gen double ate_weight = 1.treat/pscore + 0.treat/(1-pscore)
* Weight for average treatment effect on the treated (ATET)
gen double atet_weight = 1.treat/1 + 0.treat * pscore/(1-pscore)

// Common support
gen support = 1
forvalues i = 1/2 {
    su pscore if treat == `i' -1
    replace support = 0 if inrange(pscore, r(min), r(max)) == 0
  }
//_13
su pscore if support == 1
local lhs = r(min)
local rhs = r(max)
di `lhs'
di `rhs'
histogram pscore, xline(`lhs', lcolor(black) lwidth(thin) lpattern(dash)) ///
xline(`rhs', lcolor(black) lwidth(thin) lpattern(dash)) ///
freq width(0.05) ///
by(treat, row(2) graphregion(color(white)) note("Source: Cook County State's Attorney's Office" ///
"(Observations bounded by vertical lines within common support)", size(small))) ///
fcolor(maroon%70) lcolor(white%0) ///
xtitle("Propensity score", size(medsmall)) ///
ytitle("Frequency", size(medsmall))

graph export "$output/ps_hist.png", replace
//_14
eststo clear
qui estpost su `balancevar' if support == 1 [aw = ate_weight]
esttab using "$output/summary_statistics_cs.tex", replace ///
cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(0 0 0 0 0 0 1)) max(fmt(0 0 0 0 0 0 1))") ///
collabel("N" "Mean" "Standard Deviation" "Min" "Max" ) ///
width(\textwidth) nonumber  label
//_15
iebaltab `balancevar'  if support == 1 [pw = ate_weight], grpvar(treat)  ///
vce(robust) savetex("$output/balance_test_ipw.tex") replace ///
rowvarlabels pttest ftest fnoobs pftest
//_16
unique person_id
eststo clear
eststo: qui reg re_arrest treat `covlist', rob
//_17
eststo: logit re_arrest treat `covlist', rob cluster(person_id)
//_18
eststo: qui reg re_arrest treat [pw = ate_weight] if support == 1
eststo: qui reg re_arrest treat [pw = atet_weight] if support == 1
//_19
esttab using "$output/estimation.tex", se  label nobaselevels noomitted ///
addnotes("Model 1: OLS; Model 2: Logit; Model 3: IPW (ATE); Model 4: IPW (ATET)" ///
"Odds ratio reported in logit model" ///
"Robust s.e. reported in OLS model; clustered robust s.e. reported in logit model") ///
eform(0 1 0 0 ) replace eqlabels(none)
//_^
log close
