/*----------------------------------------------------*/
      /*  Statistical Analysis
          File created: Aug 28, 2020
          File updated: Aug 29, 2020
       */
/*----------------------------------------------------*/

* Preamble
clear all
set more off
set varabbrev off

* Import analysis data set
* Do not use gpa to inform analysis because theses are only for young adults
use "$output/clean_case/output/case_demo_clean.dta", clear

*The study population should have 25,000 subjects
assert(_N == 25000)

* Label variables
label var age "Age"
label var prior_arrests "Number of prior arrests"
label var re_arrest "Re-arrested"

// Balance tests for demographic characteristics
* Create dummies for gender and race
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

* Calculate mean by treatment status, difference in means, and p-value
local balancevar "female male asian black white prior_arrests age"

balancetable treat `balancevar' using "$output/analysis/output/balance_test.tex", ///
vce(robust) pval ///
ctitles("Control group" "Treatment group" "Difference" "P value") ///
booktabs varlabels replace

// Visualize number of prior arrests by enrollment status and race
* Create a numerical variable for race
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

* Plot
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

graph export "$output/analysis/output/prior_arrests.png", replace

restore

// Estimate the effect of the program on reducing the likelihood of re-arrest before disposition

* Specification 1: OLS
eststo clear
eststo: qui reg re_arrest treat `balancevar', rob

* Specification 2: Logit
eststo: qui logit re_arrest treat `balancevar', rob

* Specification 3: Weighted propensity score matching
qui logit treat `balancevar'
predict pscore

** Common support
forvalues i = 1/2 {
      su pscore if treat == `i' -1
      drop if inrange(pscore, r(min), r(max)) == 0
    }

* Weight for average treatment effect (ATE)
gen ate_weight = (1/pscore) if treat == 1
replace ate_weight = 1/(1-pscore) if treat == 0
eststo: qui reg re_arrest treat [pw = ate_weight]

* Weight for average treatment effect on the treated (ATET)
gen atet_weight = 1 if treat == 1
replace atet_weight = pscore/(1-pscore) if treat == 0
eststo: qui reg re_arrest treat [pw = atet_weight]

* Export results of three specifications
esttab using "$output/analysis/output/estimation.tex", se booktabs label ///
addnotes("Model 1: OLS; Model 2: Logit;" ///
"Model 3: Propensity score matching (ATT); Model 4: Propensity Score Matching (ATET)" ///
"Souce: Cook County State's Attorney's Office") ///
replace numbers

// EOF
