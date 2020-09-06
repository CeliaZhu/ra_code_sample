capture log close
log using "ra_stata_sample", smcl replace
//_1
  clear all
  set more off
  set varabbrev off
//_2
  global data /Users/celiazhu/Box/projects/ra_code_sample/data
  global output /Users/celiazhu/Box/projects/ra_code_sample/output
  global processed /Users/celiazhu/Box/projects/ra_code_sample/processed
//_3
  import delimited "$data/demo.csv", clear
//_4
  duplicates drop
  isid person_id
//_5
  tab gender, m
  replace gender = "F" if gender == "female"
  replace gender = "M" if gender == "male"
//_6
  assert gender == "M" | gender == "F"
//_7
  save "$processed/demo_clean.dta", replace
//_8
  import delimited "$data/case.csv", clear
//_9
  isid caseid
//_10
  merge m:1 person_id using "$processed/demo_clean.dta", nogen keep(3)
//_11
  replace address = lower(address)
  keep if strpos(address, ", chicago") > 0
//_12
  codebook arrest_date bdate
//_13
  foreach var of varlist arrest_date bdate{
    gen `var'_dt = date(`var', "YMD")
  }
  gen age = round((arrest_date_dt - bdate_dt)/365.25,0.1)
//_14
  save "$processed/case_demo_clean.dta", replace
//_15
  import delimited "$data/grades.csv", clear
//_16
  foreach g of varlist gr* {
    gen n_`g' = cond(`g' == "A", 4, ///
                cond(`g' == "B", 3, ///
                cond(`g' == "C", 2, ///
                cond(`g' == "D", 1, ///
                cond(`g' == "F", 0, .)))))
  }
//_17
  forvalues i = 9/10{
    local grade`i' n_gr`i'_*
     egen gpa`i' = rmean(`grade`i'')
  }
//_18
  su gpa*
//_19
  keep person_id gpa*
  isid person_id
  save "$processed/grades_clean.dta", replace
//_20
  use "$processed/case_demo_clean.dta", clear
//_21
  assert(_N == 25000)
//_22
  label var age "Age"
  label var prior_arrests "Number of prior arrests"
  label var re_arrest "Re-arrested"
  label var treat "Enrolled into program"
//_23
  tab gender, m gen(gender_)
  rename gender_1 female
  label var female "Female"
  rename gender_2 male
  label var male "Male"
//_24
  tab race, m gen(race_)
  rename race_1 asian
  rename race_2 black
  rename race_3 white
  label var asian "Asian"
  label var black "Black"
  label var white "White"
//_25
  local balancevar "female male asian black white prior_arrests age"
//_26
  eststo clear
  qui estpost su `balancevar'
  esttab using "$output/summary_statistics.tex", replace ///
  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0 0 0 0 0 0 1)) max(fmt(0 0 0 0 0 1)) count(fmt(0)))") ///
  collabel("Mean" "Standard Deviation" "Min" "Max Number" of "N") ///
  width(\textwidth) nonumber booktabs label
//_27
  balancetable treat `balancevar' using "$output/balance_test.tex", ///
  vce(robust) pval ///
  ctitles("Control group" "Treatment group" "Difference" "P value") ///
  booktabs varlabels replace
//_28
  tempname `balance_test_t' str21 var mean_0 mean_1 t_stat p_value using ///
  balance_test, replace
//_29
  foreach var of local `balancevar' {
    qui reg `var' treat, rob
    local mean_0 = _b[_cons]
    local mean_1 = _b[_cons] + _b[treat]
    local t_stat = _b[treat]/_se[treat]
    local p_value = 2*ttail(e(df_r), abs(`t_stat'))
//_30
    post `balance_test_t' ("`var'") (`mean_0') (`t_stat') (`p_value')
  }
//_31
  postclose `balance_test_t'
//_32
  tempname `balance_test_f' f_stat p_value using balance_test_f, replace
//_33
  qui reg treat `var', rob
  testparm `balancevars'
//_34
  local f_stat = r(F)
  local p_value = r(p)
//_35
  post `balance_test_t' (`f_stat') (`p_value')
  postclose `balance_test_f'
//_36
  preserve
  foreach file in balance_test_t.dta balance_test_f.dta {
    use `file', clear
//_37
    * label statistics
    cap label var mean_0 "Mean in unenrolled group"
    cap label var mean_1 "Mean in enrolled group"
    cap label var t_stat "t-statistic"
    cap label var f_stat "F-statistic"
    cap label var p_value "P Value"
//_38
    * format numbers
    cap tostring mean_0 mean_1 t_stat p_value, replace format(%4.2f) force
    cap tostring f_stat p_value, replace format(%4.2f) force
//_39
    * export into latex
    if "`file'" == "balance_test_t.dta" {
      tesxave * using "$output/balance_test_t.tex", replace ///
      varlabels frag location(H)  ///
      title(Balance test for individual covariates)
    }
//_40
    if "`file'" == "balance_test_f.dta" {
      texsave * using "$output/balance_test_f.tex", replace ///
      varlabels frag location(H) ///
      title(Test joint orthognality of all covariates)
    }
  }
//_41
  preserve
  encode race, gen(n_race)
//_42
  qui: codebook n_race
//_43
  gen avg = .
  gen ci_low = .
  gen ci_high = .
//_44
  qui: mean prior_arrests, over(treat n_race)
  matrix M = r(table)
  matrix list M
//_45
  forvalues i = 1/6 {
      if inrange(`i', 1, 3) == 1{
      replace avg = M[1, `i'] if treat == 0 & n_race == `i'
      replace ci_low = M[5, `i'] if treat == 0 & n_race == `i'
      replace ci_high = M[6, `i'] if treat == 0 & n_race == `i'
      }
//_46
      if inrange(`i', 4, 6) == 1 {
      replace avg = M[1, `i'] if treat == 1 & n_race == `i'-3
      replace ci_low = M[5, `i'] if treat == 1 & n_race == `i'-3
      replace ci_high = M[6, `i'] if treat == 1 & n_race == `i'-3
      }
  }
//_47
  forvalues i = 1/6 {
      if inrange(`i', 1, 3) == 1 {
      count if treat == 0 & n_race == `i' & !missing(prior_arrests)
      }
//_48
      if inrange(`i', 4, 6) == 1 {
      count if treat == 1 & n_race == `i'-3 & !missing(prior_arrests)
      }
//_49
      local `i'N = r(N)
  }
//_50
  gen treat_race = n_race if treat == 0
  replace treat_race = n_race + 4 if treat == 1
//_51
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
//_52
  graph export "$output/prior_arrests.png", replace
  restore
//_53
  unique person_id
//_54
  eststo clear
//_55
  eststo: qui reg re_arrest treat `balancevar', rob
//_56
  eststo: qui logit re_arrest treat `balancevar', rob
//_57
  qui logit treat `balancevar'
  predict pscore
//_58
  forvalues i = 1/2 {
    su pscore if treat == `i' -1
    drop if inrange(pscore, r(min), r(max)) == 0
  }
//_59
  gen ate_weight = (1/pscore) if treat == 1
  replace ate_weight = 1/(1-pscore) if treat == 0
  eststo: qui reg re_arrest treat [pw = ate_weight]
//_60
  gen atet_weight = 1 if treat == 1
  replace atet_weight = pscore/(1-pscore) if treat == 0
  eststo: qui reg re_arrest treat [pw = atet_weight]
//_61
esttab using "$output/estimation.tex", se booktabs label ///
addnotes("Model 1: OLS; Model 2: Logit;" ///
"Model 3: Propensity score matching (ATT); Model 4: Propensity Score Matching (ATET)" ///
"Souce: Cook County State's Attorney's Office") ///
replace numbers
//_^
log close
