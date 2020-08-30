/*----------------------------------------------------*/
      /*  Clean Case Data
          File created: Aug 28, 2020
          File updated: Aug 29, 2020
       */
/*----------------------------------------------------*/

* Preamble
clear all
set more off
set varabbrev off

* Import data set
import delimited "$data/case.csv", clear

* Make sure caseid is uid
isid caseid

* Merge (possible to have multiple cases for one person)
* Note: No other variables (except person_id) in demo and case sharing the same variable name
merge m:1 person_id using "$output/clean_demo/output/demo_clean.dta", nogen keep(3)

* Restrict the data to only individuals arrested in Chicago
replace address = lower(address)
keep if strpos(address, ", chicago") > 0

* Calculate age
qui: codebook arrest_date bdate
foreach var of varlist arrest_date bdate{
        gen `var'_dt = date(`var', "YMD")
      }
      gen age = round((arrest_date_dt - bdate_dt)/365.25,0.1)

* Save cleaned case and demographic data
save "$output/clean_case/output/case_demo_clean.dta", replace

// EOF
