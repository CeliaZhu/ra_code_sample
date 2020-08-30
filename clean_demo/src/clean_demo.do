/*----------------------------------------------------*/
      /*    Clean Demographic Data
            File created: Aug 28, 2020
            File updated: Aug 29, 2020
      */
/*----------------------------------------------------*/

* Preamble
clear all
set more off
set varabbrev off

* import demographic data
import delimited "$data/demo.csv", clear

* Make sure person_id is uid
duplicates drop
isid person_id

* Recode gender so that males are consistently code as "M", and females "F"
tab gender, m
replace gender = "F" if gender == "female"
replace gender = "M" if gender == "male"

* Check if gender is consistently coded
assert gender == "M" | gender == "F"

* Save cleaned demographic data
save "$output/clean_demo/output/demo_clean.dta", replace

// EOF
