/*----------------------------------------------------*/
      /*  Clean Grades Data for Defendants in Early Adulthood
          File created: Aug 28, 2020
          File updated: Aug 29, 2020
       */
/*----------------------------------------------------*/

* Preamble
clear all
set more off
set varabbrev off

* Find defendants in 18-24
use "$output/clean_case/output/case_demo_clean.dta", clear

*Select defendants between the age of 18 and 24
keep if inrange(age, 18, 24) == 1
keep person_id
duplicates drop
isid person_id

save "$output/clean_grades/output/early_adulthood.dta", replace

* Keep grade data for young adults (18-24)
import delimited "$data/grades.csv", clear
merge 1:1 person_id using "$output/clean_grades/output/early_adulthood.dta", nogen keep(3)

* Construct 9th and 10th GPA for defendants between the age of 18 and 24
foreach g of varlist gr* {
      gen byte n_`g' = cond(`g' == "A", 4, ///
                       cond(`g' == "B", 3, ///
                       cond(`g' == "C", 2, ///
                       cond(`g' == "D", 1, ///
                       cond(`g' == "F", 0, .)))))
      }

      forvalues i = 9/10{
        local grade`i' n_gr`i'_*
         egen gpa`i' = rmean(`grade`i'')
      }

su gpa9
su gpa10

* Keep person id and gpa for grade 9 and 10
keep person_id gpa*
isid person_id
save "$output/clean_grades/output/grades_clean.dta", replace

// EOF
