/*----------------------------------------------------*/
        /*  Statistical Analysis
            File created: Aug 28, 2020
            File updated: Aug 29, 2020
         */
/*----------------------------------------------------*/

clear all
set more off
set varabbrev off

global data /Users/celiazhu/Box/projects/ra_code_sample/data
global output /Users/celiazhu/Box/projects/ra_code_sample
global code /Users/celiazhu/Box/projects/ra_code_sample

* 1) Clean demographic data
do "$code/clean_demo/src/clean_demo"

* 2) Clean case data and merge with demographic data
do "$code/clean_case/src/clean_case"

* 3) Clean grade data for defendants in early adulthood
do "$code/clean_grades/src/clean_grades"

* 4) Statistical analysis
do "$code/analysis/src/analysis"

// EOF