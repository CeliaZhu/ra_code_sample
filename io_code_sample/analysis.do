
clear all
cap log close
set more off
set varabbrev off

/* Directories */
* Data directory
global data "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/data"
* Figure directory
global figure "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/output/figure"
* Table directory
global table "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/output/table"
* Log directory
global log "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/output/log"
log using "$log/stata_code_sample.log", replace

* Part 1 Data Cleaning
* The central task is to merge car data and market data into a panel with
* 3 dimensions: car model, market, and year.

* append al car data
local carcsv: dir "$data/car_data" files "*.csv"
foreach file of local carcsv {
  preserve
  insheet using "$data/car_data/`file'", clear
  cap destring sp, force replace // change sp (maximum speed) from string to numeric
  replace ye = 1900 + ye // use full year
  foreach var of varlist ma loc {
    replace `var' = "United Kingdom" if `var' == "UK" // use full country name
  }
  save temp, replace
  restore
  append using temp
}

save "$data/clean_data/car_data_all.dta", replace
rm temp.dta

* append all market data
clear
local mktcsv: dir "$data/market_data" files "*.csv"
foreach file of local mktcsv {
  preserve
  insheet using "$data/market_data/`file'", clear
  save temp, replace
  restore
  append using temp
}

* spell out full country names
local country_list `" "Belgium" "France" "Germany" "Italy" "United Kingdom" "'
foreach country in `country_list' {
replace ma = "`country'" if ma == substr("`country'", 1, 1)
}
save "$data/clean_data/market_data_all.dta", replace
rm temp.dta

* merge car anad market data
merge 1:m ye ma using "$data/clean_data/car_data_all.dta", nogen

* fix missing values of fuel consumption variables
destring li*, force replace
replace li = (li1 + li2 + li3)/3 if li == .
foreach var of varlist li1 li2 li3 {
  replace `var' = 0 if `var' == .
  replace `var' = li*3 - (li1 + li2 + li3) if `var' == 0
}

* fix storage type
destring ac, force replace // ac (time to acceleration) is float
foreach var of varlist ma loc brand model cla frm {
  encode `var', gen(`var'_code)
}

* label needed variables
label var hp "Horsepower (in kW)"
* store cleaned data
save "$data/clean_data/all_data.dta", replace

* Part 2 Data Exploration
* For the years 1970 and 1990, group cars by decile of observed horsepower (hp)
* in that year, and then compute the sales-weighted average of fuel consumption
* for cars in each horsepower decile.
xtile hp_decile_70 = hp if ye == 1970, nq(10)
xtile hp_decile_90 = hp if ye == 1990, nq(10)

* Visualize the distrubition of observed horsepower in 1970 and 1990
gen ye_decile = .
foreach year of numlist 1970 1990 {
  replace ye_decile = `year' if ye == `year'
}
histogram hp, freq bin(10) by(ye_decile, graphregion(color(white)) ///
title("Distribution of Horsepower in 1970 and 1990", size(medium)) note("")) ///
ylabel (0(10)100) xlabel(10(20)150) ///
fcolor(navy%80) lcolor(white%0)
graph export "$figure/hp_hist.png", replace

* Compute the sales-weighted average of fuel consumption for cars in each decile
gen wtd_li = .
foreach year of numlist 70 90 {
  su qu if !missing(hp_decile_`year')
  local sales_total r(sum) // total number of cars sold in each year
  replace wtd_li = li * qu /`sales_total' if !missing(hp_decile_`year')
  bysort hp_decile_`year': egen wtd_avg_li_`year' = total(wtd_li) if !missing(hp_decile_`year')
}
drop wtd_li

* Combine horsepower decile in different years into one variable
gen hp_decile = cond(!missing(hp_decile_70), hp_decile_70, hp_decile_90)
label var hp_decile "Decile of observed horsepower"

* Combine sales-weighted average of fuel consumption in different years into one variable
gen wtd_avg_li = cond(!missing(wtd_avg_li_70), wtd_avg_li_70, wtd_avg_li_90)
label var wtd_avg_li "Sales-weighted average of fuel consumption"

* For each year, produce a scatterplot of the sales-weighted average of fuel consumption
* versus the midpoint of each horsepower decile.
* Collapse data (one observation per year and horsepower decile group)

log close
* End of file
