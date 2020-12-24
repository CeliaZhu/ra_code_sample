capture log close
log using "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/code_sample", smcl replace
//_1
clear all
cap log close
set more off
set varabbrev off

* Data directory
global data "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/data"

* Figure directory
global figure "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/output/figure"

* Table directory
global table "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/output/table"

* Log directory
global log "/Users/celiazhu/Box/projects/ra_code_sample/io_code_sample/output/log"
log using "$log/stata_code_sample.log", replace
//_2
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
//_3
clear
local mktcsv: dir "$data/market_data" files "*.csv"
foreach file of local mktcsv {
  preserve
  insheet using "$data/market_data/`file'", clear
  save temp, replace
  restore
  append using temp
}
//_4
local country_list `" "Belgium" "France" "Germany" "Italy" "United Kingdom" "'
foreach country in `country_list' {
replace ma = "`country'" if ma == substr("`country'", 1, 1)
}
save "$data/clean_data/market_data_all.dta", replace
rm temp.dta
//_5
merge 1:m ye ma using "$data/clean_data/car_data_all.dta", nogen
//_6
destring li*, force replace
replace li = (li1 + li2 + li3)/3 if li == .
foreach var of varlist li1 li2 li3 {
  replace `var' = 0 if `var' == .
  replace `var' = li*3 - (li1 + li2 + li3) if `var' == 0
}
//_7
destring ac, force replace // ac (time to acceleration) is float
foreach var of varlist ma loc brand model cla frm {
  encode `var', gen(`var'_code) // transform string to categorical variables
}
//_8
label var hp "Horsepower (kW)"
label var li "Fuel consumption (liter per km)"
label var eurpr "Price in common currency"
//_9
save "$data/clean_data/all_data.dta", replace
//_10
preserve
tempfile data_70
keep if ye == 1970
keep ye qu hp li
save `data_70', replace
restore

preserve
tempfile data_90
keep if ye == 1990
keep ye qu hp li
save `data_90', replace
restore
//_11
foreach year of numlist 70 90 {
  use `data_`year'', clear
  xtile hp_decile = hp, nq(10)
  bysort hp_decile: asgen wtd_avg_li = li, weight(qu)
  gen loghp = ln(hp)
  * fit curves; use sales as sample weights
  reg li hp loghp [pw = qu]
  predict li_hat
  * range and midpoint of each decile group
  gen hp_mid = .
  gen hp_min = .
  gen hp_max = .
  * total sales of each decile group
  gen qu_total = .
  * loop over each horsepower decile group
  forvalues  i = 1/10 {
  * horsepower summary statistics
  su hp if hp_decile == `i'
  local hp_min r(min)
  local hp_max r(max)
  replace hp_min = `hp_min' if hp_decile == `i'
  replace hp_max = `hp_max' if hp_decile == `i'
  replace hp_mid = (hp_min + hp_max)/2 if hp_decile == `i'
  * sales summary statistics
  su qu if hp_decile == `i'
  local qu_total r(sum)
  replace qu_total = `qu_total' if hp_decile == `i'
  }
  save `data_`year'', replace
}

* Aggregated data of fuel consumption and horsepower in 1970 and 1990
use `data_70', clear
append using `data_90'
collapse (first) wtd_avg_li hp_min hp_max hp_mid li_hat qu_total, by(ye hp_decile)
label var wtd_avg_li "Sales-weighted average of fuel consumption"
label var hp_mid "Midpoint of horsepower decile"
//_12
twoway (scatter wtd_avg_li hp_mid if ye == 1970, mcolor(navy%70)) ///
(scatter wtd_avg_li hp_mid if ye == 1990, mcolor(orange%70)) ///
(line li_hat hp_mid if ye == 1970, lcolor(navy%70) lpattern(shortdash)) ///
(line li_hat hp_mid if ye == 1990, lcolor(orange%70) lpattern(longdash)), ///
xlabel(15(15)150) ylabel(5(2)15) ///
xtitle("Midpoint of each horsepower decile") ///
ytitle("Sales-weighted average of fuel consumption") ///
title("Relationship between Fuel Consumption and Horsepower in 1970 and 1990", ///
size(medsmall) color(black)) ///
legend(label(1 "1970") label(2 "1990") ///
label(3 "1970 fitted curve") label(4 "1990 fitted curve") ///
nobox region(lcolor(white)) size(small) rows(1)) ///
graphregion(color(white))
graph export "$figure/relation_li_hp.png", replace
//_13
* prepare data to generate texsave table
reshape wide wtd_avg_li hp_min hp_max hp_mid li_hat qu_total, i(hp_decile) j(ye)
foreach year of numlist 1970 1990 {
  * horspower range of each decile group
  egen hp_range`year' = concat(hp_min`year' hp_max`year'), punct("--")
  order hp_range`year', before(qu_total`year')
}

* drop un-needed variables
drop hp_min* hp_max* hp_mid* li_hat*

* generate summary statistics table
local file_name hp_sum_table
local title title("Sales-Weighted Average of Fuel Consumption by Decile of Horsepower")
local midliners "\cmidrule(lr){2-4} \cmidrule(lr){5-7} \addlinespace[-2.5ex]"
local colnames "{Decile Group} &{Fuel consumption} &{Range} &{Sales} &{Fuel consumption} &{Range} &{Sales}"
local headerlines headerlines("& \multicolumn{3}{c}{1970} & \multicolumn{3}{c}{1990}" "`midliners'" "`colnames'")
local fn footnote("Fuel consumption represents the sales-weighted average of fuel consumption")
local marker marker("tbl1")
local size size("small")
texsave using "$table/`file_name'.tex", replace frag nonames `headerlines' `marker' `fn' `size' `title'
//_14
use "$data/clean_data/all_data.dta", clear

* Generate the number of consumers in market j in year t
gen n_consumers = pop/4
label var n_consumers "Number of consumers"

* Generate market share of car model i in market j in year t
gen share = qu/n_consumers
label var share "Market share"

* Generate share of consumers who by no cars in market j in year t
bysort ma_code ye: egen qu_total = total(qu)
gen share_no_cars = 1 - qu_total/n_consumers
label var share_no_cars "Share of consumers who buy no cars"

* Generate outcome variable
gen outcome = ln(share) - ln(share_no_cars)
label var outcome "Outcome"

* Regress outcome variable on a constant, fuel consumption, and price
eststo clear
eststo: qui reg outcome li eurpr, r
estadd local fe "None"
//_15
* Including car model, market, and year fixed effects
eststo: qui reg outcome li eurpr i.ma_code i.model_code i.ye, r
estadd local fe "car model, market, and year"

esttab using "$table/linear.tex", replace se drop(*.ma_code *.model_code *.ye) ///
booktabs width(\textwidth) label scalars("fe Fixed effects")
//_^
log close
