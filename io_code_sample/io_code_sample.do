
/**********************************************************************/
/*  Stata Code Sample
    Author: Xiling (Celia) Zhu xiling@uchicago.edu
    Date: Feb 14, 2020
    Output: https://github.com/CeliaZhu/ra_code_sample/blob/master/io_code_sample/io_code_sample_report.pdf
*/
/**********************************************************************/

/* [> Overview <] */ 

/* This task was inspired by IO research on the automobile industry. I was
provided with car model sales data in five European markets. `car data` contains
the manufacturing characteristics and the quantity sold of each car model within
each market, from 1970 to 1990. `market data` contains the GDP, population, and
tax rate of each market in each year. */

clear all
set more off
set varabbrev off

/* [> Set up directories <] */
global path "/Users/celiazhu/OneDrive - The University of Chicago/JobApplications/dime/code_sample/Stata"
* Data directory
global data "$path/data"
cap mkdir "$data/clean_data"
* Figure directory
cap mkdir "$path/output/figure"
global figure "$path/output/figure"
* Table directory
cap mkdir "path/output/table"
global table "$path/output/table"

/* [> Part 1 Data Cleaning <] */
/* The central task of the section is to merge `car data` and `market data` into
one panel with 3 dimensions: car model ($i$), market ($j$), and year ($t$). In
each row, it contains car model manufacturing characteristics, price, and quantity
sold, market GDP, population, and tax rate. */

// Append all car data.
local carcsv: dir "$data/car_data" files "*.csv"
foreach file of local carcsv {
	preserve
	qui insheet using "$data/car_data/`file'", clear
	qui cap destring sp, force replace // change sp (maximum speed) from string to numeric
	qui replace ye = 1900 + ye // use full year
	foreach var of varlist ma loc {
		qui replace `var' = "United Kingdom" if `var' == "UK" // use full country name
	}
	qui save temp, replace
	restore
	qui append using temp
}
save "$data/clean_data/car_data_all.dta", replace
rm temp.dta

// Append all market data.
clear
local mktcsv: dir "$data/market_data" files "*.csv"
foreach file of local mktcsv {
	preserve
	qui insheet using "$data/market_data/`file'", clear
	qui save temp, replace
	restore
	qui append using temp
}

// Spell out full country names in appended market data.
/* Abbreviated market name corresponds to the first letter of the full country
name, and the correspondence is unique in this dataset. */
local country_list `" "Belgium" "France" "Germany" "Italy" "United Kingdom" "'
foreach country in `country_list' {
	qui replace ma = "`country'" if ma == substr("`country'", 1, 1)
}
save "$data/clean_data/market_data_all.dta", replace
rm temp.dta

// Merge car and market data.
merge 1:m ye ma using "$data/clean_data/car_data_all.dta", nogen

// Fix missing values of fuel consumption variables.
qui destring li*, force replace
replace li = (li1 + li2 + li3)/3 if li == .
foreach var of varlist li1 li2 li3 {
	qui replace `var' = 0 if `var' == .
	qui replace `var' = li*3 - (li1 + li2 + li3) if `var' == 0
}

qui destring ac, force replace // ac (time to acceleration) is float
foreach var of varlist ma loc brand model cla frm {
	qui encode `var', gen(`var'_code) // transform string to categorical variables
}

label var hp "Horsepower (kW)"
label var li "Fuel consumption (liter per km)"
label var eurpr "Price in common currency"

// Save clean data
qui save "$data/clean_data/all_data.dta", replace

/* [> Part 2 Data Exploration <] */
/* This section uses the model-market-year panel dataset to visualize the relationship
between fuel consumption (`li`) and horsepower (`hp`) in the years 1970 and 1990.*/

preserve
// a temporary dataset for 1970 data
tempfile data_70
* keep 1970 data
keep if ye == 1970
* keep neede variables
keep ye qu hp li ma_code model_code
qui save `data_70', replace
restore

preserve
// a temporary dataset for 1970 data
tempfile data_90
* keep 1990 data
keep if ye == 1990
* keep needeed variables
keep ye qu hp li ma_code model_code
qui save `data_90', replace
restore

/* The chunk of codes do the following:
1) group cars by decile of horsepower;
2) fit curves, weighted by sales;
3) calculate summary statistics of horsepower and sales in each decile group */
foreach year of numlist 70 90 {
	use `data_`year'', clear
	// 1) group cars by decile of horsepower
	xtile hp_decile = hp, nq(10)
	bysort hp_decile: asgen wtd_avg_li = li, weight(qu)
	// 2) fit curves; use sales as sample weights
	qui gen loghp = ln(hp)
	sort ye ma_code model_code
	qui reg li hp loghp [pw = qu]
	qui predict li_hat
	// 3) summary statistics of horsepower and sales in each decile group
	qui gen hp_mid = .
	qui gen hp_min = .
	qui gen hp_max = .
	qui gen qu_total = .
	* loop over each horsepower decile group
	forvalues  i = 1/10 {
		* horsepower summary statistics
		qui su hp if hp_decile == `i'
		local hp_min r(min)
		local hp_max r(max)
		qui replace hp_min = `hp_min' if hp_decile == `i'
		qui replace hp_max = `hp_max' if hp_decile == `i'
		* midpoint of each decile group for x-axis of scatterplot
		qui replace hp_mid = (hp_min + hp_max)/2 if hp_decile == `i'
		* sales summary statistics
		qui su qu if hp_decile == `i'
		local qu_total r(sum)
		qui replace qu_total = `qu_total' if hp_decile == `i'
	}
	qui save `data_`year'', replace
}

// Aggregated data of fuel consumption and horsepower in 1970 and 1990
use `data_70', clear
append using `data_90'
collapse (first) wtd_avg_li hp_min hp_max hp_mid li_hat qu_total, by(ye hp_decile)
label var wtd_avg_li "Sales-weighted average of fuel consumption"
label var hp_mid "Midpoint of horsepower decile"

// Scatter plot (figure 1)
local file_name relation_li_hp
twoway (scatter wtd_avg_li hp_mid if ye == 1970, mcolor(navy%70)) ///
(scatter wtd_avg_li hp_mid if ye == 1990, mcolor(orange%70)) ///
(line li_hat hp_mid if ye == 1970, sort lcolor(navy%70) lpattern(shortdash)) ///
(line li_hat hp_mid if ye == 1990, sort lcolor(orange%70) lpattern(longdash)), ///
xlabel(15(15)150) ylabel(5(2)15) ///
xtitle("Midpoint of each horsepower decile (kW)", size(medsmall)) ///
ytitle("Sales-weighted average of fuel consumption (liter per km)", size(medsmall) margin(top)) ///
title("Relationship between Fuel Consumption and Horsepower in 1970 and 1990", ///
size(medsmall) color(black)) ///
legend(label(1 "1970") label(2 "1990") ///
label(3 "1970 fitted curve") label(4 "1990 fitted curve") ///
nobox region(lcolor(white)) size(small) rows(1)) ///
graphregion(color(white))
qui graph export "$figure/`file_name'.png", replace

// Summary statistics (table 1)
* prepare data to generate texsave table
qui gen wtd_avg_li_3 = string(wtd_avg_li,"%4.3f") // round up to 3 decimal places
qui drop wtd_avg_li
rename wtd_avg_li_3 wtd_avg_li
qui reshape wide wtd_avg_li hp_min hp_max hp_mid li_hat qu_total, i(hp_decile) j(ye)
foreach year of numlist 1970 1990 {
	* horsepower range of each decile group
	egen hp_range`year' = concat(hp_min`year' hp_max`year'), punct("--")
	* insert comma in total sales quantity
	gen qu_total`year'_comma = string(qu_total`year', "%15.0fc")
	drop qu_total`year'
	rename qu_total`year'_comma qu_total`year'
}
* reorder variables
order hp_decile wtd_avg_li1970 hp_range1970 qu_total1970 ///
wtd_avg_li1990 hp_range1990 qu_total1990
* drop un-needed variables
drop hp_min* hp_max* hp_mid* li_hat*
* generate summary statistics table
local file_name hp_sum_table
local title title("Sales-weighted average of fuel consumption by decile of horsepower")
local midliners "\cmidrule(lr){2-4} \cmidrule(lr){5-7} \addlinespace[-2.5ex]"
local colnames "{Decile} &{Fuel consumption} &{Horsepower range} &{Sales} &{Fuel consumption} &{Horsepower range} &{Sales} "
local headerlines headerlines("& \multicolumn{3}{c}{1970} & \multicolumn{3}{c}{1990}" "`midliners'" "`colnames'")
local fn footnote("Fuel consumption represents the sales-weighted average of fuel consumption.")
local marker marker("tab:1")
local size size("small")
texsave using "$table/`file_name'.tex", replace ///
loc(H) frag nonames `headerlines' `marker' `fn' `size' `title'

/* [> Part 3 Estimation and Causal Inference <] */
// Use cleaned data
use "$data/clean_data/all_data.dta", clear

// sort observations to better repliacte results
sort ye ma_code model_code

// configure table setting for regression results
local reg_tbl_setting "b(%5.3f) se(%5.3f) se booktabs width(\textwidth) label nofloat scalars("fe Fixed effects" "se Standard errors")"

// Generate variables needed for discrete choice model
* Generate the number of consumers in market j in year t
gen n_consumers = pop/4 // 4 is average family size; assume each family potentially buys one car
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

// Scalars in regression tables
* configure se
local no_cluster "Robust"
local cluster_model "Clustered on car models"
* configure fe
local no_fe "None"
local add_fe "Car model, market, and year"

// Regress outcome variable on a constant, fuel consumption, and price; no fixed effects
eststo clear

* Heteroskedastic robust standard error with no cluster
eststo: qui reg outcome li eurpr, r
qui estadd local se "`no_cluster'"
qui estadd local fe "`no_fe'"
* cluster on car model
eststo: qui reg outcome li eurpr, vce(cluster model_code)
qui estadd local se "`cluster_model'"
qui estadd local fe "`no_fe'"

// Output regression results
local file_name reg
local title "Structural parameters for fuel consumption and price"
local label \label{tab:2}
qui esttab using "$table/`file_name'.tex", replace title("`title' `label'") ///
`reg_tbl_setting'


// Including car model, market, and year fixed effects
eststo clear
* no cluster
eststo: qui reg outcome li eurpr i.ma_code i.model_code i.ye, r
qui estadd local se "`no_cluster'"
qui estadd local fe "`add_fe'"
* cluster on model
eststo: qui reg outcome li eurpr i.ma_code i.model_code i.ye, vce(cluster model_code)
qui estadd local se "`cluster_model'"
qui estadd local fe "`add_fe'"

// Output regression results
local file_name reg_fe
local title "Structural parameters for fuel consumption and price (added fixed effects)"
local label \label{tab:3}
qui esttab using "$table/`file_name'.tex", replace ///
drop(*.ma_code *.model_code *.ye) title("`title' `label'") ///
`reg_tbl_setting'

// EOF
