********************************************************************************
* Does medicaid affect live birth weights?
* Difference-in-differences analysis
* Conor Hennessy
* 02/2021
********************************************************************************

local years_with_data 2014 2015 2016 2017 2018 2019
local childhealth_indicators
local indicator_descriptions

*first, get child health birth weight data from GitHub
*data originally from https://www.medicaid.gov/medicaid/quality-of-care/performance-measurement/adult-and-child-health-care-quality-measures/childrens-health-care-quality-measures/index.html#AnnualReporting
foreach year of local years_with_data{
	
	import delimited "https://github.com/cfhenn/healthcare/raw/master/Data/child_healthcare_data/`year'_Child_Health_Care_Quality_Measures.csv", clear
	
	keep if (measureabbreviation == "LBW-CH" & population == "Medicaid only")
			
	keep state staterate 
	
	recast str16 state, force
	
	gen year = `year'

	destring staterate, force replace
			
	save childhealth`year'.dta, replace
}

*merge all years  of birth data together
foreach year of local years_with_data{
	append using childhealth`year'.dta
}
duplicates drop
save medicaid_outcome_merged.dta, replace


*next, merge in data on when expansions took effect in states that expanded medicaid
import delimited "https://github.com/cfhenn/healthcare/raw/master/Data/medicaid_expansion.csv", clear
merge 1:m state using medicaid_outcome_merged.dta, keep(3) nogen
save medicaid_outcome_merged.dta, replace

*next, merge in data for control variables
*race and income have been shown to be correlated with live birth weight
use "https://github.com/cfhenn/healthcare/blob/master/Data/pums_smallfile.dta?raw=true", clear
keep if hinscaid == 2 // only keep people on medicaid
gen pct_white = (race == 1)*perwt
gen pct_black = (race == 2)*perwt
gen pct_asian = (race >= 4 & race >= 6)*perwt
gen pct_othrc = (race == 3 | race >  6)*perwt
replace inctot = inctot*perwt
decode statefip, gen(state)

collapse (sum) pct_white pct_black pct_asian pct_othrc inctot perwt, by(state year)

local control_vars pct_white pct_black pct_asian pct_othrc inctot
foreach cv of local control_vars{
	replace `cv' = `cv'/perwt
}

merge 1:1 state year using medicaid_outcome_merged.dta, keep(3) nogen


*finally, conduct a differnce-in-differences regression
*medicaid expansion considered as the treatment
*regression will be run once with control variables and once without
*control variables likely to be important, as states with medicaid expansions will have higher-income medicaid patients
*and income has been shown to correlate with live birth weight
drop if medicaid_expansion == 2014 | medicaid_expansion == 2019 // drop states treated in the first or last years of data
drop if year == 2015 | year == 2016 //drop the years in which treatment took effect - there are now pre-treatment / post treatment periods for all treated states

gen time  = year > 2016

gen treated = medicaid_expansion_year != .

gen d_i_d = time*treated

regress staterate time treated d_i_d, robust

regress staterate time treated d_i_d `control_vars', robust

*this regression does not show that the medicaid expansion had any affect on 
*live birth weights; however, the number of states in the treatment and control 
*groups are small. Also, the parallel trends assumption may not hold, and the 
*sample size is too small for propensity-score matching, thus these 
*results are do not comprehensively disprove that any such effect exists

rm medicaid_outcome_merged.dta
foreach year of local years_with_data{
	rm childhealth`year'.dta
}

