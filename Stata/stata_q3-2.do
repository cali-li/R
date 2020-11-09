* Question 3 ------------------------------------------------------------------*
* Question 3.1 ----------------------------------------------------------------*
// Table "DEMO_D.XPT"
// ridageyr seqn riagendr indfmpir 
// ridexmon(whether the respondents exam was done in “winter” 1=YES)

// Table "DR1TOT_D.XPT"
// seqn DRDINT(1=ONE DAY;2=TWO DAYS) dr1day
// dr1_320z                Total plain water drank yesterday (gm)
// dr1_330z                Total tap water drank yesterday (gm)
// dr1bwatz                Total bottled water drank yesterday (gm)

// Table "DR2TOT_D.XPT"
// seqn DRDINT(1=ONE DAY;2=TWO DAYS) dr2day
// dr2_320z                Total plain water drank yesterday (gm)
// dr2_330z                Total tap water drank yesterday (gm)
// dr2bwatz                Total bottled water drank yesterday (gm)
*------------------------------------------------------------------------------*
*80: ---------------------------------------------------------------------------  
* Set up: ----------------------------------------------------------------------
version 16.0
log using ps4_q3.log, text replace

// "DR1TOT_D.XPT"
fdause "DR1TOT_D.XPT", clear
gen weekday1= drdint==1
label define typ_label 0 "F" 1 "M"
label values weekday1 typ_label
rename dr1day day1
gen water1=dr1_320z+dr1_330z+dr1bwatz
gen any_water1= water1>0
keep seqn day1 weekday1 any_water1
save day1_table.dta, replace

// "DR2TOT_D.XPT"
fdause "DR2TOT_D.XPT", clear
gen weekday2= drdint==1
label define typ_label2 0 "F" 1 "M"
label values weekday2 typ_label2
rename dr2day day2
gen water2=dr2_320z+dr2_330z+dr2bwatz
gen any_water2= water2>0
keep seqn day2 weekday2 any_water2
save day2_table.dta, replace
fdause "DEMO_D.XPT", clear

// 
gen winter= ridexmon==1
label define typ_label 0 "N" 1 "Y"
label values winter typ_label
keep ridageyr seqn riagendr indfmpir winter
merge 1:1 seqn using day1_table.dta
keep if _merge==3
drop _merge
merge 1:1 seqn using day2_table.dta
drop _merge
reshape long day weekday any_water, i(ridageyr seqn riagendr indfmpir winter) j(survey_day)
save water_table.dta, replace
preserve

* Question 3.2 ----------------------------------------------------------------*
// centering the continuous variables, age(ridageyr) and pir(indfmpir)
gen missing=0
foreach l of varlist *{
	replace missing=1 if `l'==.
	}
*gen byte missing= missing(ridageyr, seqn, riagendr, indfmpir, winter, survey_day, day,any_water, weekday)
keep if missing==0
summarize ridageyr
gen centered_age = ridageyr  - r(mean)
replace centered_age = centered_age/10
label variable centered_age "Centered age by decade"
summarize indfmpir
gen centered_pir = indfmpir  - r(mean)
label variable centered_pir "Centered PIR"
drop missing ridageyr indfmpir
save mod_water_table.dta, replace

* Question 3.3 ----------------------------------------------------------------*
// fit model
logistic any_water i.weekday i.winter c.centered_age##c.centered_age i.riagendr centered_pir
matrix return_table1=r(table)

// compute marginal effects
margins, dydx(*) post
matrix return_table2=r(table)

// select data
mata
return_table1=st_matrix("return_table1")
return_table1=return_table1[(1,5,6),(2,4,5,6,8,9)]
st_matrix("ratio",return_table1)
return_table2=st_matrix("return_table2")
return_table2=return_table2[(1,5,6),(2,4,5,7,8)]
st_matrix("margin",return_table2)
end

// make table
putexcel set ps4_q3c.xls, replace

// set column name
putexcel A2="ratio"
putexcel A3="lwr"
putexcel A4="upr"
putexcel A5="margin"
putexcel A6="lwr_margin"
putexcel A7="upr_margin"
// set row name
putexcel B1="weekday"
putexcel C1="winter"
putexcel D1="centered_age"
putexcel E1="centerd_age^2"
putexcel F1="gender"
putexcel G1="PIR"
// put data in
putexcel B2=matrix(ratio)
putexcel B5=matrix(margin)

* Question 3.4 ----------------------------------------------------------------*
// fit model
meglm any_water i.weekday i.winter c.centered_age##c.centered_age i.riagendr centered_pir||seqn:, family(bernoulli) link(logit) or
matrix table1=r(table)

// compute marginal effects
margins, dydx(*) post
matrix table2=r(table)

// select data
mata
table1=st_matrix("table1")
table1=table1[(1,5,6),(2,4,5,6,8,9)]
st_matrix("ratio",table1)
table2=st_matrix("table2")
table2=table2[(1,5,6),(2,4,5,7,8)]
st_matrix("margin",table2)
end

// make table
putexcel set ps4_q3d.xls, replace

// set column name
putexcel A1=" "
putexcel A2="ratio"
putexcel A3="lwr"
putexcel A4="upr"
putexcel A5="margin"
putexcel A6="lwr_margin"
putexcel A7="upr_margin"

// set row name
putexcel B1="weekday"
putexcel C1="winter"
putexcel D1="centered_age"
putexcel E1="centerd_age^2"
putexcel F1="gender"
putexcel G1="PIR"

// put data in
putexcel B2=matrix(ratio)
putexcel B5=matrix(margin)

// Note that two methods produce similar results and the result from logistic 
// model one is much more precise and can be put into good use.

log close
*80: --------------------------------------------------------------------------*