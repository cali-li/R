* Question 2 ------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*80: ---------------------------------------------------------------------------  
* Set up: ----------------------------------------------------------------------
version 16.0
log using ps4_q2.log, text replace

// load data
import delimited recs2015_public_v4.csv, clear
// generate new variable uatyp with 0 represent "Urban", 1 means "Rural"
gen uatyp= uatyp10=="R"
label define typ_label 0 "Urban" 1 "Rural"
label values uatyp typ_label
preserve
// compute proportion based on nweight
keep doeid uatyp nweight internet division
gen nweight_half= cond(internet==1,nweight,0)
collapse (sum) nweight* , by(division uatyp)
gen nweight_prop= nweight_half/nweight
drop nweight nweight_half
reshape wide nweight_prop, i(division) j(uatyp)
save half_result_recs2015.dta, replace
restore
// compute proportion based on replicate weights
local var="brrwt*"
keep doeid uatyp internet division `var'
reshape long brrwt, i(doeid) j(weight)
gen brrwt_half= cond(internet==1,brrwt,0)
collapse (sum) brrwt* , by(division uatyp weight)
gen brrwt_prop= brrwt_half/brrwt
drop brrwt brrwt_half
reshape wide brrwt_prop, i(division weight) j(uatyp)
// merge two tables and compute se
merge m:1 division using half_result_recs2015.dta
gen dispar_weight= abs(nweight_prop0-nweight_prop1)
gen dispar_brrwt= abs(brrwt_prop0-brrwt_prop1)
gen se_dispar= (dispar_brrwt-dispar_weight)^2
collapse (first) dispar_weight (sum) se_dispar, by(division)

replace se_dispar=sqrt(se_dispar*4/96)
gen lwr_dispar= dispar_weight - se_dispar
gen upr_dispar= dispar_weight + se_dispar
drop se_dispar
// sort data and export result
gsort -dispar_weight
export delimited recs2015_result.csv, replace

log close
*80: --------------------------------------------------------------------------*