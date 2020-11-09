*------------------------------------------------------------------------------*

* Basic useage of STATA
* Data: the output from dplyr question (question 1)
*       recs2015_public_v4.csv (question 2)
*       NHANES(2015-2016): DEMO_D.XPT, DR1TOT_D.XPT, DR2TOT_D.XPT (question 3)
*
* Author: Cali Li
* Updated: Nov. 18, 2019
*
*------------------------------------------------------------------------------*
*80: ---------------------------------------------------------------------------  
* Set up: ----------------------------------------------------------------------
version 16.0
log using ps4_q1.log, text replace

* Question 1 ------------------------------------------------------------------*
// data prep
import delimited df.csv , clear
describe
encode condition, generate(newcondition)

// model for tot_dist
gen new_tot_dist= log(tot_dist)
mixed new_tot_dist newcondition || _all: R.subject_nr || _all: R.exemplar
matrix table1=r(table)

// model for max_abs_dev
gen new_abs_dev= log(max_abs_dev)
mixed new_abs_dev newcondition || _all: R.subject_nr || _all: R.exemplar
matrix table2=r(table)

// model for avg_abs_dev
gen new_avg_dev= log(avg_abs_dev)
mixed new_avg_dev newcondition || _all: R.subject_nr || _all: R.exemplar
matrix table3=r(table)

// model for auc
gen new_auc= log(auc)
mixed new_auc newcondition || _all: R.subject_nr || _all: R.exemplar
matrix table4=r(table)

// select data
mata
table1=st_matrix("table1")
table1=table1[(1,5,6),1]
st_matrix("tot_dist",table1)
table2=st_matrix("table2")
table2=table2[(1,5,6),1]
st_matrix("max_abs_dev",table2)
table3=st_matrix("table3")
table3=table3[(1,5,6),1]
st_matrix("avg_abs_dev",table3)
table4=st_matrix("table4")
table4=table4[(1,5,6),1]
st_matrix("auc",table4)
end

// make table
putexcel set ps4_q1.xls, replace
// set column name
putexcel A1=" "
putexcel A2="estimation"
putexcel A3="lwr"
putexcel A4="upr"
// set row name
putexcel B1="tot_dist"
putexcel C1="max_abs_dev"
putexcel D1="avg_abs_dev"
putexcel E1="auc"
// put data in
putexcel B2=matrix(tot_dist)
putexcel C2=matrix(max_abs_dev)
putexcel D2=matrix(avg_abs_dev)
putexcel E2=matrix(auc)

// Note that condition has the largest (relative) effect for avg_abs_dev.

log close
*80: --------------------------------------------------------------------------*