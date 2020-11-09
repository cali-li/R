/* 
 * Basic SAS usage
 *
 * Updated: December 10, 2019
 * Author: Wenjing Li
 */

/* 80: ---------------------------------------------------------------------- */

/* question 1: -------------------------------------------------------------- */

/** libnames **/
libname mylib 'C:/Users/lwenjing/Downloads/';
%let path= C:/Users/lwenjing/Downloads/;

/** import data **/
proc import 
  datafile = "&path./df.csv" 
  out = mylib.ps6_q1_result
  replace;
 delimiter = ','; 
 getnames = yes;
run;

/** procs for inspecting data **/
proc contents data=mylib.ps6_q1_result;
run;

/** prepare response of data **/
data ps6_q1_tot_dist;
 set mylib.ps6_q1_result;
 tot_dist_c = log(tot_dist);
run;

/** lmm model for tot_dist **/
proc mixed data=ps6_q1_tot_dist;
 class subject_nr Exemplar Condition;
 model tot_dist_c = Condition / cl;
 random int / subject=subject_nr;
 random int / subject=Exemplar;
run;

/** prepare response of data **/
data ps6_q1_max_abs_dev;
 set mylib.ps6_q1_result;
 max_abs_dev_c = log(max_abs_dev);
run;

/** lmm model for max_abs_dev **/
proc mixed data=ps6_q1_max_abs_dev;
 class subject_nr Exemplar Condition;
 model max_abs_dev_c = Condition / cl;
 random int / subject=subject_nr;
 random int / subject=Exemplar;
run;

/** prepare response of data **/
data ps6_q1_avg_abs_dev;
 set mylib.ps6_q1_result;
 avg_abs_dev_c = log(avg_abs_dev);
run;

/** lmm model for avg_abs_dev **/
proc mixed data=ps6_q1_avg_abs_dev;
 class subject_nr Exemplar Condition;
 model avg_abs_dev_c = Condition / cl;
 random int / subject=subject_nr;
 random int / subject=Exemplar;
run;

/** prepare response of data **/
data ps6_q1_auc;
 set mylib.ps6_q1_result;
 auc = log(abs(AUC));
run;

/** lmm model for auc **/
proc mixed data=ps6_q1_auc;
 class subject_nr Exemplar Condition;
 model auc = Condition / cl;
 random int / subject=subject_nr;
 random int / subject=Exemplar;
run;

/* 80: ---------------------------------------------------------------------- */
