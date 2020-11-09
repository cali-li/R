/* 
 * Basic SAS usage
 *
 * Updated: December 10, 2019
 * Author: Wenjing Li
 */

/* 80: ---------------------------------------------------------------------- */

/* question 2: -------------------------------------------------------------- */
/** libnames **/
libname brr_long 'C:/Users/lwenjing/Downloads/';
%let path= C:/Users/lwenjing/Downloads/;

/** import data **/
proc import 
  datafile = "&path./recs2015_public_v4.csv" 
  out = brr_long.ps6_q2_result
  replace;
 delimiter = ','; 
 getnames = yes;
run;

/** data processing **/
data work.ps6_q2_result;
 set brr_long.ps6_q2_result;
 if TEMPNITE = -2 then delete; 
 if HEATHOME = 1;
 keep BRRWT1-BRRWT96 TEMPNITE NWEIGHT
run;

proc sort data=ps6_q2_result;
by TEMPNITE NWEIGHT;
run;

/** data from wide to long **/
proc transpose data=ps6_q2_result out=brr_long.sas7bdat prefix=BRRWT ;
   by TEMPNITE NWEIGHT;
var BRRWT1-BRRWT96;
run;

data brr_long.sas7bdat;
  set brr_long.sas7bdat (rename=(BRRWT1=BRRWT));
  count=input(substr(_name_, 6), 2.);
  drop _name_ BRRWT2 BRRWT3;
run; 

/** compute estimate and se **/
proc sort data=brr_long.sas7bdat;
by count;
run;

proc summary data=brr_long.sas7bdat;
 by count;
 output out=total
 sum(BRRWT)=total_brr
 sum(NWEIGHT)=total_n;
run;

data totals_data;
 merge total brr_long.sas7bdat;
 by count;
run;

data estimate_data;
 set totals_data;
 means_rep = TEMPNITE*BRRWT/total_brr;
 means_tot = TEMPNITE*NWEIGHT/total_n;
 diff = (means_rep-means_tot)**2;
run;

proc summary data=estimate_data;
 output out=estimate
 sum(diff) = sum_sqr
 sum(means_tot) = sum_tot;
run;

data estimate;
 set estimate;
 lwr = sum_tot - quantile('NORMAL', 0.975) * sqrt(sum_sqr*4/96);
 upr = sum_tot + quantile('NORMAL', 0.975) * sqrt(sum_sqr*4/96);
 estimate = sum_tot;
 drop _TYPE_ _FREQ_ sum_sqr sum_tot;
run;

/* Export to csv: ----------------------------------------------------------- */
proc export data=estimate
  outfile = 'C:/Users/lwenjing/Downloads/ps6_q2_result_1.csv'
  dbms=dlm replace; 
  delimiter  = ",";
run; 

/** by census division **/
data work.ps6_q2_result;
 set brr_long.ps6_q2_result;
 if TEMPNITE = -2 then delete; 
 if TEMPHOME ge 0;
 if TEMPGONE ge 0;
 keep TEMPNITE  DIVISION TEMPHOME TEMPGONE NWEIGHT BRRWT1-BRRWT96;
run;

/** create new format **/
proc format library=brr_long.ps6_q2_formats;
 value division
 	1='NewEng'
	2='MidAtlan'
	3='ENCen'
	4='WNCentral'
	5='SoutAtlan'
	6='ESCen'
	7='WSCentral'
	8='MountNth'
	9='MountSth'
	10='Pacific';

options fmtsearch=( brr_long.ps6_q2_formats work );

/** data processing and computing **/
proc sort data=ps6_q2_result;
by DIVISION;
run;

data ps6_q2_nweight;
 set ps6_q2_result;
 nite_n = TEMPNITE*NWEIGHT;
 gone_n = TEMPGONE*NWEIGHT;
 home_n = TEMPHOME*NWEIGHT;
run;

proc summary data=ps6_q2_nweight;
 by DIVISION;
 output out=total_q2_nweight
  sum(nite_n)=total_nite
  sum(gone_n)=total_gone
  sum(home_n)=total_home
  sum(NWEIGHT)=total_n;
run;

data total_q2_nweight;
 set total_q2_nweight;
 w4tempnite = total_nite/total_n;
 w4tempgone = total_gone/total_n;
 w4temphome = total_home/total_n;
run;

/** for replicate weights **/
data work.ps6_q2_result;
 set brr_long.ps6_q2_result;
 if TEMPNITE = -2 then delete; 
 if TEMPHOME ge 0;
 if TEMPGONE ge 0;
 keep TEMPNITE  DIVISION TEMPHOME TEMPGONE BRRWT1-BRRWT96;
run;

proc sort data=ps6_q2_result;
by DIVISION TEMPNITE TEMPHOME TEMPGONE;
run;

proc transpose data=ps6_q2_result out=ps6_q2_brr prefix=BRRWT ;
   by DIVISION TEMPNITE TEMPHOME TEMPGONE;
var BRRWT1-BRRWT96;
run;

data ps6_q2_brr;
  set ps6_q2_brr (rename=(BRRWT1=BRRWT));
  count=input(substr(_name_, 6), 2.);
  drop _name_ BRRWT2-BRRWT73;
run; 

proc sort data=ps6_q2_brr;
by DIVISION count;
run;

data ps6_q2_brr1;
 set ps6_q2_brr;
 if cmiss(of _all_) then delete;
 nite_brr = TEMPNITE*BRRWT;
 gone_brr = TEMPGONE*BRRWT;
 home_brr = TEMPHOME*BRRWT;
run;

proc summary data=ps6_q2_brr1;
 by DIVISION count;
 output out=total_q2_brr
 sum(nite_brr)=total_nite_b
 sum(gone_brr)=total_gone_b
 sum(home_brr)=total_home_b
 sum(BRRWT)=total_b;
run;

data total_q2_brr;
 set total_q2_brr;
 brr4tempnite = total_nite_b/total_b;
 brr4tempgone = total_gone_b/total_b;
 brr4temphome = total_home_b/total_b;
run;

data totals_q2;
 merge total_q2_brr total_q2_nweight;
 by DIVISION;
run;

proc sort data=totals_q2;
by DIVISION;
run;

data totals_q2;
 set totals_q2;
 diff_nite = (brr4tempnite-w4tempnite)**2;
 diff_gone = (brr4tempgone-w4tempgone)**2;
 diff_home = (brr4temphome-w4temphome)**2;
run;

proc summary data=totals_q2;
 by DIVISION;
 output out=estimate
 sum(diff_nite) = sum_nite
 sum(diff_gone) = sum_gone
 sum(diff_home) = sum_home;
run;

data totals_q2;
 set totals_q2;
 drop count _TYPE_ _FREQ_ total_nite_b total_gone_b 
      total_home_b total_b brr4tempnite brr4tempgone brr4temphome 
      total_nite total_gone total_home total_n diff_nite diff_gone diff_home;
run;

data totals_estimate_q2c;
 merge totals_q2 estimate;
 by DIVISION;
run;

proc sort data=totals_estimate_q2c dupout=totals_estimate_q2c_distinct nodupkey;
 by DIVISION;
run;

data totals_estimate_q2c;
 set totals_estimate_q2c;
 se4nite = sqrt(sum_nite)*4/96;
 se4gone = sqrt(sum_gone)*4/96;
 se4home = sqrt(sum_home)*4/96;
 lwr_nite = w4tempnite - quantile('NORMAL', 0.975) * se4nite;
 lwr_gone = w4tempgone - quantile('NORMAL', 0.975) * se4gone;
 lwr_home = w4temphome - quantile('NORMAL', 0.975) * se4home;
 upr_nite = w4tempnite + quantile('NORMAL', 0.975) * se4nite;
 upr_gone = w4tempgone + quantile('NORMAL', 0.975) * se4gone;
 upr_home = w4temphome + quantile('NORMAL', 0.975) * se4home;
 drop _TYPE_ _FREQ_ sum_nite sum_gone sum_home se4nite se4gone se4home;
run;

/** output **/
proc print data=totals_estimate_q2c noobs;
 format DIVISION division.;
title "Estimation for winter home temperatures at night, 
      during the day with somone home, and during the day with no one home.";
run;

proc export data=totals_estimate_q2c 
outfile='C:/Users/lwenjing/Downloads/ps6_q2_result.csv' 
dbms = dlm replace;
 delimiter=',';
run;

/* 80: ---------------------------------------------------------------------- */
