/* 
 * Basic SAS usage
 *
 * Updated: December 10, 2019
 * Author: Wenjing Li
 */

/* 80: ---------------------------------------------------------------------- */

/* question 3: -------------------------------------------------------------- */
proc sql;

	create table total as 
		select (TEMPNITE*BRRWT/sum(BRRWT)-TEMPNITE*NWEIGHT/sum(NWEIGHT))**2 as diff,
     TEMPNITE*NWEIGHT/sum(NWEIGHT) as means_tot, count
		from brr_long.sas7bdat
		group by count;

  create table estimate as
    select sum(means_tot)-quantile('NORMAL',0.975)*sqrt(sum(diff)*4/96) as lwr,
          sum(means_tot)+quantile('NORMAL',0.975)*sqrt(sum(diff)*4/96) as upr,
		  sum(means_tot) as estimate
    from total;
	quit;
run;

/* Export to csv: ----------------------------------------------------------- */
proc export data=estimate
  outfile = 'C:/Users/lwenjing/Downloads/ps6_q3_result_p1.csv'
  dbms=dlm replace; 
  delimiter  = ",";
run; 

/** by census division **/
proc sql;

  create table total_q2_nweight as
    select sum(TEMPNITE*NWEIGHT)/sum(NWEIGHT) as w4tempnite,
            sum(TEMPGONE*NWEIGHT)/sum(NWEIGHT) as w4tempgone,
            sum(TEMPHOME*NWEIGHT)/sum(NWEIGHT) as w4temphome,
             DIVISION
    from brr_long.ps6_q2_result
    where TEMPNITE ne -2 and TEMPHOME ge 0 and TEMPGONE ge 0;
	quit;
run;
/** for replicate weights **/
proc sql;

  create table ps6_q2_brr as
    select sum(TEMPNITE*BRRWT) as sum4tempnite,
            sum(TEMPGONE*BRRWT) as sum4tempgone,
            sum(TEMPHOME*BRRWT) as sum4temphome,
            DIVISION, count, sum(BRRWT) as sum_brr
    from ps6_q2_brr
	group by DIVISION, count;
	quit;
run;

proc sql;
	create table total_q2_brr as
    select b.sum4tempnite/b.sum_brr as brr4tempnite,
            b.sum4tempgone/b.sum_brr as brr4tempgone,
            b.sum4temphome/b.sum_brr as brr4temphome,
            b.DIVISION, b.count, n.w4tempnite, n.w4tempgone, n.w4temphome
    from ps6_q2_brr b
    inner join total_q2_nweight n
    on n.DIVISION=b.DIVISION
    order by b.DIVISION;
	quit;
run;
proc sql;
  create table total_q2_brr_1 as 
    select DIVISION as division,
		  (brr4tempnite-w4tempnite)**2 as nite2,
          (brr4tempgone-w4tempgone)**2 as gone2,
          (brr4temphome-w4temphome)**2 as home2,
		  w4tempnite, w4tempgone, w4temphome
    from total_q2_brr;
  quit;
run;
proc sql;
  create table total_q2_brr_2 as 
    select division,
		  sum(nite2) as sum_nite,
          sum(gone2) as sum_gone,
          sum(home2) as sum_home
    from total_q2_brr_1
	group by division;
  quit;
run;
proc sql;
  create table total_q2_brr_3 as 
    select l.division,
		  quantile('NORMAL', 0.975)*sqrt(l.sum_nite)*4/96 as se_nite,
          quantile('NORMAL', 0.975)*sqrt(l.sum_gone)*4/96 as se_gone,
          quantile('NORMAL', 0.975)*sqrt(l.sum_home)*4/96 as se_home,
		  r.w4tempnite, r.w4tempgone, r.w4temphome
    from total_q2_brr_2 l
	inner join total_q2_brr_1 r
	on l.division=r.division
	group by l.division;
  quit;
run;

proc sql;
  create table totals_q2 as 
    select DIVISION as division,
		  w4tempnite - se_nite as lwr_nite,
          w4tempnite + se_nite as upr_nite,
          w4tempgone - se_gone as lwr_gone,
          w4tempgone + se_gone as upr_gone,
          w4temphome - se_home as lwr_home,
          w4temphome + se_home as upr_home,
		  w4tempnite, w4tempgone, w4temphome
    from total_q2_brr_3
	group by division
	having monotonic()=min(monotonic()); 
  quit;
run;

/* Print the result: -------------------------------------------------------- */
proc print data=totals_q2;
  format DIVISION division.;
title "Estimation for winter home temperatures at night, 
      during the day with somone home, and during the day with no one home.";
run;

/* Export to csv: ----------------------------------------------------------- */
proc export data=totals_q2
  outfile = 'C:/Users/lwenjing/Downloads/ps6_q3_result.csv'
  dbms=dlm replace; 
  delimiter  = ",";
run; 

/* 80: ---------------------------------------------------------------------- */
