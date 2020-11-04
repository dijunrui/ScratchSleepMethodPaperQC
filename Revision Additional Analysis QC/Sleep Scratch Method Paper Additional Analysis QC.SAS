OPTIONS nofmterr;

libname mylib 'C:\Users\zhangy6\Pfizer\Di, Junrui - SQUAD Study Programming Analysis Tables Quanticate\Quanticate Derived Datasets and Documentation\Training Set Output Datasets and Documentation';
libname mylib2 'C:\Users\zhangy6\Pfizer\Di, Junrui - SQUAD Study Programming Analysis Tables Quanticate\Quanticate Derived Datasets and Documentation\Training Set Demography Dataset';


/************ Additional Analysis a  ***************/

data dat1_scatt_n;
  set mylib.f_15_2_7_2_8_3;
  ptflag=1;
  if acc=. or vid=. then ptflag=0;
  endpt="Number Events";
run;

data dat1_scatt_t;
  set mylib.f_15_2_7_2_8_4;
  ptflag=1;
  if acc=. or vid=. then ptflag=0;
  endpt="Duration";
run;

data dat1_ba_n;
  set mylib.f_15_2_7_2_7_3;
  ptflag=1;
  if diff=. or mean=. then ptflag=0;
  endpt="Number Events";
run;

data dat1_ba_t;
  set mylib.f_15_2_7_2_7_4;
  ptflag=1;
  if diff=. or mean=. then ptflag=0;
  endpt="Duration";
run;

data dat1_scatt;
  set dat1_scatt_n dat1_scatt_t;
run;

data dat1_ba;
  set dat1_ba_n dat1_ba_t;
run;

proc sort data=dat1_scatt;
  by endpt subject avisitn;
run;

proc sort data=dat1_ba;
  by endpt subject avisitn;
run;

proc freq data=dat1_scatt;
  by endpt;
  table avisitn*ptflag; 
run;

proc freq data=dat1_ba;
  by endpt;
  table avisitn*ptflag;
run;

proc mixed data=dat1_scatt;
by endpt;
class subject avisitn;
model acc = vid avisitn / solution;
repeated avisitn / subject=subject type=un;
ods output tests3=dat1_scatt_tests3 SolutionF=dat1_scatt_fixed covparms=dat1_scatt_cp;
run;

** also output dataset dat1_scatt to R to compute marginal r;

proc mixed data=dat1_ba;
by endpt;
class subject avisitn;
model diff = / solution;
repeated avisitn / subject=subject type=cs;
ods output SolutionF=dat1_ba_fixed covparms=dat1_ba_cp;
run;

proc means data=dat1_ba_cp;
  by endpt;
  var estimate;
  output out=dat1_ba_var mean=meanvar;
run;

data dat1_ba_limit;
  merge dat1_ba_fixed dat1_ba_var;
  by endpt;
  upper=estimate+1.96*sqrt(_FREQ_*meanvar);
  lower=estimate-1.96*sqrt(_FREQ_*meanvar);
run;

proc print data=dat1_ba_limit;
run;


/************ Additional Analysis b  ***************/

proc import datafile="C:\Users\zhangy6\Documents\yaozhang\Preclinical\DigitalMedcine\AtopicDerm\SQUAD\Publication\Methodology July2020\Revision\dat2_scratch0.csv"
        out=dat2_scratch0
        dbms=csv
        replace;
     getnames=yes;
run;

data dat2_sleep_TST;
  set mylib.f_15_2_7_2_3_6;
  tst=sleeppy_var;
  keep subject tst;
run;

data dat2_sleep_WASO;
  set mylib.f_15_2_7_2_3_12;
  waso=sleeppy_var;
  keep subject waso;
run;

proc sort data=dat2_sleep_TST;
  by subject;
run;

proc sort data=dat2_sleep_WASO;
  by subject;
run;

data dat2_sleep;
  merge dat2_sleep_TST dat2_sleep_WASO;
  by subject;
run;

data dat2_scratch1;
  set dat2_scratch0;
  if visit="V02";
  subject=put(ID, 12.);
run;

proc sort data=dat2_scratch1;
  by subject wrist;
run;

data dat2_scratch;
  set dat2_scratch1;
  by subject;
  if First.subject then n=0;
  if First.subject then t=0;
  if First.subject then freq=0;
  if First.subject then chk=0;
  n + Number_of_Scratch;
  t + Duration_of_Scratch;
  freq + 1;
  if Number_of_Scratch=. or Duration_of_Scratch=. then chk+1;
  if Last.subject;
  if freq=2;
  if chk=0;
  logn=log(n+1);
  logt=log(t/60+1);
  keep subject logn logt;
run;

data dat2;
  merge dat2_sleep dat2_scratch;
  by subject;
run;

ods graphics on;
proc corr data=dat2 outp=dat2_cor plots=scatter;
run;
ods graphics off;


/************ Additional Analysis c  ***************/

proc import datafile="W:\PfIRe-SQUAD\scratch_dev\model_development\model_training\trained_models\random_forest\20hz\FINAL_MODEL_RESULTS_2_7_20\per_subject_validation_performance.csv"
        out=dat3_perf
        dbms=csv
        replace;
     getnames=yes;
run;

data dat3_perf(rename=(ID=subject));
  set dat3_perf;
  ID=put(subject, 12.);
  drop subject;
run;

data dat3_demo;
  set mylib2.demo_pro_train;
  keep subject gender ISGA_V0 ISGA_V5;
run;

proc sort data=dat3_perf;
  by subject;
run;

proc sort data=dat3_demo;
  by subject;
run;

data dat3;
  merge dat3_demo dat3_perf;
  by subject;
run;

proc sort data=dat3;
  by gender;
run;

** note R uses a slightly different quantile calculation from SAS;
proc means data=dat3 n p25 p50 p75 qntldef=5 noprint;
  by gender;
  var accuracy	sensitivity	specificity	positive_predictive_value	negative_predictive_value	false_positive_rate	false_negative_rate	false_discovery_rate	f1_score	total_data_contributed_minutes;
  output out=dat3_gsummary n=n1 n2 n3 n4 n5 n6 n7 n8 n9 n10
                          p50=med1 med2 med3 med4 med5 med6 med7 med8 med9 med10
                          p25=lq1 lq2 lq3 lq4 lq5 lq6 lq7 lq8 lq9 lq10 
                          p75=uq1 uq2 uq3 uq4 uq5 uq6 uq7 uq8 uq9 uq10;
run;

proc print data=dat3_gsummary;
run;

proc npar1way data=dat3 noprint;
  class gender;
  var accuracy	sensitivity	specificity	positive_predictive_value	negative_predictive_value	false_positive_rate	false_negative_rate	false_discovery_rate	f1_score	total_data_contributed_minutes;
  output out=dat3_gtest wilcoxon;
run;

proc sort data=dat3;
  by ISGA_V0;
run;

** note R uses a slightly different quantile calculation from SAS;
proc means data=dat3 n p25 p50 p75 qntldef=5 noprint;
  by ISGA_V0;
  var accuracy	sensitivity	specificity	positive_predictive_value	negative_predictive_value	false_positive_rate	false_negative_rate	false_discovery_rate	f1_score	total_data_contributed_minutes;
  output out=dat3_isgasummary n=n1 n2 n3 n4 n5 n6 n7 n8 n9 n10
                          p50=med1 med2 med3 med4 med5 med6 med7 med8 med9 med10
                          p25=lq1 lq2 lq3 lq4 lq5 lq6 lq7 lq8 lq9 lq10 
                          p75=uq1 uq2 uq3 uq4 uq5 uq6 uq7 uq8 uq9 uq10;
run;

proc print data=dat3_isgasummary;
run;

proc npar1way data=dat3 noprint;
  class ISGA_V0;
  var accuracy	sensitivity	specificity	positive_predictive_value	negative_predictive_value	false_positive_rate	false_negative_rate	false_discovery_rate	f1_score	total_data_contributed_minutes;
  output out=dat3_isgatest wilcoxon;
run;

/* also output dataset dat3 to R to check quartiles in R default  */

/************ Additional Analysis d  ***************/

data dat4_demo;
  set mylib2.demo_pro_train;
run;

proc contents data=dat4_demo;
run;

proc summary data=dat4_demo print;
  var ISGAN_V0 PGISN PPNRSN SPSN;
  output out=dat4_bse_summary;
run;

proc freq data=dat4_demo;
  table SPS;
run;


/************ Additional Analysis e  ***************/

data dat5_pro;
  set mylib2.demo_pro_train;
  protso=(entm-sttm)/60;
  if protso<0 then protso=protso+24*60;
  keep subject protso;
run;

data dat5_sleep_TSO;
  set mylib.f_15_2_7_2_3_3;
  psgtso=psg_var;
  sleeppytso=sleeppy_var;
  keep subject psgtso sleeppytso;
run;

proc sort data=dat5_pro;
  by subject;
run;

proc sort data=dat5_sleep_TSO;
  by subject;
run;

data dat5;
  merge dat5_pro dat5_sleep_TSO;
  by subject;
run;

ods graphics on;
proc corr data=dat5 outp=dat5_cor plots=scatter;
run;
ods graphics off;

data dat5_ba;
  set dat5;
  diff_psg=protso-psgtso;
  mn_psg=(protso+psgtso)/2;
  diff_sleeppy=protso-sleeppytso;
  mn_sleeppy=(protso+sleeppytso)/2;
  keep subject diff_psg mn_psg diff_sleeppy mn_sleeppy;
run;

proc sgplot data=dat5_ba;
   scatter y=diff_psg x=mn_psg;
run;

proc sgplot data=dat5_ba;
   scatter y=diff_sleeppy x=mn_sleeppy;
run;

proc summary data=dat5_ba print;
  var diff_psg diff_sleeppy;
  output out=dat5_ba_summary;
run;

proc transpose data=dat5_ba_summary out=dat5_ba_limit;
  var diff_psg diff_sleeppy;
  id _STAT_;
run;

data dat5_ba_limit;
  set dat5_ba_limit;
  upper=mean+1.96*std;
  lower=mean-1.96*std;
run;


/********** End of Code ***********/
