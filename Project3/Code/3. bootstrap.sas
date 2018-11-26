
proc format;
	value yr 	34 = "1980JAN-JUN"
				35 = "1980JUL-DEC"
				36 = "1981JAN-JUN"
				37 = "1981JUL-DEC"
				38 = "1982JAN-JUN"
				39 = "1982JUL-DEC"
				;
	value diff  1 = "Higher"
				2 = "Lower"
				0 = "Not change much"
				. = "N/A"
				;

libname hw "C:\repository\bios6624-zhwr7125\Project3\Data";
data vadata2;
	set hw.vadata2;
	format sixmonth yr.;
run;

****************************************************************;
*Logistic regression											*;
****************************************************************;
data vadata4;
	set vadata2;
	if sixmonth ^=39;
	if asa_combine ^=. and proced ^=. and bmi ^=.;
	keep hospcode sixmonth proced asa_combine bmi death30;
run;
%let NumSamples = 10;       /* number of bootstrap resamples */
proc sort data=vadata4; by hospcode; run;
proc surveyselect data=vadata4 seed=1
     out=BootSSFreq(rename=(Replicate=SampleID))
     method=urs              /* resample with replacement */
     samprate=1              /* each bootstrap sample has N observations */
     /* OUTHITS                 option to suppress the frequency var */
     reps=&NumSamples;       /* generate NumSamples bootstrap resamples */
	 strata hospcode;
run;

%macro boot5050();
%do i = 1 %to &NumSamples.;
proc logistic data=BootSSFreq;
	where SampleID=&i.;
	class death30(ref="0") proced(ref=first) asa_combine(ref=first);
	model death30=proced asa_combine bmi;
	WEIGHT NumberHits;
	ods output ParameterEstimates=haha&i.;
run;

data hahah&i.;
	set haha&i.;
	name=strip(Variable)||ClassVal0;
	keep name estimate;
run;
proc sort data=hahah&i.; by name; run;
proc transpose data=hahah&i. out=haha_wide&i.;
	var estimate;
run;

proc datasets;
APPEND BASE=OutTable
DATA=haha_wide&i.
FORCE; 
delete haha: ;
run;
%end;
%mend;
%boot5050();

data outtable;
	set outtable;
	rename col1=coef_intercept;
	rename col2=coef_asa_combine3;
	rename col3=coef_asa_combine4;
	rename col4=coef_asa_combine5;
	rename col5=coef_bmi;
	rename col6=coef_proced1;
	B=_n_;
	drop _NAME_;
run;



*plug in 39;
data vadata5;
	set vadata2;
	if sixmonth=39;
	keep hospcode sixmonth proced asa_combine bmi death30;
run;
proc transreg data=vadata5 design;
   model class(asa_combine / zero=first);
   id hospcode sixmonth proced death30 bmi;
   output out=vadata6(drop=_: Int:);
run;

proc sql;
	create table betacomb as select * from vadata6,hw.outtable;
quit;

data betacomb2;
	set betacomb;
	logit=coef_intercept+coef_asa_combine3*asa_combine3+coef_asa_combine4*asa_combine4+coef_asa_combine5*asa_combine5+coef_bmi*bmi+coef_proced1*proced;
	p=exp(logit)/(1+exp(logit));
run;

proc sort data=betacomb2;
	 by B hospcode;
run;

data pred_sum;
	set betacomb2;
    	by B hospcode;
		pdeath+p;
		nrow+1;
	if first.hospcode then do;
		pdeath=p;
		nrow=1;
	end;
	if last.hospcode;
	death=(pdeath/nrow);
	death100=death*100;
	keep B hospcode death100;
run;

proc transpose data=pred_sum out=pred_wide;
	by B;
	var death100;
run;

%macro genedata();
data pred_wide2;
	set pred_wide;
    %do i=1 %to 44;
	rename col&i. =%sysfunc(cat(hospcode,%eval(&i)));
    %end;
run;
%mend;
%genedata;

/*
data hw.Pred_wide2;
	set Pred_wide2;
run;
*/

proc means data=pred_wide2;
	var hospcode1-hospcode44;
	output out=mean(where=(_stat_="MEAN") drop= _type_ _freq_);
run;
proc transpose data=mean out=mean_long;
run;

proc univariate data = pred_wide2 noprint;
	var hospcode1-hospcode44;
	output out=quantile pctlpts= 2.5 97.5 
	pctlpre= hospcode1-hospcode44;
run;

proc transpose data=quantile out=quan_long;
run;

data quan_long1 quan_long2;
	set quan_long;
	id=_n_;
	drop _label_;
	if id<=44 then output quan_long1;
	if id>44 then output quan_long2;
run;

data expect_summary;
	merge mean_long(rename=(COL1=death100_expect))
			quan_long1(drop=id _NAME_ rename=(COL1=low_expect)) 
			quan_long2(drop=id _NAME_ rename=(COL1=high_expect)) ;
	result100_expect=strip(strip(round(death100_expect,0.01))||"("||strip(round(low_expect,0.01))||","||strip(round(high_expect,0.01))||")");
	cat=compress(_name_,"hospcode");
	hospcode=input(cat,best8.);
	keep _NAME_ death100_expect low_expect high_expect result100_expect hospcode;
run;

data count_summary;
	set hw.count_summary;
run;

proc sort data=expect_summary;  
	by hospcode;
run;
proc sort data=count_summary;
	by hospcode;
run;
data final_summary;
	merge expect_summary count_summary;
	by hospcode;
run;
proc transpose data=final_summary out=final_summary2(rename=col1=mean) name=drug;
   var death100_expect death100;
   by _name_ notsorted;
run;
proc transpose data=final_summary out=final_summary2a(rename=col1=low) name=drug;
   var low_expect low;
   by _name_ notsorted;
run;
proc transpose data=final_summary out=final_summary2b(rename=col1=high) name=drug;
   var high_expect high;
   by _name_ notsorted;
run;

data final_summary3;
	merge final_summary2 final_summary2a final_summary2b;
	cat=compress(_name_,"hospcode");
	hospcode=input(cat,best8.);
	drop cat;
	if drug="high_expect" then group="Expectation";
	else if drug="high" then group="Observation";
run;


ods html;
ods html file='scatterplot.html' path='.' style=styles.statistical;                                                                     
ods graphics / reset width=1200px height=600px imagefmt=gif;   
title1 "Death rates per 100 patients and 95% CI during 1982 Jul-Dec, Period 39"; 
title2 "Observtion death rate: N=4417; Expectation death rate: N=4011";

proc sgplot data=Final_summary3;
   scatter x=hospcode y=mean /group=group yerrorlower=low                                                                                            
                           yerrorupper=high                                                                                          
                           markerattrs=(symbol=CircleFilled size=4); 
   xaxis label="Hospital" values=(1 to 44 by 1) ;
   yaxis label="Death rates per 100 patients in period39" values=(1 to 25 by 1) ;
   keylegend / title="Classification";
run; 
	
data final_summary_print;
	set final_summary;
	if high=. or low=. or high_expect=. or low_expect=. then different=.;
	else if low>high_expect then different=1;
	else if low_expect>high then different=2;
	else different=0;

	if result100_expect=".(.,.)" then result100_expect="N/A";
	if result100 = ".(.,.)" then result1000 = ".(.,.)";
	label different = "Reality compared to expectation, period 39";
	format different diff.;
run;

proc freq data=final_summary_print;
	table different;
run;

options nodate nonumber orientation=landscape; 
ods noproctitle;
title;
ods rtf file='C:\repository\bios6624-zhwr7125\Project3\Reports\result_count.doc' bodytitle style=journal3; *you could also use style=monospace;
proc report data=final_summary_print;
	column _NAME_ result100_expect result100 different;
    DEFINE _NAME_ / 'Hospital';
    DEFINE result100_expect / 'Expected death rate(95%CI)' center;
    DEFINE result100 / 'Actual death rate(95%CI)' center;
    DEFINE different / 'Reality compared to expectation, period 39' center;
	format different diff.;
run;
ODS LISTING;

ods rtf close;


data check;
	set vadata2;
	if hospcode=30 and sixmonth=39;
run;

