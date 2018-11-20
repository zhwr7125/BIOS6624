
proc format;
	value yr 	34 = "1980JAN-JUN"
				35 = "1980JUL-DEC"
				36 = "1981JAN-JUN"
				37 = "1981JUL-DEC"
				38 = "1982JAN-JUN"
				39 = "1982JUL-DEC"
				;

libname hw "C:\repository\bios6624-zhwr7125\Project3\Data";
data vadata2;
	set hw.vadata2;
run;

****************************************************************;
*Logistic regression											*;
****************************************************************;
data vadata4;
	set vadata2;
	if sixmonth ^=39;
	if asa ^=. and proced ^=. and bmi ^=.;
	keep hospcode sixmonth proced asa asa_combine bmi death30;
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
	create table betacomb as select * from vadata6,outtable;
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

data check;
	set vadata2;
	if hospcode=30 and sixmonth=39;
run;
