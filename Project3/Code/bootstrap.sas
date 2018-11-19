proc import file="C:\repository\bios6624-zhwr7125\Project3\Data\vadata2_SAS.csv" out=vadata;

run;

proc format;
	value yr 	34 = "1980JAN-JUN"
				35 = "1980JUL-DEC"
				36 = "1981JAN-JUN"
				37 = "1981JUL-DEC"
				38 = "1982JAN-JUN"
				39 = "1982JUL-DEC"
				;

*Check categorical data;
proc freq data=vadata;
	table hospcode sixmonth proced asa death30/missing;
run;*we should delete proced=2;
proc freq data=vadata;
	where proced^=2;
	table proced/missing;
run;
*We need to figure out the missing pattern of asa and proced;


*Check numeric data;
proc means data=vadata;
	var weight height bmi albumin;
run;
proc univariate data=vadata;
	histogram weight height bmi albumin;
run;
*No impossible values, but the weight is a mixture of two normal;


*Check the missing pattern of numeric data;
*Create dummy variables;
data vadata2;
	set vadata;
	*Define missing indicator for missing variables;
	array apple{6} asa proced weight height bmi albumin;
	array ident{6} asa_i proced_i weight_i height_i bmi_i albumin_i;

	do i=1 to 6;
		if apple(i)=. then ident(i)=0;
		else ident(i)=1;
	end;
	drop i;

	*Define a weight mix seperator, might have no use;
	if weight<102 then weight_mix=1;
	else if weight>=102 then weight_mix=2;

	*Define the normal range of albumin;
	if albumin=. then albumin_normal=.;
	else if albumin<3.4 then albumin_normal=0;
	else if albumin>=3.4 and albumin<=5.4 then albumin_normal=1;
	else if albumin>=5.4 then albumin_normal=2;

	*Check the bmi;
	bmi_cal=703*weight/(height*height);
	diff=round(abs(bmi_cal-bmi),0.1);
	if diff=0 then diff_error=0;
	else if diff=. then diff_error=.;
	else diff_error=1;
	*weight is bad, so we will use BMI it self.

	*Correct procedure=2;
	if proced=2 then delete;

	*Combine ASA 1 and 2;
	asa_combine=asa;
	if asa in (1,2) then asa_combine=1;
run;



****************************************************************;
*Calculated per hospital per year total line number and total 1*;
****************************************************************;

proc sort data=vadata2; by hospcode; run;
data vadata3;
	set vadata2;
	where sixmonth=39;
	by hospcode;
		ndeath+death30;
		nrow+1;
	if first.hospcode then do;
		ndeath=0;
		nrow=0;
	end;
	if last.hospcode;
	death=(ndeath/nrow);
	death1000=(ndeath/nrow)*1000;
run;
		
proc sgplot data=vadata3;
	vbar hospcode/response=death1000;
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
proc sort data=vadata4; by sixmonth; run;
proc surveyselect data=vadata4 seed=1
     out=BootSSFreq(rename=(Replicate=SampleID))
     method=urs              /* resample with replacement */
     samprate=1              /* each bootstrap sample has N observations */
     /* OUTHITS                 option to suppress the frequency var */
     reps=&NumSamples;       /* generate NumSamples bootstrap resamples */
	 strata sixmonth;
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

proc univariate data=outtable;
	histogram;
run;
