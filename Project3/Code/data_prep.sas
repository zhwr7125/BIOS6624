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
proc freq data=vadata;
	table hospcode/missing out=hops_count;
run;
proc freq data=vadata;
	table hospcode*sixmonth/missing out=hops_count;
run;

proc freq data=vadata;
	table hospcode sixmonth proced asa death30/missing;
run;*we should delete proced=2;
*We need to figure out the missing pattern of asa and proced;

proc means data=vadata;
	var weight height bmi albumin;
run;
proc univariate data=vadata;
	histogram weight height bmi albumin;
run;
*No impossible values, but the weight is a mixture of two normal;

*Create dummy variables;
data vadata2;
	set vadata;
	array apple{6} asa proced weight height bmi albumin;
	array ident{6} asa_i proced_i weight_i height_i bmi_i albumin_i;

	do i=1 to 6;
		if apple(i)=. then ident(i)=0;
		else ident(i)=1;
	end;
	drop i;

	if weight<102 then weight_mix=1;
	else if weight>=102 then weight_mix=2;

	if albumin=. then albumin_normal=.;
	else if albumin<3.4 then albumin_normal=0;
	else if albumin>=3.4 and albumin<=5.4 then albumin_normal=1;
	else if albumin>=5.4 then albumin_normal=2;
run;

*See weight height bmi;
proc print ddata=vadata2;
	where weight=. and (height^=. or bmi^=.);
	var weight height bmi;
run;

*We can use weight to represent height and bmi;
proc freq data=vadata2;
	table asa_i proced_i weight_i  albumin_i;
run;

*test for albumin;
*categorical variables;
proc freq data=vadata2;
	table albumin_i*(hospcode proced proced_i asa asa_i weight_mix)/norow nopercent chisq;
run;



*numeric variables;
proc npar1way wilcoxon correct=no data=vadata2;
	class albumin_i;
	var weight;
    exact wilcoxon;
run;

proc ttest data=vadata2;
	class albumin_i;
	var height;
run;
*good;

proc ttest data=vadata2;
	class albumin_i;
	var bmi;
run;
*good;


**********************************************;
*Calculated per hospital per year total line number and total 1*;
proc sort data=vadata2; by hospcode sixmonth death30; run;
data vadata3;
	set vadata2;
	by hospcode sixmonth death30;
		ndeath+death30;
		nrow+1;
	if first.sixmonth then do;
		ndeath=0;
		nrow=0;
	end;
	if last.sixmonth;
	death=(ndeath/nrow)*1000;
run;
		


proc sgplot data=vadata3;
	series x=sixmonth y=death/group=hospcode;
	yaxis values=(0 to 200 by 10) label="Total death per 1,000 heart surgeries";
	xaxis label="Every six month period";
	format sixmonth yr.;
run;

proc sort data=vadata3 out=id nodupkey; by hospcode; run;
proc surveyselect data=id
      method=srs n=10
      seed=1953 out=SampleStrata;
run;
data sample_vadata;
	merge SampleStrata (keep=hospcode in=a)
			vadata3;
	by hospcode ;
	if a=1;
run;
proc sgplot data=sample_vadata;
	series x=sixmonth y=death/group=hospcode;
	yaxis values=(0 to 200 by 10) label="Total death per 1,000 heart surgeries";
	xaxis label="Every six month period";
	format sixmonth yr.;
run;
