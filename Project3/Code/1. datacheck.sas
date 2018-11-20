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

	*Define the normal range of albumin;
	if albumin=. then albumin_normal=.;
	else if albumin<3.4 then albumin_normal=0;
	else if albumin>=3.4 and albumin<=5.4 then albumin_normal=1;
	else if albumin>=5.4 then albumin_normal=2;

	*Check the bmi;
	weight_lb=weight;

	if sixmonth=39 and hospcode <17 then do;
		weight_lb=2.205*weight;
	end;

	bmi_cal=703*weight_lb/(height*height);
	diff=round(abs(bmi_cal-bmi),1);

	if diff=0 then diff_error=0;
	else if diff=. then diff_error=.;
	else diff_error=1;


	*Correct procedure=2;
	if proced=2 then delete;

	*Combine ASA 1 and 2;
	asa_combine=asa;
	if asa in (1,2) then asa_combine=1;
	drop bmi;
run;

data vadata2;
	set vadata2;
	rename bmi_cal=bmi;
run;

*Check miss-calculatd bmi;
proc freq data=vadata2;
	table diff_error;
run;
proc print data=vadata2;
	where diff_error=1;
run;
proc freq data=vadata2;
	where sixmonth=39;
	table diff_error*hospcode;
run;
*more detailed check on 18, 20 and 23 becaues they miss a few;
proc freq data=vadata2;
	where hospcode in (18,20,23) and sixmonth=39;
	table diff_error*hospcode/norow nopercent;
run;
proc print data=vadata2;
	where hospcode in (18,20,23) and sixmonth=39 and diff_error=1;
run;



proc 
*Check missing of asa using categorical data;
proc freq data=vadata2;
	table (asa_i)*(hospcode sixmonth proced proced_i death30 height_i bmi_i albumin_i)/chisq;
run;
*It seems that proced_i and hospcode death30, bmi_i are significant;
proc logistic data=vadata2;
	class asa_i hospcode;
	model asa_i=hospcode;
run;
proc logistic data=vadata2;
	class asa_i proced_i;
	model asa_i=proced_i;
run;
proc logistic data=vadata2;
	class asa_i death30;
	model asa_i=death30;
run;
proc logistic data=vadata2;
	class asa_i bmi_i;
	model asa_i=bmi_i;
run;

*Check missing of asa using numerical data;
proc ttest data=vadata2;
	class asa_i;
	var bmi;
run;
*Significant;
proc ttest data=vadata2;
	class asa_i;
	var albumin;
run;
*Significant;







*Check missing of proced using categorical data;
proc freq data=vadata2;
	table (proced_i)*(hospcode sixmonth asa death30 height_i bmi_i albumin_i)/chisq;
run;
*check proced_i by death30, proced_i by height_i, proced_i by bmi_i;
proc logistic data=vadata2;
	class proced_i death30;
	model proced_i=death30;
run;
proc logistic data=vadata2;
	class proced_i bmi_i;
	model proced_i=bmi_i;
run;
*Check missing of proced using numerical data;
proc ttest data=vadata2;
	class proced_i;
	var bmi;
run;
*not Significant;
proc ttest data=vadata2;
	class proced_i;
	var albumin;
run;
*not Significant;




*Test numeric: bmi and albumin;

*bmi, check categorical data;
proc freq data=vadata2;
	table (bmi_i)*(hospcode sixmonth asa proced proced_i death30 albumin_i)/chisq;
run;
proc logistic data=vadata2;
	class bmi_i hospcode;
	model bmi_i=hospcode;
run;
proc logistic data=vadata2;
	class bmi_i sixmonth;
	model bmi_i=sixmonth;
run;
proc logistic data=vadata2;
	class bmi_i proced_i;
	model bmi_i=proced_i;
run;
proc logistic data=vadata2;
	class bmi_i death30;
	model bmi_i=death30;
run;

/*
bmi_i by hospcode
bmi_i by sixmonth
bmi_i by proced_i
bmi_i by death30

*/
*bmi, Check missing of bmi using numerical data;
proc ttest data=vadata2;
	class bmi_i;
	var albumin;
run;
*Not significant


*albumin, check categorical data;
proc freq data=vadata2;
	table (albumin_i)*(hospcode sixmonth asa proced proced_i death30 )/chisq;
run;
*No one is significant;
*albumin, Check missing of albumin using numerical data;
proc ttest data=vadata2;
	class albumin_i;
	var bmi;
run;
*Not significant;

libname hw "C:\repository\bios6624-zhwr7125\Project3\Data";
data hw.vadata2;
	set vadata2;
run;
