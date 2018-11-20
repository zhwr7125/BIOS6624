
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

