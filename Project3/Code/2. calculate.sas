
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
proc freq data=vadata2;
	table sixmonth;
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
	death100=(ndeath/nrow)*100;
	death_se=sqrt(death*(1-death)/nrow);
	high=death100+1.96*death_se*100;
	low=max(death100-1.96*death_se*100,0);
	result100=strip(round(death100,0.01))||"("||strip(round(low,0.01))||","||strip(round(high,0.01))||")";
	label death100="death rate";
	label low = "95% lower CI";
	label high = "95% upper CI";
run;

data count_summary;
	set vadata3;
	keep hospcode death100 high low result100;
run;

data hw.count_summary;
	set count_summary;
run;


ods graphics / reset width=1500px height=600px imagefmt=gif;   
title1 "Death rates per 100 patients and 95% CI during 1982 Jul-Dec, Period 39"; 
title2 "Observations with missing covariates are included, N=4417";
proc sgplot data=vadata3;
   format death100 low high 4.1;
   vbarparm  category=hospcode response=death100 /datalabel limitlower=Low limitupper=high; 
   xaxis label="Hospital" values=(1 to 44 by 1) ;
   yaxis label="Death rates per 100 patients in period39";
run;

