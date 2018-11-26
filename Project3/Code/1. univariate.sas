
proc format;
	value yr 	34 = "1980JAN-JUN"
				35 = "1980JUL-DEC"
				36 = "1981JAN-JUN"
				37 = "1981JUL-DEC"
				38 = "1982JAN-JUN"
				39 = "1982JUL-DEC"
				;
	value by_f  
				0 = "ALL"
				1 = "old"
				2 = "new"
				;
	value asa_f 1 = "1,2"
				3 = "3"
				4 = "4"
				5 = "5"
				;
	value proced_f 0 = "Valve surgery" 
				 1 = "CABG surgery"
				 ;
	value death_f   0 = "All"
					1 = "Alive" 
				 2 = "Dead"
				 ;

libname hw "C:\repository\bios6624-zhwr7125\Project3\Data";
data vadata2;
	set hw.vadata2;
	if sixmonth <=38;
	pnum=_n_;
	label weight_lb="Weight,lb";
	label height="Height,inch";
	label BMI="BMI,kg/cm^2";
	label albumin="Albumin,g/dL";

	label proced = "procedure";
	label asa_combine = "Patient's condition";
	label death30 = "30 day mortality";
	label sixmonth = "Period";

	id=hospcode;
	deathby=death30+1;
	if asa_combine ^=. and proced ^=. and bmi ^=.;
run;

%contg(var=weight_lb height bmi albumin,indat=vadata2,byvar=deathby,byvarf=death_f.,byvarlist=All Alive Dead);
data table_num;
	set Out_baseout;
run;
%freqg(var=asa_combine proced sixmonth,varformat=asa_f. proced_f. yr.,indat=vadata2,byvar=deathby,byvarf=death_f.,byvarlist=All Alive Dead);
data table_cha;
	set Out_baseout;
run;

data table_result;
	set table_cha
		table_num;
run;
options nodate nonumber orientation=landscape; 
ods noproctitle;
title;
ods rtf file='C:\repository\bios6624-zhwr7125\Project3\Reports\table.doc' bodytitle style=journal3; *you could also use style=monospace;
proc print data=table_result;
run;
ODS LISTING;

ods rtf close;



data vadata2_39;
	set hw.vadata2;
	if sixmonth =39;
	pnum=_n_;
	label weight_lb="Weight,lb";
	label height="Height,inch";
	label BMI="BMI,kg/cm^2";
	label albumin="Albumin,g/dL";

	label proced = "procedure";
	label asa_combine = "Patient's condition";
	label death30 = "30 day mortality";
	label sixmonth = "Period";

	id=hospcode;
	deathby=death30+1;
	if asa_combine ^=. and proced ^=. and bmi ^=.;
run;
%contg(var=weight_lb height bmi albumin,indat=vadata2_39,byvar=deathby,byvarf=death_f.,byvarlist=All Alive Dead);
data table_num;
	set Out_baseout;
run;
%freqg(var=asa_combine proced sixmonth,varformat=asa_f. proced_f. yr.,indat=vadata2_39,byvar=deathby,byvarf=death_f.,byvarlist=All Alive Dead);
data table_cha;
	set Out_baseout;
run;

data table_result;
	set table_cha
		table_num;
run;
options nodate nonumber orientation=landscape; 
ods noproctitle;
title;
ods rtf file='C:\repository\bios6624-zhwr7125\Project3\Reports\table2.doc' bodytitle style=journal3; *you could also use style=monospace;
proc print data=table_result;
run;
ODS LISTING;

ods rtf close;
