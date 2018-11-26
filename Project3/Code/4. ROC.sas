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
data vadata_roc;
	set hw.vadata2;
	if sixmonth ^=39 and albumin^=.;
	format sixmonth yr.;
run;

****************************************************************;
*Logistic regression											*;
****************************************************************;
proc logistic data=vadata_roc plots(only)=(roc(id=prob));
	class death30(ref="0") proced(ref=first) asa_combine(ref=first);
	model death30=proced asa_combine bmi;
   roc 'Procedure' proced;
   roc "Patient's condition" asa_combine;
   roc 'BMI' bmi;
run;

proc logistic data=vadata_roc plots(only)=(roc(id=prob));
class death30(ref="0") proced(ref=first) asa_combine(ref=first);
	model death30=proced asa_combine bmi albumin;
   roc 'Procedure' proced;
   roc "Patient's condition" asa_combine;
   roc 'BMI' bmi;
   roc 'Albumin' albumin;
run;
