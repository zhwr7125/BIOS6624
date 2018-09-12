********************************************************;
*Program Name: DENTAL.sas						        ;
*Purpose: To find the the change of depth pocket 
*		and attachment loss					            ;
*Create by: Wenru Zhou  								;
********************************************************;

/*read in the data*/

proc import DATAfile="\Users\zhouwenru\Repositories\bios6624-zhwr7125\Project0\DataProcessed\Project0_dental_data.csv" out=dental_data
	DBMS=CSV REPLACE;
	GETNAMES=YES;
RUN;

data dental;
	set dental_data;
	d_attach=attach1year-attachbase;
	d_pd=pd1year-pdbase;
	if attach1year=. then attach_ind=1; else attach_ind=0;
	if pd1year=. then pd_ind=1; else pd_ind=0;
	if race in (1,2,4) then race_2=0; else if race=5 then race_2=1; 
	sex_2=sex;
	if sex_2=2 then sex=0;
run;


/*Frequency table*/

proc freq data=dental;
	where attach_ind=0;
	table (sex race_2 smoker)*trtgroup /norow nopercent missprint;
run;

*marginal;
proc freq data=dental;
	where attach_ind=0;
	table (sex race_2 smoker)/ missprint;
run;

proc sort data=dental; by trtgroup; run;
proc means data=dental;
	where attach_ind=0;
	var attach1year attachbase d_attach pd1year pdbase d_pd age sites;
	by trtgroup;
run;

*marginal;
proc means data=dental;
	where attach_ind=0;
	var attach1year attachbase d_attach pd1year pdbase d_pd age sites;
run;


/*check if missing cause bias*/
proc logistic data=dental;
	model trtgroup=attachbase/ link = glogit;
quit;
proc logistic data=dental;
	model trtgroup=attach1year/ link = glogit;
quit;
proc logistic data=dental;
	model trtgroup=d_attach/ link = glogit;
quit;

proc logistic data=dental;
	model trtgroup=age/ link = glogit;
quit;

proc logistic data=dental;
	model trtgroup=sites/ link = glogit;
quit;

proc logistic data=dental;
	model trtgroup=pdbase/ link = glogit;
quit;
proc logistic data=dental;
	model trtgroup=pd1year/ link = glogit;
quit;
proc logistic data=dental;
	model trtgroup=d_pd/ link = glogit;
quit;

proc freq data=dental;
	table (sex race_2 smoker)*trtgroup/chisq fisher;
quit;


/*check if missing at random*/
proc logistic data=dental;
	model attach_ind=attachbase;
quit;

proc logistic data=dental;
	model attach_ind=age;
quit;

proc logistic data=dental;
	model attach_ind=sites;
quit;

proc logistic data=dental;
	model pd_ind=pdbase;
quit;

proc logistic data=dental;
	model pd_ind=age;
quit;

proc logistic data=dental;
	model pd_ind=sites;
quit;



proc freq data=dental;
	table (trtgroup sex race_2 smoker)*attach_ind/chisq fisher;
	table (trtgroup sex race_2 smoker)*pd_ind/chisq fisher;
quit;


/*D_ATTACH*/
/*Univaraite test for numeric var*/
proc reg data=dental;
	model d_attach=attachbase;
run;
*Chech constant variance;
proc autoreg data=dental;
	model d_attach=attachbase/archtest;
run;
proc reg data=dental;
	model d_attach=age;
run;
proc autoreg data=dental;
	model d_attach=sites/archtest;
run;
proc reg data=dental;
	model d_attach=sites;
run;

/*Check assumption and univariate test for categorical var*/
proc glm data=dental;
   class trtgroup;
   model d_attach=trtgroup;
   means trtgroup / hovtest welch;
run;

proc glm data=dental;
	class sex;
	model d_attach=sex;
    means sex / hovtest welch;
run;

proc glm data=dental;
	class race_2;
	model d_attach=race_2;
    means race_2 / hovtest welch;
run;

proc glm data=dental;
	class smoker;
	model d_attach=smoker;
    means smoker / hovtest welch;
run;

quit;


/*linear regression*/
proc glmselect data=dental ;
	class trtgroup sex race_2 smoker;
	model d_attach=attachbase age trtgroup sex race_2 smoker sites/selection=backward(select=SL SLS=0.05);
run;

ods graphics on;
proc glm data=dental PLOTS=(DIAGNOSTICS RESIDUALS);
	class trtgroup(ref='1') sex(ref='0') race_2(ref='0') smoker(ref='0');
	model d_attach=attachbase age trtgroup sex race_2 smoker sites/solution;
	output out=d_attach COOKD=COOKD RSTUDENT=RSTUDENT;
quit;
ods graphics off;
*http://support.sas.com/kb/22/585.html;

/*diagnose*/
proc freq data=d_attach;
	where cookd>0.5;*may be influential, >1 is quite likely to be influencial;
	table cookd;
run;
proc freq data=d_attach;
	where RSTUDENT>3 or RSTUDENT<-3;*outlier;
	table RSTUDENT;
run;


*ID=104 has studentized residual=-0.326086956;
*After check, data is not very bad, but let's remove him and fit again;
ods graphics on;
proc glm data=dental PLOTS=(DIAGNOSTICS RESIDUALS);
	where id^=104;
	class trtgroup(ref='1') sex(ref='0') race_2(ref='0') smoker(ref='0');
	model d_attach=attachbase age trtgroup sex race_2 smoker sites/solution;
quit;
ods graphics off;
proc glmselect data=dental ;
	where id^=104;
	class trtgroup(ref='1') sex(ref='0') race_2(ref='0') smoker(ref='0');
	model d_attach=attachbase age trtgroup sex race_2 smoker sites/selection=backward(select=SL SLS=0.05);
run;



/*d_pd*/
/*Univaraite test*/
proc reg data=dental;
	model d_pd=pdbase;
quit;

proc autoreg data=dental;
	model d_pd=pdbase/archtest;
quit;

proc reg data=dental;
	model d_pd=age;
quit;

proc reg data=dental;
	model d_pd=sites;
quit;

proc autoreg data=dental;
	model d_pd=sites/archtest;
quit;

/*Check assumption and univariate test*/
proc glm data=dental;
   class trtgroup;
   model d_pd=trtgroup;
   means trtgroup / hovtest welch;
quit;

proc glm data=dental;
	class sex;
	model d_pd=sex;
    means sex / hovtest welch;
quit;

proc glm data=dental;
	class race_2;
	model d_pd=race_2;
    means race_2 / hovtest welch;
quit;

proc glm data=dental;
	class smoker;
	model d_pd=smoker;
    means smoker / hovtest welch;
quit;


/*linear regression*/
ods graphics on;
proc glmselect data=dental ;
	class trtgroup(ref='1') sex(ref='0') race_2(ref='0') smoker(ref='0');
	model d_pd=pdbase age trtgroup sex race_2 smoker sites/selection=backward(select=SL SLS=0.05);
quit;

proc glm data=dental PLOTS=(DIAGNOSTICS RESIDUALS);
	class trtgroup(ref='1') sex(ref='0') race_2(ref='0') smoker(ref='0');
	model d_pd=pdbase age trtgroup sex race_2 smoker sites/solution;
	output out=d_pd COOKD=COOKD RSTUDENT=RSTUDENT;
quit;

ods graphics off;
/*diagnose*/
proc freq data=d_pd;
	where cookd>0.5;*may be influential, >1 is quite likely to be influencial;
	table cookd;
quit;

proc freq data=d_pd;
	where RSTUDENT>3 or RSTUDENT<-3;*outlier;
	table RSTUDENT;
quit;
*Not significant;
