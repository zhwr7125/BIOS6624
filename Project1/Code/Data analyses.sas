/*proc import datafile  =  'C:\repository\bios6624-zhwr7125\Project1\Data\hivclean.CSV' 
 out  =  cleandata
 dbms  =  CSV
 replace
 ;
run;
*/
proc import datafile  =  'C:\repository\bios6624-zhwr7125\Project1\Data\hiv_6624_final_SAS.CSV' 
 out  =  cleandata_SAS
 dbms  =  CSV
 replace
 ;
run;
DATA cleansas0 cleansas2;
	set cleandata_SAS;

	RACE_ori=RACE;
	if race_ori in (1,2) then race=1;*white;
	else if race_ori >=0 then race=0;*non-white;

	SMOKE_ORI=SMOKE;
	if smoke_ori in (1,2) then smoke=0;
	else if smoke=3 then smoke=1;

	ADH_ORI=ADH;
	if adh_ori in (1,2) then ADH=1;
	else if adh in (3,4) then adh=0;*no adh;

	EDUCBAS_ori=EDUCBAS;
	if EDUCBAS_ori in (1,2,3,4) then EDUCBAS=1;
	else if EDUCBAS_ori in (5) then EDUCBAS=2;
	else if EDUCBAS_ori in (6,7) then EDUCBAS=3;

	if BMI>500 or BMI=999 or BMI=-1 then bmi=.;

	group=hard_drugs;

	keep newid AGG_MENT AGG_PHYS LEU3N VLOAD ART years group age BMI ADH RACE SMOKE EDUCBAS;

	if years=0 then output cleansas0;
	else if years=2 then output cleansas2;
run;
/*
proc freq data=cleansas;
	table race_ori*race smoke_ori*smoke adh_ori*adh educbas_ori*educbas;
quit;
*/

data cleanwide;
	merge cleansas0(rename=(agg_ment=agg_ment_0 AGG_PHYS=AGG_PHYS_0 LEU3N=LEU3N_0 VLOAD=VLOAD_0 
							ART=ART_0 years=years_0 age=age_0 bmi=BMI_0 adh=ADH_0 race=RACE_0
							smoke=SMOKE_0 educbas=EDUCBAS_0)in=a)
			cleansas2(rename=(agg_ment=agg_ment_2 AGG_PHYS=AGG_PHYS_2 LEU3N=LEU3N_2 VLOAD=VLOAD_2 
							ART=ART_2 years=years_2 age=age_2 bmi=BMI_2 adh=ADH_2 race=RACE_2
							smoke=SMOKE_2 educbas=EDUCBAS_2)in=b drop=group);
	by newid;
	if a and b;
	if age_0^=. and BMI_0^=.  and RACE_0^=. and SMOKE_0^=. and EDUCBAS_0^=. ;
run;

proc freq data=cleanwide;
	table group;
run;

proc means data=cleanwide;
	var age_0 bmi_0;
run;
data cleandata2;
	set cleanwide;
	l_LEU3N_0=log10(LEU3N_0);
	l_LEU3N_2=log10(LEU3N_2);

	l_VLOAD_0=log10(VLOAD_0);
	l_VLOAD_2=log10(VLOAD_2);

	diff_LEU3N=l_LEU3N_2-l_LEU3N_0;
	diff_VLOAD=l_VLOAD_2-l_VLOAD_0;
	diff_MENT=AGG_MENT_2-AGG_MENT_0;
	diff_PHYS=AGG_PHYS_2-AGG_PHYS_0;

run;
proc corr data=cleandata2;
	var age_old;
	with bmi_old;
run;
proc corr data=cleandata2;
	var age_0;
	with bmi_0;
run;

%macro check_bad(dat);
proc freq data=&dat;
	where cookd>0.5;*may be influential, >1 is quite likely to be influencial;
	table cookd;
quit;
proc freq data=&dat;
	where RSTUDENT>3 or RSTUDENT<-3;*outlier;
	table RSTUDENT/missing;
quit;
%mend;
proc glm data=cleandata2  PLOTS=(DIAGNOSTICS RESIDUALS);
	class RACE_0(ref='0') EDUCBAS_0(ref='1') SMOKE_0(ref='0') group(ref='0') ADH_2(ref='0');
	model diff_LEU3N=l_LEU3N_0 age_0 BMI_0 RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2/solution; 
	output out=leu COOKD=COOKD RSTUDENT=RSTUDENT;
quit;
%check_bad(leu)
data cleandata_leu;
	set leu(where=(RSTUDENT<=3 and RSTUDENT>=-3));
run;
proc glm data=cleandata_leu  PLOTS=(DIAGNOSTICS RESIDUALS);
	class RACE_0(ref='0') EDUCBAS_0(ref='1') SMOKE_0(ref='0') group(ref='0') ADH_2(ref='0');
	model diff_LEU3N=l_LEU3N_0 age_0 BMI_0 RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2/solution; 
quit;



proc glm data=cleandata2  PLOTS=(DIAGNOSTICS RESIDUALS);
	class RACE_0(ref='0') EDUCBAS_0(ref='1') SMOKE_0(ref='0') group(ref='0') ADH_2(ref='0');
	model diff_VLOAD=l_VLOAD_0 age_0 BMI_0 RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2/solution; 
	output out=vlo COOKD=COOKD RSTUDENT=RSTUDENT;
quit;
%check_bad(vlo)
data cleandata_vlo;
	set vlo(where=(RSTUDENT<=3 and RSTUDENT>=-3));
run;
proc glm data=cleandata_vlo  PLOTS=(DIAGNOSTICS RESIDUALS);
	class RACE_0(ref='0') EDUCBAS_0(ref='1') SMOKE_0(ref='0') group(ref='0') ADH_2(ref='0');
	model diff_VLOAD=l_VLOAD_0 age_0 BMI_0 RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2/solution; 
quit;




proc glm data=cleandata2  PLOTS=(DIAGNOSTICS RESIDUALS);
	class RACE_0(ref='0') EDUCBAS_0(ref='1') SMOKE_0(ref='0') group(ref='0') ADH_2(ref='0');
	model diff_MENT=AGG_MENT_0 age_0 BMI_0 RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2/solution; 
	output out=men COOKD=COOKD RSTUDENT=RSTUDENT;
quit;
%check_bad(men)
data cleandata_men;
	set men(where=(RSTUDENT<=3 and RSTUDENT>=-3));
run;
proc glm data=cleandata_men  PLOTS=(DIAGNOSTICS RESIDUALS);
	class RACE_0(ref='0') EDUCBAS_0(ref='1') SMOKE_0(ref='0') group(ref='0') ADH_2(ref='0');
	model diff_MENT=AGG_MENT_0 age_0 BMI_0 RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2/solution; 
quit;



proc glm data=cleandata2  PLOTS=(DIAGNOSTICS RESIDUALS);
	class RACE_0(ref='0') EDUCBAS_0(ref='1') SMOKE_0(ref='0') group(ref='0') ADH_2(ref='0');
	model diff_PHYS=AGG_PHYS_0 age_0 BMI_0 RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2/solution; 
	output out=phy COOKD=COOKD RSTUDENT=RSTUDENT;
quit;
%check_bad(phy)
data cleandata_phy;
	set phy(where=(RSTUDENT<=3 and RSTUDENT>=-3));
run;
proc glm data=cleandata_phy  PLOTS=(DIAGNOSTICS RESIDUALS);
	class RACE_0(ref='0') EDUCBAS_0(ref='1') SMOKE_0(ref='0') group(ref='0') ADH_2(ref='0');
	model diff_PHYS=AGG_PHYS_0 age_0 BMI_0 RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2/solution; 
quit;


proc means data=cleandata2;
	var diff_LEU3N diff_VLOAD diff_MENT diff_PHYS;
quit;






**** ************************************************************;
*		MCMC												   *;
****************************************************************;
proc transreg data=cleandata2 design;
   model class(RACE_0 EDUCBAS_0 SMOKE_0 group ADH_2 / zero=first);
   id diff_LEU3N l_LEU3N_0 diff_VLOAD l_VLOAD_0 diff_MENT AGG_MENT_0 diff_PHYS AGG_PHYS_0 age_0 BMI_0;
   output out=input_mcmc(drop=_: Int:);
run;

/*CD4 Cell*/
proc mcmc data=input_mcmc nbi=10000 nmc=50000 outpost=callout plots=all DIC;
  parms beta0-beta9 0 sigma2 1; 
  prior beta0-beta9 ~ normal(mean = 0, var = 100000);
  prior sigma2 ~ igamma(shape = 2.001,scale = 1.001);
  mu = beta0 + beta1*l_LEU3N_0+beta2*age_0+beta3*BMI_0+beta4*RACE_01+beta5*EDUCBAS_02+beta6*EDUCBAS_03+beta7*smoke_01+beta8*group1+beta9*ADH_21;
  model diff_LEU3N ~ normal(mu, var = sigma2);
  title "Change in log10 CD4 cells and other variables";
run;
proc sgscatter data=callout;
   matrix beta0-beta9 sigma2;
run;
*remove beta8,GROUP;
proc mcmc data=input_mcmc nbi=10000 nmc=50000 outpost=callout plots=all DIC;
  parms beta0-beta7 0 beta9 0 sigma2 1; 
  prior beta0-beta7 beta9 ~ normal(mean = 0, var = 100000);
  prior sigma2 ~ igamma(shape = 2.001,scale = 1.001);
  mu = beta0 + beta1*l_LEU3N_0+beta2*age_0+beta3*BMI_0+beta4*RACE_01+beta5*EDUCBAS_02+beta6*EDUCBAS_03+beta7*smoke_01+beta9*ADH_21;
  model diff_LEU3N ~ normal(mu, var = sigma2);
  title "Change in log10 CD4 cells and other variables, without group";
run;
proc sgscatter data=callout;
   matrix beta0-beta7 beta9 sigma2;
run;




/*Vload*/
proc mcmc data=input_mcmc nbi=10000 nmc=50000 outpost=callout plots=all DIC;
  parms beta0 0 beta1 0 beta2 0 beta3 0 beta4 0 beta5 0 beta6 0 beta7 0 beta8 0 beta9 0 sigma2 1; 
  prior beta0-beta9 ~ normal(mean = 0, var = 100000);
  prior sigma2 ~ igamma(shape = 2.001,scale = 1.001);
  mu = beta0 + beta1*l_VLOAD_0+beta2*age_0+beta3*BMI_0+beta4*RACE_01+beta5*EDUCBAS_02+beta6*EDUCBAS_03+beta7*smoke_01+beta8*group1+beta9*ADH_21;
  model diff_VLOAD ~ normal(mu, var = sigma2);
  title "Change in log10 virus load and other variables";
run;
*remove beta8,GROUP;
proc mcmc data=input_mcmc nbi=10000 nmc=50000 outpost=callout plots=all DIC;
  parms beta0-beta7 0 beta9 0 sigma2 1; 
  prior beta0-beta7 beta9 ~ normal(mean = 0, var = 100000);
  prior sigma2 ~ igamma(shape = 2.001,scale = 1.001);
  mu = beta0 + beta1*l_VLOAD_0+beta2*age_0+beta3*BMI_0+beta4*RACE_01+beta5*EDUCBAS_02+beta6*EDUCBAS_03+beta7*smoke_01+beta9*ADH_21;
  model  diff_VLOAD ~ normal(mu, var = sigma2);
  title "Change in log10 virus load and other variables, without grou";
run;
proc sgscatter data=callout;
   matrix beta0-beta7 beta9 sigma2;
run;



/*Mental*/
proc mcmc data=input_mcmc nbi=1000 nmc=50000 outpost=callout plots=all DIC;
  parms beta0 0 beta1 0 beta2 0 beta3 0 beta4 0 beta5 0 beta6 0 beta7 0 beta8 0 beta9 0 sigma2 1; 
  prior beta0-beta9 ~ normal(mean = 0, var = 100000);
  prior sigma2 ~ igamma(shape = 2.001,scale = 1.001);
  mu = beta0 + beta1*AGG_MENT_0+beta2*age_0+beta3*BMI_0+beta4*RACE_01+beta5*EDUCBAS_02+beta6*EDUCBAS_03+beta7*smoke_01+beta8*group1+beta9*ADH_21;
  model diff_MENT ~ normal(mu, var = sigma2);
  title "Change in mental score and other variables";
run;
proc sgscatter data=callout;
   matrix beta0-beta9 sigma2;
run;
*remove beta8,GROUP;
proc mcmc data=input_mcmc nbi=10000 nmc=50000 outpost=callout plots=all DIC;
  parms beta0-beta7 0 beta9 0 sigma2 1; 
  prior beta0-beta7 beta9 ~ normal(mean = 0, var = 100000);
  prior sigma2 ~ igamma(shape = 2.001,scale = 1.001);
  mu = beta0 + beta1*AGG_MENT_0+beta2*age_0+beta3*BMI_0+beta4*RACE_01+beta5*EDUCBAS_02+beta6*EDUCBAS_03+beta7*smoke_01+beta9*ADH_21;
  model  diff_MENT ~ normal(mu, var = sigma2);
  title "Change in mental score and other variables, without group";
run;
proc sgscatter data=callout;
   matrix beta0-beta7 beta9 sigma2;
run;


/*Physical*/
proc mcmc data=input_mcmc nbi=1000 nmc=50000 outpost=callout plots=all DIC;
  parms beta0 0 beta1 0 beta2 0 beta3 0 beta4 0 beta5 0 beta6 0 beta7 0 beta8 0 beta9 0 sigma2 1; 
  prior beta0-beta9 ~ normal(mean = 0, var = 100000);
  prior sigma2 ~ igamma(shape = 2.001,scale = 1.001);
  mu = beta0 + beta1*AGG_PHYS_0+beta2*age_0+beta3*BMI_0+beta4*RACE_01+beta5*EDUCBAS_02+beta6*EDUCBAS_03+beta7*smoke_01+beta8*group1+beta9*ADH_21;
  model diff_PHYS ~ normal(mu, var = sigma2);
  title "Change in physical score and other variables";
run;
proc sgscatter data=callout;
   matrix beta0-beta9 sigma2;
run;
*remove beta8,GROUP;
proc mcmc data=input_mcmc nbi=10000 nmc=50000 outpost=callout plots=all DIC;
  parms beta0-beta7 0 beta9 0 sigma2 1; 
  prior beta0-beta7 beta9 ~ normal(mean = 0, var = 100000);
  prior sigma2 ~ igamma(shape = 2.001,scale = 1.001);
  mu = beta0 + beta1*AGG_PHYS_0+beta2*age_0+beta3*BMI_0+beta4*RACE_01+beta5*EDUCBAS_02+beta6*EDUCBAS_03+beta7*smoke_01+beta9*ADH_21;
  model  diff_PHYS ~ normal(mu, var = sigma2);
  title "Change in physical score and other variables, without group";
run;
proc sgscatter data=callout;
   matrix beta0-beta7 beta9 sigma2;
run;
