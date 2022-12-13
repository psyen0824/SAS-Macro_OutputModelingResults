/*****************************************************************************************
Purpose: Unweighted Logistic/Linear Regression Model - Output multiple modeling results into Excel
Dataset: PBC
Date: 12/13/2022
Demonstrated Version: SAS 9.4
Author: Pei-Shan Yen
Biostatistics Core, Center for Clinical and Translational Science, University of Illinois at Chicago
*********************************************************************************************/

/*Read Dataset*//*Import PBC data from my drive*/ 
PROC IMPORT OUT= WORK.PBC_Temp 
            DATAFILE= "G:\My Drive\UIC RA CCTS\20190911 Side Project_Group Comparison SAS Macro\Group Comparison SAS Macro\Data\pbc.csv" 
            DBMS=csv REPLACE;
            GETNAMES=YES;
RUN;

/*format for categorical variables*/ 
PROC FORMAT;
	value status_F   0="alive"           1="liver transplant" 2="dead" .=' ';
    value trt_F      0="placebo"         1="Dpenicillamine"  .=' ';
	value sex_F      0="male"            1="female" .=' ';
	value stage_F    0="stage I&II"      1="stage III"     2="stage IV";
	value hepato_F   0="no"              1="yes"  .=' ';

RUN;

DATA WORK.PBC;
 SET WORK.PBC_Temp(OBS=312);
if trt = 1 then trt_f = 1;
else if trt = 2 then trt_f = 0;

if stage in (1,2) then stage_f = 0;
else if stage = 3 then stage_f = 1; 
else if stage = 4 then stage_f = 2; 

label 	
status    = "status"
trt_f     = "for D-penicillmain, placebo, not randomised"
age       = "age in years"
sex       = "sex"
hepato    = "presence of hepatomegaly or enlarged liver"
stage_f   = "histologic stage of disease (needs biopsy)"
platelet  = "platelets per cubic ml/1000";

/*Outcome*/
format status status_F.;
format trt_f trt_F.;
format sex sex_F.;
format stage_f stage_F.;; 
format hepato hepato_F.; 
RUN;

/*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*/
/*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*/
/*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*/
/*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*/
/*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*//*Logistic Model*/
%macro Unweighted_Logistic_Model (series =, sn_model=, out=, cov=, class=,  descending=, data= ,pop=);  
/*Modeling*/
ods listing close;
ods select ResponseProfile ParameterEstimates OddsRatios Fitstatistics;
ods output ResponseProfile = Size ;
ods output ParameterEstimates = Para;
ods output OddsRatios = OR;
ods output Fitstatistics = Fit;
proc logistic data =  &data &descending;
 where &pop;
 class &class;
model &out = &cov/expb gof rsquare;
run;
ods listing;

/*Obtain AUC*/
ods listing close;
ods select ROCASSOCIATION;
ods output ROCASSOCIATION = AUC;
proc logistic data =  &data &descending plots(only)=roc;
 where &pop;
 class &class;
model &out = &cov/OUTROC = ROC;
    ROC;
run;
ods listing;

/*Sample Size*/
data T_Size_Yes(keep = n_Yes);
set Size;
 if OrderedValue = 1;
 n_Yes = count;
run;

proc sql;
    create table T_Size_Total as
        select sum(count) as n_Total
        from Size;
quit;

/*R-Square*/
data T_McFadden_R2(keep=McFadden_R2);
set Fit;
 if RowId = "MCFADDEN";
 McFadden_R2 = compress(put(Value,8.3));;
run;

data T_CoxSnell_R2(keep=CoxSnell_R2);
set Fit;
 if RowId = "RSQUARE";
 CoxSnell_R2 = compress(put(Value,8.3));;
run;

/*AUC*/
data T_AUC(keep=AUC);
set AUC;
 if ROCModel = "Model";
 AUC = compress(put(Area,8.3));;
run;

/*Parameter Estimation*/
data T_Para;
set Para;
 Model_series = &series;
 Model_sn = &sn_model;
 Cov_sn = _n_;
 Covariate = Variable;
    Para_Esti = compress(put(Estimate,8.3));
	Para_Std  = compress(put(StdErr,8.3));
    P_Value   = compress(put(ProbChiSq,8.3));
	keep Model_series Model_sn Cov_sn Covariate Para_Esti Para_Std P_Value;
run;

/*Odds Ratio*/
data T_OR;
set OR;
 Cov_sn = _n_ + 1;
    L_OR = compress(put(LowerCL,8.3));   
    OR   = compress(put(OddsRatioEst,8.3));
    U_OR = compress(put(UpperCL,8.3));   
	keep Cov_sn Effect L_OR OR U_OR;
run;

proc sql;
   insert into T_OR
        set Effect = "Intercept",
            Cov_sn = 1;
quit;

proc sort data = T_OR;
by Cov_sn;
run;

/*Combine all model*/
data M&sn_model; 
merge T_Para T_OR T_Size_Total T_Size_Yes T_McFadden_R2 T_CoxSnell_R2 T_AUC;
proc sort data = M&sn_model; 
by model_series model_sn Cov_sn;
run;

/*Delete temporary dataset*/
proc datasets library=work;
delete Size T_size_total T_size_yes Fit T_McFadden_R2 T_CoxSnell_R2 ROC AUC T_AUC Para T_Para OR T_OR;
run;
quit;
%mend;

/*Perform Logistic Model*/
  /*Step1. Define Covariate for Logistic Model*/
data dt_M1;
   length name $400.;
name = 'trt_f' ; output ;
name = 'trt_f sex age' ; output ;
name = 'trt_f sex age stage_f' ; output ;
name = 'trt_f sex age stage_f hepato' ; output ;
;
RUN;

data dt_C1;
   length name $400.;
name = 'trt_f(ref=first)/ param=ref' ; output ;
name = 'trt_f(ref=first) sex(ref=first)/ param=ref' ; output ;
name = 'trt_f(ref=first) sex(ref=first) stage_f(ref=first)/ param=ref' ; output ;
name = 'trt_f(ref=first) sex(ref=first) stage_f(ref=first) hepato(ref=first) / param=ref' ; output ;
;
RUN;

proc sql;
select compress(put(count(*),best.) ) 
into : M1_n 
from dt_M1;
quit;

proc sql;
select compress(put(count(*),best.) ) 
into : C1_n 
from dt_C1;
quit;

data _null_;  
set dt_M1;
call symput('M1'||compress(put(_n_,best.)),name);  
run;

data _null_;  
set dt_C1;
call symput('C1'||compress(put(_n_,best.)),name);  
run;

  /*Step2. Use Macro to Obtain Multiple Modeling Results*/
%macro do_Unweighted_Logistic_Model(out=, n_cov=, descending=,data=,pop=);
	%do i=1 %to &n_cov;
%put &&M1&i ;
%Unweighted_Logistic_Model( series =1, sn_model = &i, out = &out, cov = &&M1&i, class = &&C1&i, descending=&descending, data=&data, pop=&pop);
  %end;
%mend;

%do_Unweighted_Logistic_Model(out = status, n_cov = &C1_n, descending = descending, data = pbc, pop = status^=2);

data Out_logistic_Temp;
length Effect $400.;
length Covariate $400.;
set M:;
run;

proc sql;
create table Out_Logistic as
select model_series, model_sn, n_Total, n_Yes, Cov_sn, Covariate, Effect, Para_Esti, Para_Std, P_value, L_OR, OR, U_OR, McFadden_R2, CoxSnell_R2, AUC
from  Out_Logistic_Temp;
quit;

proc export data= Out_Logistic
outfile='PBC_Logistic_Model_12132022.xlsx' 
dbms = xlsx replace;
sheet="Logistic";
run;


/*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*/
/*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*/
/*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*/
/*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*/
/*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*/
/*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*//*Linear Model*/
%macro Unweighted_Linear_Model (series =,sn_model=, out=, cov=, descending=, data= , pop=); 
ods listing close;
ods select Nobs ParameterEstimates Fitstatistics;
ods output ParameterEstimates=Para;
ods output Nobs= Size ;
ods output Fitstatistics = Fit;
proc reg data=&data PLOTS=NONE;
model &out= &cov;  
quit;
ods listing;

/*Sample Size*/
data T_Size(keep = n_Total);
set Size;
 if Label = "Number of Observations Used";
 n_Total = NObsUsed;
run;

/*R-Square*/
data T_R2(keep=Adj_R2);
set Fit;
 if Label2 = "Adj R-Sq";
 Adj_R2 = compress(put(nValue2,8.3));;
run;

/*Parameter Estimation*/
data T_Para;
set Para;
 Model_series = &series;
 Model_sn = &sn_model;
 Cov_sn = _n_;
 Covariate = Variable;
    Para_Esti = compress(put(Estimate,8.3));
	Para_Std  = compress(put(StdErr,8.3));
    P_Value   = compress(put(Probt,8.3));
	keep Model_series Model_sn Cov_sn Covariate Label Para_Esti Para_Std P_Value;
run;

/*Combine all model*/
data Lin_M&sn_model; 
merge T_Size T_Para T_R2;
proc sort data = M&sn_model; 
by model_series model_sn Cov_sn;
run;

/*Delete temporary dataset*/
proc datasets library=work;
delete Size T_Size Fit T_R2 Para T_Para ;
run;
quit;
%mend;

/*Perform Linear Model*/
  /*Step1. Define Covariate for Logistic Model*/
data dt_Cov1;
   length name $400.;
name = 'trt_f' ; output ;
name = 'trt_f sex age' ; output ;
name = 'trt_f sex age stage_f' ; output ;
name = 'trt_f sex age stage_f hepato' ; output ;
;
RUN;

proc sql;
select compress(put(count(*),best.) ) 
into : Cov1_n 
from dt_Cov1;
quit;

data _null_;  
set dt_Cov1;
call symput('Cov1'||compress(put(_n_,best.)),name);  
run;

 /*Step2. Use Macro to Obtain Multiple Modeling Results*/
%macro do_Unweighted_Linear_Model(out=, n_cov=, data=,pop=);
	%do i=1 %to &n_cov;
%put &&Cov1&i ;
%Unweighted_Linear_Model( series =1, sn_model = &i, out = &out, cov = &&Cov1&i, data=&data, pop=&pop);
  %end;
%mend;

%do_Unweighted_Linear_Model(out = platelet, n_cov = &Cov1_n, data = pbc, pop = status^=2);

data Out_Linear_Temp;
length Label $400.;
length Covariate $400.;
set Lin_M:;
run;

proc sql;
create table Out_Linear as
select model_series, model_sn, n_Total, Cov_sn, Covariate, Label, Para_Esti, Para_Std, P_value, Adj_R2
from  Out_Linear_Temp;
quit;

proc export data= Out_Linear
outfile='PBC_Linear_Model_12132022.xlsx' 
dbms = xlsx replace;
sheet="Linear";
run;
