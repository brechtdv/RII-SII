/**********************************************************************************************************/
									/*1.A. Simulation_for_PUb.sas;*/
/**********************************************************************************************************/

/*AIMS of the program:
I. Creating an database with varying shares of the Educational levels;
		INPUT files: 	- no input file, but newly created file 
						for rates: X:\Health information\FRenard\PHD\COMPOSITE_INDICES\DATASETS\workfiles\adjrates4.xlsx
						(=premature mortality rates of men based on census 2001);

OUTPUT: 	sim.simfreq		(1 line by EL and by composition of population); N=2528 lines, 632 different combinations);

II . Calculation of regression-based indices : SII and RII
		INPUT files:sim.simfreq	
		OUTPUT files: sim.regindex

(1 line by composition of population);

III.  /*MAKING THE PAFS*/ 
/*population-attributable fraction;

INPUT files:sim.simfreq	
		OUTPUT files: sim.paf*/

*/
/*************************************************************************************************************************/

*Define the libname;
*libname sim "\\iph.local\FS\Services\33_SLCD\DATA\Health information\FRenard\PHD\COMPOSITE_INDICES\DATASETS\workfiles";


/**********************************************************************************************************/
/*I. Creating a database with varying shares of the Educational levels*/
/**********************************************************************************************************/


*creating an empty structure;
Data simul_0;
 attrib
 p1 length = 8 
 p2 length = 8
 p3 length = 8
 p4 length = 8
 ;
*stop;
 Run;

*creating the lines;
 *P1,P2,P3 vary  , P4 is fixed to 1-the sum of the others; 
 data simul_1a;
 set simul_0;
retain num_a  1;
 do i=  0.05 to 0.5 by 0.05;
 p1=round(i,0.01);
	do j=  0.05 to 0.5 by 0.05;
 		p2=round(j,0.01);
		do k=  0.05 to 0.5 by 0.05;
 			p3=round(k,0.01);
				do EL=1 to 4;
 					p4= 1-sum(p1,p2,p3);
				
						output;
					
				end;
 			num_a +1;
 	
 		end;
		
 	end ;

end;

drop i j k ;
 run; /*4000 lines;
 num_a= counter with all the combinations of P1,p2,P3= 1000; negative values will be deleted in a further step*/

 *here I do the same with p1=1-(sum of the 3 others);
data simul_1b;
 set simul_0;
retain num_a  1001; /*put here the highest num from simul_1a +1*/
retain num_a p1 p2 p3 p4;
 do i=  0.05 to 0.5 by 0.05;
 p4=round(i,0.01);
	do j=  0.05 to 0.5 by 0.05;
 		p2=round(j,0.01);
		do k=  0.05 to 0.5 by 0.05;
 			p3=round(k,0.01);
				do EL=1 to 4;
 					p1= 1-sum(p2,p3,p4);
						output;
				end;
 			num_a +1;
 	
 		end;
		
 	end ;

end;
drop i j k ;
 run;

 /*setting together simul_1a and simul_1b*/
 /*nb: round because some floating problems*/
data simul_1_1;
set simul_1a simul_1b;
el1=round(100*p1);
el2=round(100*p2);
el3=round(100*p3);
el4=round(100*p4);
comb= 1000000*el1 + 10000*el2 + 100*el3 +el4;
run; /*num_a=counter of the different combinations; 2000 combinations*4; BUT: contains negative values and doubles to be deleted*/
/*8000 lines*/

/*removing the negative and null values, as well as the values of p1 and p4 above 0.5;*/
data simul_1_2;
set simul_1_1;
if p1< 0.05 or p4<0.05 then delete;/*delete null and negative*/
if p1>0.5 or p4>0.5 then delete;/* too high p1 and p4 values;*/
run;/*4888 lines*/
/*check*/
 proc means data=simul_1_2;
 var p1 p2 P3 p4;
 run;/*OK*/


/*looking for double combinations*/

data double_0;
 set simul_1_2;
 if EL=1;

 keep num_a comb p1_100 p2_100 p3_100 p4_100;
 run;

proc sort data=double_0;by comb;run;

data /*sim.*/double;
retain flag;
set double_0;
by comb;
if first.comb then double=0;
else double+1;
run;

/*check*/
proc freq data=/*sim.*/double;
tables double;
run;/*632 different different combinations of the Ps,490 doubles*/


proc sort data=simul_1_2; by num_a;run;
proc sort data=/*sim.*/double;by num_a;run;

*makig a new simulation file without the doubles;
data simul_1_2_nodouble;
merge simul_1_2 /*sim.*/double(keep=num_a comb double);
by num_a;
if double=1 then delete;
run;

proc freq data=simul_1_2_nodouble;
tables double;
run;/*2528 lines ,corresponding to 632 different combinations, times 4 since there are 4 ELs*/





/*setting a new counter of the different p1 to p4 combinations, without the negative values and without the doubles= num */
Proc sort data=simul_1_2_nodouble;by p1 p2 P3  el;run;

data simul_1;
retain num;
set simul_1_2_nodouble;
by p1 p2 P3  el;
lagp1=lag(P1);lagp2=lag(P2);lagp3=lag(P3);
if p1 ne lagp1 or P2 ne lagp2 or p3 ne lagP3 then num+1;
drop lagp1 lagp2 lagp3 num_a;
run;  /*Lines= 2528 ;Num 632*/

/*check*/
 proc means data=simul_1;
 var p1 p2 P3 p4 num;
 run; /*OK*/

 

/*permanently save this structure, with the hypothetical population compositions: 632 comp, so 2528 lines*/
 data sim.simul_1;
set simul_1;run;

/*copying 1 line by number, for further use*/
 data sim.num_freq;
 set simul_1;
 if EL=1;
 keep num p1 p2 p3 p4;
 run;
proc sort data=sim.num_freq;by num;run;

/*attributing EL-spec rates;*/
/*Here any plausible rates can be added; I chose for males premature mortality rates in BE in 2001, in 4 ELs*/  
 /*INPUT VALUES: X:\Health information\FRenard\PHD\COMPOSITE_INDICES\DATASETS\workfiles\adjrates4.xlsx;
 produced by Stata-prog: X:\Health information\FRenard\PHD\COMPOSITE_INDICES\PROG\Dstize_fullbe.do";*/ 

 *creating a new variable "freq" which is the proportion of the specific EL corresponding to the line";

data simul_2;
 retain num EL; /*ordering variables*/
 set simul_1;
if EL=1 then do;
freq=p1;
adjrate=0.007338;
*se=0.000071933653; 
end;
if EL=2 then do;
freq=p2;
adjrate=0.005521;
*se=0.000034933750; 
end; 
if EL=3 then do;
freq=p3;
adjrate=0.004501;
*se=0.000028940925;
end;
if EL=4 then do;
freq=p4;
adjrate=0.003139;
*se=0.000023839550;
end;
 run;

*calculating the midpoint of each class (ridit);
proc sort data=simul_2;by num el;run;

data simul_3;
retain num el freq cum_freq ;
set simul_2;
by num el;
if first.num then do ;
cum_freq=freq;
end;
else do;
cum_freq+freq;
end;
run;


data simul_4;
retain num el freq cum_freq ridit;
set simul_3;by num;
lagcum=lag(cum_freq);
if first.num then lagcum=0;
ridit=lagcum+freq/2;
drop lagcum;
*calculating the events;
py=freq* 20294723  ;
events=round(adjrate*py);
se=sqrt(events)/py;
run;
/*******************************************/

*permanently save the file with the proportion of EL and the rates = OUTPUT FILE; 
data sim.simfreq;
set simul_4;
run;
*saving also in excell for further work ;
proc export data=sim.simfreq
OUTFILE= "X:\Health information\FRenard\PHD\COMPOSITE_INDICES\DATASETS\workfiles\simfreq.xlsx"
            DBMS=xlsx REPLACE;
			putnames=yes;	

run;


/**********************************************************************************************************************/

						/*II . Calculation of regression-based indices SII and RII*/

/**********************************************************************************************************************/

/*making a file with the mean rate for each combination of the EL distribution;*/

proc means data=sim.simfreq mean stderr;
by num;
var adjrate;
wgt freq;
output out=ybar_1 mean=ybar ;
run;


data sim.ybar;
set ybar_1;
py= 20294723  ;
events=round(ybar*py);
ev_se=sqrt(events);
stderr=ev_se/ 20294723  ;
ybar_var=stderr*stderr;
run;

*saving the ybar in excell;
proc export data=sim.ybar
OUTFILE= "X:\Health information\FRenard\PHD\COMPOSITE_INDICES\DATASETS\workfiles\ybar.xlsx"
            DBMS=xlsx REPLACE;
			putnames=yes;	

run;


/* regress adjusted prevalence rates on cumulative SES classes */
	*proc reg data=simul_4 simple ;
proc sort data=sim.simfreq  ; by num; run;

proc reg data=sim.simfreq simple ;
		by num;
	id p1 p2 p3 p4;
		weight freq;
		model adjrate=ridit/clb covb;
        *ods output ParameterEstimates=regparms;
        ods output CovB=sim.covparms;
		run; 

/* get estimates, var and ci for intercept */
	data alphas; set regparms;
		where Variable="Intercept";
		alpha     = Estimate;
		alpha_VAR = StdErr**2;
		alpha_LCL = LowerCL;
		alpha_UCL = UpperCL;
		keep num alpha alpha_VAR alpha_LCL alpha_UCL; run;

/* get estimates, var and ci for slope */
	data betas; set regparms;
		where Variable="ridit";
		beta     = Estimate;
		beta_VAR = StdErr**2;
		beta_LCL = LowerCL;
		beta_UCL = UpperCL;
		keep num p1 p2 p3 p4 beta beta_VAR beta_LCL beta_UCL; run;




/* merge alphas, betas and ybar info */
	data regindex_1; 
		retain num xbar ybar ybar_VAR  alpha alpha_VAR alpha_LCL alpha_UCL beta beta_VAR beta_LCL beta_UCL;
		merge sim.num_freq sim.ybar alphas betas;
		xbar = 0.5;
		by num; run;

/* compute regression based indices */
	data regindex; set regindex_1;
    	SII      = beta;
	 	SII_VAR  = beta_VAR;
	 	SII_LCL  = beta_LCL;
	 	SII_UCL  = beta_UCL;

	 	RII     = alpha/(alpha+beta);

	 	RII_VAR = (beta*beta*ybar_VAR + ybar*ybar*beta_VAR)/((ybar+beta*(1-xbar))**4);
	 	RII_LCL = RII - quantile('T',(1-0.05/2),3)*sqrt(RII_VAR); 					/* T(0.025,3) = 3.182 */	
	 	RII_UCL = RII + quantile('T',(1-0.05/2),3)*sqrt(RII_VAR);						/* T(0.025,3) = 3.182 */
alpha_se=sqrt(alpha_var);
beta_se=sqrt (beta_var);
	 	keep num p1 p2 p3 p4 alpha alpha_var alpha_se beta beta_var beta_se SII SII_VAR SII_LCL SII_UCL RII RII_VAR RII_LCL RII_UCL; run;

*saving a permanent file;
		data sim.regindex;
		set regindex;
		run;



/*****************************************************************************************************************/
											/*MAKING THE PAFS*/
/*****************************************************************************************************************/

*making a file with the reference rates;

data refrate;
set sim.simfreq;
where el=4;
run;

*merging file with the refrate and the file with the mean rate;
proc sort data=refrate;by num;run;
proc sort data=sim.ybar;by num;run;



data sim.paf;
merge refrate (keep=num p1 p2 p3 p4 adjrate events py ridit 
se rename=(adjrate=refrate se=refrate_se events=refevents py=refpy ridit=refridit)) sim.ybar(drop=_type_ _freq_);
by num;
refrate_var=(refrate_se)**2;
PAF=(ybar-refrate)/ybar;
PAF_var=(refrate_var + ybar_var);
PAF_SE=sqrt(PAF_var);
PAF_LCL=PAF - 1.96*PAF_SE;
PAF_UCL=PAF  + 1.96*PAF_SE;
run;



	
