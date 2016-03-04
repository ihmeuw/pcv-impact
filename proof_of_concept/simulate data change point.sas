/*Simulate data for the excercise*/

data sim1;
	call streaminit(123); *set random seed;

	do t1=1 to 240;
	*COPD;
	if t1<107 then mu1=exp( 3.0  +0.004*t1 +rand("NORMAL",0,0.005) );
	else if t1>=107 then mu1=exp( 3  +0.004*t1 + (t1-107)*(-0.005) +  rand("NORMAL",0,0.005) );
	COPD_N = rand("POISSON", mu1);
	

	date=intnx("MONTH", "01Jan1996"d, t1);
	format date date9.;

	output;
	end;
run;
data sim1; set sim1;
if t1<50 then delete;
t=t1-50;
keep t date COPD_N ;
run;
proc sgplot data=sim1;
	series x=t y=copd_N;
run;


/*Instead of just testing  random knot locations, we want to test ALL knot locations between  month 6 and 114
to do this efficiently, we are going to rplicate the original data set 108 times, and in each of those
	replicates, we will test a different knot location (given by "cp")*/
data ds2; set sim1;
/*this do-loop replicates the dataset 108 times, and each of the replicates will be numbered from 6 to 114,
	corrsponding to different possible knot locations*/
	DO cp=6 to 234; 
	if t>=cp then spl= t-cp; /*in a particular replicate, if we are after the knot location (cp), then takes on a value of 1,2,3,4..., otehrwise it is 0*/
	else spl=0;

	output ;
	end;
run;
/*sort by the replicates*/
proc sort data=ds2;
	by cp t;
	run;
/*run he model separately fr each possible knot location using "by cp";*/
ods listing close; /*this just turns off the output window so that we don't overload it results*/
proc genmod data=ds2;
	by cp;
	ods output parameterestimates=parm_spl modelfit=modelfit_CP; /*outputs the parameter estimates and AIC scores to new files*/
	model copD_N= t spl /dist=poisson link=log;
	output out=pred_spl predicted=pred_cases;
run;
ods listing; /*this turns the output window back on*/

/*Plot predictions from all 108 models...it is a mess,  but some of these models fit very poorly and will
	not receive much weight*/
proc sgplot data=pred_spl;
	series x=t y=pred_cases/group=cp;
run;

/*keep the AIC score from each of the models*/
data aic1; set modelfit_CP;
	where criterion="AIC (smaller is better)";
	one=1; /*this just gives a convenient way to mereg the results back in after collapsing*/
run;
proc sort data=aic1;
	by value;
run;
/*plots the raw AIC score by change point knotlocation...smaller=better fit*/
proc sgplot data=aic1;
	scatter x=cp y=value;
run;

/*now we need to weight the models based on AIC scores. the best model has the lowest AIC score. Models nearte best receive more weight*/
		/** ofirst obtain the AIC score for best model and output to new dataset**/
					proc means noprint data=aic1 nway noprint;
					 var value;
					 class one;
					output out=minaic min=minvari;
					run;
					
			/*Merge the minimum AIC score in with all of the AIC score;
						Delta AIC gives AIC difference between model with minimum AIC and other models*/
				data aic2;
					merge aic1 minaic;
						by one;
						delta_aic = value- minvari; /*difference between each model's AIC and best AIC*/
					/*Relative likelihood of model given the data*/
					 wnum=exp((-0.5 * delta_aic)); /*this gives the raw weight of the model--NOTE THIS SEPCIFICATION ASSUME UNIFORM PRIOR ON ALL CHANGE POINTS
					 AND ASSUMES 100% PRIOR PROBABILITY THAT THERE IS A CHANGE SOMEWHERE*/
					run;
					/*we want to scale the weights so they add to one, so we sum the weights from all models,
						then divide by this value to get weights that add to 1*/
					proc means data=aic2 nway noprint;
						var wNUM;
						class one;
					output out=wdenominator sum=wden;
					run;
					/*GIVES THE AIC WEIGHTS*/
				data aicw;
					merge  aic2 wdenominator;
					by one;
					  w_Akaike= wnum/wden; /*this is the Scale AIC weight, which add to 1 across all models*/
					  keep cp w_akaike;
					run;
					proc sort data=aicw;
						by w_akaike;
						run;
				/*plot the model weights. You'll notice this is essentially an inverse of the graph of the AIC scores viewed above*/
				proc sgplot data=aicw;
					scatter x=cp y=W_akaike;
				run;
		
		/*next, we want to average the predicted values across all models. merge in the model weights with the predicted values*/
				proc sort data=pred_spl;
					by cp;
					run;
				proc sort data=aicw;
					by cp;
					run;
		data pred_spl2;
			merge pred_spl aicw;
				by cp;
		run;
		/*now take the weighted average of the predictions for each time point*/
		proc means data=pred_spl2 nway noprint;
			class t;
			var copD_N pred_cases;
			weight w_akaike; /*gives more weight to predictions from better-fitting models*/
			output out=pred_spl_ave mean=copD_N ave_pred_cases;
		run;
		/*view the averaged predictions*/
	proc sgplot data=pred_spl_ave;
		scatter x=t y=copD_N;
		series x=t y=ave_pred_cases;
	run;

	/*now let's look at the averaged  slope and 95% CI of the spl terms*/
	proc sort data=aicw;
		by cp;
		run;
		data parm_spl2;
			merge parm_spl(where=(parameter="spl")) aicw; /*merge in the model weights with the slope estimates for the "spl" term*/
			by cp;
			one=1;
		run;
		/*takes the weighted average of the slope estimates*/
		proc means data=parm_spl2 nway noprint;
			var estimate;
			class one;
			weight W_akaike;
			output out=ave_parm mean=beta_spl;
		run;

	/*confidence intervals around the average*/
					data ave_slope3;
						merge parm_spl2 ave_parm;
							by one;
							ave_var_piece= (stdErr**2+  (beta_spl-estimate)**2); /*the variance for each model depends on the model variance (stderr^2) as well as the between-model variation, which is given by difference between the slope estimate from a single model and the average slope estimate*/
												
					run;
					proc means data=ave_slope3 nway noprint;
						class one;
						var ave_var_piece;
						weight w_akaike;
						output out=variance mean(ave_var_piece)=variance; /*note there was an error here in class...should be mean, not sum*/
					run;
					data ave_slope_CIs;
						merge ave_parm variance;
						by one;
						/*the confidencer intervals are the mean +/- 1.96*stderr*/
						ave_slope_LCL=beta_spl-1.96*sqrt(variance);
						ave_slope_UCL=beta_spl+1.96*sqrt(variance);

						/*remember, these estimates are from a POISSON model, so interpret as:
							a 1 unit increase in time is associated with an exp(beta_spl)-fold decrease in cases, if you want
								to estimate change per year (12 months) it would be exp(beta_spl*12)*/
						IRR_YR=exp(beta_spl*12);
						IRR_YR_LCL=exp(ave_slope_LCL*12);
						IRR_YR_UCL=exp(ave_slope_LCL*12);
						run;
					proc print data=ave_slope_CIs;
						run;


		/*Main pieces to report*/
				/*1) the plot showing the weight/probability of a change at each time point*/
				proc sgplot data=aicw;
					scatter x=cp y=W_akaike;
				run;
				/*2) plot showing the averaged predictions*/
					proc sgplot data=pred_spl_ave;
						scatter x=t y=copD_N;
						series x=t y=ave_pred_cases;
					run;
				/*3) the estimates for the IRR per year*/
					proc print data=ave_slope_CIs;
					var irr:;
						run;


