### PCV Impact Analysis Using Data from Manhica DSS
#### Basic approach: interrupted time series analysis with Bayesian model averaging

##### Code structure:
1. impact_analysis.r

   Main code that executes full analysis

2. prepData.r

   Code that loads/preps data. 
   Inputs:  
   * dir - directory where data live
  
    Outputs:  
   * data - all datasets in 'prepped' format (below)

2. its.r

   Function that carries out interrupted time series analysis.  
   Inputs:  
   * data     - data table object in 'prepped' format (see below)
   * outcome  - character. name of the outcome variable
   * cutpoint - date object containing the time point or points (up to 2) of intervention
   * slope    - logical. TRUE indicates that an interaction term (or terms) should be used to estimate a different slope before/after intervetion
   
   Outputs (in a list):  
   * data        - the input data object with six new columns: [outcome]_pred, [outcome]_pred_upper, [outcome]_pred_lower, [outcome]_cf, [outcome]_cf_upper, [outcome]_cf_lower,
   * outcome  - character. name of the outcome variable
   * cutpoint - date object containing the time point or points (up to 2) of intervention
   * effect size - a data frame containing the intercept shift associated with intervention, including uncertainty
   * gof         - goodness of fit based on BIC

3. bma.r

   Function that carries out Bayesian model averaging over multiple interrupted time series analyses.  
   Inputs:  
   * data     - data table object in 'prepped' format (see below)
   * outcome  - character. name of the outcome variable
   * cutpoint - date vector or matrix (up to 2 columns) containing the time points of intervention to include in BMA
   * slope    - logical. TRUE indicates that an interaction term (or terms) should be used to estimate a different slope before/after intervetion
   
   Outputs (in a list):  
   * data        - the input data object with six new columns: [outcome]_pred, [outcome]_pred_upper, [outcome]_pred_lower, [outcome]_cf, [outcome]_cf_upper, [outcome]_cf_lower,
   * effect size - a data frame containing the intercept shift associated with intervention, including uncertainty

4. cpbma.r

   Function that carries out change-point Bayesian model averaging following Kurum 2016.
   Inputs:  
   * data     - data table object in 'prepped' format (see below)
   * outcome  - character. name of the outcome variable
   * cutpoint - date vector or matrix (up to 2 columns) containing the time points of intervention to include in BMA
   * slope    - logical. TRUE indicates that an interaction term (or terms) should be used to estimate a different slope before/after intervetion
   
   Outputs (in a list):  
   * data        - the input data object with six new columns: [outcome]_pred, [outcome]_pred_upper, [outcome]_pred_lower, [outcome]_cf, [outcome]_cf_upper, [outcome]_cf_lower,
   * effect size - a data frame containing the intercept shift associated with intervention, including uncertainty

5. graph.r

   Function that produces a time series graph from the results of its.r, bma.r or cpbma.r.
   Inputs:  
   * itsOutput - list. standard output from its.r
   
   Outputs:  
   * p - a ggplot graph

   
##### Data formats:
1. 'prepped' format
FILL IN

2. 'output' format
FILL IN
