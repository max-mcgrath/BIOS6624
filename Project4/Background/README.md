## Project Description

Investigation of Variable Selection Algorithms: The goal of this project is to 
investigate different model selection techniques in linear regression.  We will 
investigate choosing the best fitting model using backward selection based on 
F-tests and p-values (automated), AIC, BIC, lasso, and elastic net. The cases to
investigate are 1) N=250 subjects with 20 variables in the model. 5 of them 
would be considered important with coefficients between 0.5/3 and 2.5/3 in 0.5/3
unit increments (0.5/3, 1/3, 1.5/3, 2.0/3, and 2.5/3). In case 1a consider all X
variables as independent from one another. In case 1b, induce correlation 
between the X variable (we’ll have to specify how as a group). 2) N=500 subjects
with 20 variables in the model. 5 of them would be considered important with 
coefficients between 0.5/3 and 2.5/3 in 0.5/3 unit increments (0.5/3, 1/3, 
1.5/3, 2.0/3, and 2.5/3). In case 2a consider all X variables as independent 
from one another. In case 2b, induce correlation between the X variable (we’ll 
have to specify how as a group).

For each case first simulate the data using the hdrm package that can be found 
on github. Then fit each model using the 5 model fitting approaches above. There
are different choices for how to choose lambda for lasso and elastic net. 
Investigate at least 2 choices (we’ll have to specify the choices as a group). 
Summarize your findings in terms of true positives (what percentage of the time 
are variables X1 - X5 in the final model) and false positive rates (confusion 
matrix is another name for this). In other words, how often does your final 
model include any of the 15 variables that are not associated with the outcome 
and how often does your final model include each of the 5 variables with a known
relationship with Y. For the variables that are retained in the model, what is 
the bias of the parameter estimates, what is the coverage of the 95% CI, and 
what is the type I and II error of the variables selected.  For the variables 
retained, you can do testing on the final model (in other words refit the final 
model like no model selection was done) or propose an alternative approach to 
testing since we believe there is bias in just fitting the final model like 
there was no variable selection done.

I will post papers that may be of help. A google scholar search will identify 
many more papers that could be of use I am sure.