# ml_als

trial 1 :
trial for time series clustering of ALSFRS (dtw)
data refinement -> get DTW distance matrix -> data refinement -> hierarchical clustering


script #2 :
convert ALSFRS_R to ALSFRS
select patients with more or equal to 3 ALSFRS data
dtw time series clustering with sample patients -> select 4 clusters via hierarchical clustering
have a look at each clusters
tried to fit Weibull to a cluster but glm function cannot be applied.


script 3 :
classification with slopes
get each patients' slope and take a look
-> cut data to just 0~365 ALSFRS_Delta
then see slopes again


script 4 :
least square estimation of Weibull model (2 parameter) via nls function
(other optimization such as optim function also considered)
omit all same ALSFRS
many errors in optimization with whole time series -> try for just 365 days
Weibull model(2 parameter) fitting success, added sum of squares for all patients as a goodness of fit criteria
compare with linear model (lm function)


script 5 :
least square estimation of Weibull model (3 parameter) via minpack.lm::nlsLM
plot each patient's ALSFRS
See why rmse is a reasonable goodness of fit critria
compare rmse of Weibull and linear and give label according to smaller rmse
out of 5923 patients, 1547 is W and 4376 is L


script 6 :
get graphs of W labeled patients and L labeled patients for ppt


script 7 :
script for classicfication
data refinement of data "Labs.csv"
tidying data so that each patient has is a row and each lab tests are columns
get minimum ALSFRS_delta for each patients to get lab tests of that point -> lab.start
errors due to duplication
delete "X" from each subject_id
created lab data for machine learning
try ML classification via randomeForest::randomForest
failed due to missing data


script 8 : 171122
creating label (modification) 
calculate of relative errors of Weibull and linear
label based on density plot of relative error difference


script 9 :
data refinement for machine learning classification
calculate min and max ALSFRS total for first 90 days

script 10 :
Ongoing data refinement

script 11 :
Ongoing machine learning classification
