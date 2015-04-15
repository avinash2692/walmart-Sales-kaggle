Kaggle Walmart competition:
########## Code Courtesy Prof Feng liang and Mike Kim (From Kaggle)##############
Set the working directory to where the raw file are located
The following code contains 4 code files, in their order of execution: 

1) Data_prep_visual (run first):  Loads the data, does the data preprocessing and provide 
   basic visual inferences.

2) Simplemodel_median : Runs the simple model discussed in section 3 in the report

3) Regression_model: Runs the linear regression models, as discussed int section 3 of the 
   report

4) rf_model : runs the random forests model.

Also included are 3 output data files that were submitted to Kaggle for their scores

The models don't perform optimally as compared to the ones on the leader board, 
further tweaks have to be made. Major take away was the inability of regression to handle 
dates as a factor(owing to the fact that in a time series data, new dates do come up)
