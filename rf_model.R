# Random Forests 

########          CODE  COURTESY    #############
# Michael Kim (mikeskim  AT  g m a i l  DOT c o m)
# https://www.kaggle.com/users/64626/mike-kim
# The following code is ased on the model suggested
# by the author above, with a few minor tweaks.
##################################################
library(randomForest)
rm(list=objects())
load("myTrain.Rdata")
load("myTest.Rdata")

# Make a copy of data

rf.train = mytrain
rf.test = mytest
submission = read.csv('sampleSubmission.csv', header= T , as.is=T)

rf.train$logsales = log(4990+rf.train$sales)

# Randome Forests Model (reduced Tree to 300 after eliminating certain factors)

 n.row = nrow(submission) 
# # id = nrow(submission)
j=1
while (j < n.row) {
  print(j/n.row)#keep track of progress
  #select only relevant data for the store and department tuple
  tmpId = submission$Id[j]
  tmpStore = rf.test$store[j]
  tmpDept = rf.test$dept[j]
  train.dept = rf.train[rf.train$dept==tmpDept,] # all departments that meet the Id requirement
  nrow.train.dept = nrow(train.dept[train.dept$store==tmpStore,]) # No. of Rows in that table
  
  #since MAE is weighted, increase weights of holiday data by 5x
  
  train.dept.holiday = train.dept[train.dept$isholiday == T,]
  train.dept = rbind(train.dept,do.call("rbind", replicate(4, train.dept.holiday, simplify = FALSE)))
  train.dept.store = train.dept[train.dept$store==tmpStore,]  # All the Stores in the Subset of train.dept
  test.dept.store = rf.test[rf.test$dept==tmpDept,]
  test.dept.store = test.dept.store[test.dept.store$store==tmpStore,]
  testRows = nrow(test.dept.store)
  if (nrow.train.dept<10) {#sample size restrictions since rf can fail if there isn't enough data
    #this model uses all dept data (since that store + dept pair does not exist in the training set)
    tmpModel =  randomForest(logsales~size+type+year+month+wk+isholiday,
                             ntree=300, replace=TRUE, mtry=4, data=train.dept)}
  else {
    #this model is trained on store+dept filtered data
    tmpModel =  randomForest(logsales ~ year+month+wk+isholiday+type, 
                             ntree=300, replace=TRUE, mtry=3, data=train.dept.store)}
  tmpP = exp(predict(tmpModel,test.dept.store))-4990
  k = j + testRows - 1
  submission$Weekly_Sales[j:k] = tmpP
  j = k+1
}



#write the submission to csv for Kaggle submission
write.table(x=submission,
            file='output/Rf_submission.csv',
            sep=',', row.names=FALSE, quote=FALSE)

