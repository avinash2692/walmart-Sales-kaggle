###################################################################
# A Simple Model:
###################################################################
# predict the sale at (store, dept, wk, year) by the median of the
# following three sales a year ago at the same (store, dept) 
# at (wk-1, wk, wk+1). 
# If there is not enough historical data (history < 1 yr), 
# then just use the median of all other non-NA predictions. 


library(lubridate)
rm(list=objects())
load("myTrain.Rdata")
load("myTest.Rdata")

#table(mytrain$year, mytrain$wk) # 53 weeks per year

dept.names = sort(unique(mytrain$dept))
num.stores = length(unique(mytest$store))
num.depts = length(dept.names)


dept_store_test = table(mytest$dept, mytest$store)
dept_store_train = table(mytrain$dept, mytrain$store)

mypred = rep(NA, nrow(mytest)) # vector used to store the prediction

for(i in 1:num.depts){
  print(paste("dept = ", i, "....")) # keep track of the process
  for(j in 1:num.stores){
    if (dept_store_test[i,j]==0 | dept_store_train[i,j]==0) next
    
    # for each store+dept combo, filter the train/test data   
    tmp.train.id = which(mytrain$dept==dept.names[i] & mytrain$store==j)
    tmp.train.sales = mytrain$sales[tmp.train.id]; 
    tmp.test.id = which(mytest$dept==dept.names[i] & mytest$store==j)
    
    # cumulative weeks; set week 1 in training as the reference
    week1 = week("2010-02-05")
    cum.wk.train = (mytrain$year[tmp.train.id] -2010)*53+mytrain$wk[tmp.train.id]
    cum.wk.train = cum.wk.train - week1+1; 
    cum.wk.test = (mytest$year[tmp.test.id] -2010)*53+mytest$wk[tmp.test.id]
    cum.wk.test = cum.wk.test - week1 + 1; 
    
    # there could be some missing weeks in the training data for store+dept
    # Note that tmp.train.sales may have NA values
    tmp.train.sales = rep(NA, max(cum.wk.train))
    tmp.train.sales[cum.wk.train] = mytrain$sales[tmp.train.id];
    
    
    # create the 3 sales for each record in the test data
    hist.data = matrix(, length(tmp.test.id), 3)
    hist.data[,1] = tmp.train.sales[cum.wk.test-53-1]; 
    hist.data[,2] = tmp.train.sales[cum.wk.test-53];
    hist.data[,3] = tmp.train.sales[cum.wk.test-53+1];
    
   med.pred = apply(hist.data, 1, function(x) median(x[!is.na(x)]))
   #med.pred = apply(hist.data, 1, function(x) median(x,na.rm = T))
    # If no prior year's data available, just predict it to be the median
    # of the other predicted values
    med.pred[is.na(med.pred)] = median(med.pred, na.rm=TRUE)
    
   length(med.pred[which(is.na(med.pred))])
   
   mypred[tmp.test.id] = med.pred     
  }
}

length(mypred[which(is.na(mypred))])
# 319 missing values, These occur as there are certain dates 
#that are available in the test data set but have no corresponding historical 
#week in the training data set , all set to 0

mypred[which(is.na(mypred))] = 0
mypred[mytest$missing==1] = NA # Setting the missing values back to their original missing value

##################################################################
# No histrical data available for the 36 obs in the test set
##################################################################


       
mean.store<-ave(mytrain$sales, mytrain$store)
mean.store = unique(mean.store)

mean.dept = unique(ave(mytrain$sales, mytrain$dept))
mean.dept = as.matrix(mean.dept)
row.names(mean.dept) = dept.names



temp.id = which(is.na(mypred), arr.ind = F)
for(k in temp.id)
{
  tmp.store = mytest[k,'store']
  tmp.dept = as.character( mytest[k,'dept'])
  mypred[k] = mean(mean.store[tmp.store],mean.dept[as.character(tmp.dept),],na.rm = TRUE)
}





##################################################################
# The prediction for holidays need to be adjusted
##################################################################

sales.christmas = cbind (mytrain$store[mytrain$holiday == 'Christmas'],
                         mytrain$dept[mytrain$holiday == 'Christmas'],
                         mytrain$sales[mytrain$holiday == 'Christmas'])
colnames(sales.christmas) = c('store','dept','holiday')

sales.thanksgiving = cbind (mytrain$store[mytrain$holiday == 'Thanksgiving'],
                         mytrain$dept[mytrain$holiday == 'Thanksgiving'],
                         mytrain$sales[mytrain$holiday == 'Thanksgiving'])
colnames(sales.thanksgiving) = c('store','dept','holiday')

sales.super.bowl = cbind (mytrain$store[mytrain$holiday == 'Super_Bowl'],
                         mytrain$dept[mytrain$holiday == 'Super_Bowl'],
                         mytrain$sales[mytrain$holiday == 'Super_Bowl'])
colnames(sales.super.bowl) = c('store','dept','holiday')

for (k in dept.names)
{
  for (l in num.stores)
  {
    mypred[mytest$store ==l & mytest$dept == k & mytest$holiday == 'Christmas'] = 
      mean(sales.christmas[(sales.christmas[,1] ==l & sales.christmas[,2] == k),3], na.rm=T)
    
    mypred[mytest$store ==l & mytest$dept == k & mytest$holiday == 'Thanksgiving'] = 
      mean(sales.thanksgiving[(sales.thanksgiving[,1] ==l & sales.thanksgiving[,2] == k),3], na.rm=T)
    
    mypred[mytest$store ==l & mytest$dept == k & mytest$holiday == 'Super_Bowl'] = 
      mean(sales.super.bowl[(sales.super.bowl[,1] ==l & sales.Super.bowl[,2] == k),3], na.rm=T)
    
  }
}

##################################################################
# Prepare the submission file
##################################################################

# For each row in the test set (store + department + date triplet), 
# you should predict the weekly sales of that department. The Id column 
# is formed by concatenating the Store, Dept, and Date with underscores 
# (e.g. Store_Dept_2012-11-02).  The file should have a header and looks 
# like the following:
#
# Id,Weekly_Sales
# 1_1_2012-11-02,0
# 1_1_2012-11-09,0
# 1_1_2012-11-16,0


filepath = "output/simple_model.csv";
ID=apply(mytest[,c("store", "dept", "date")], 1, function(x)  
  paste(x[1],  x[2],  as.character(x[3]), sep='_'))
#ID[1:5] 
# need to trim the space in ID
ID = gsub("\\s","", ID)
myout = data.frame(ID=ID, Weekly_Sales = mypred)
write.csv(myout, file=filepath,  row.names=FALSE, quote = FALSE);

