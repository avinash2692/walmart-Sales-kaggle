# Regression model: there are 2 model in this code giving 2 outputs:
#   reg_model1     = Gives a regression model for each Store:Dept combination based on the 
#                    Continous Variables that are available; could not incllude factors as 
#                    adding new levels to prediction
#   reg_modelfinal = This is a simple model based on intuition of what truly predicts sales 
#                    Taking only the week, month, Department and store as the predictors, 
#                    This model worked out to be a better model than the previous one.



rm(list=objects())
load("myTrain.Rdata")
load("myTest.Rdata")
# Initialize new datafram
lm.train <- mytrain
lm.test <- mytest
submission = read.csv('sampleSubmission.csv', header = T)
submission_full = read.csv('sampleSubmission.csv', header = T)
# modify dataset for regresison , The stores and departments can be taken as factors 
# and an individual reression model can be creadted for each store- model combination
#that exists. 

# Taking only year and week into account as a factor as the output need are weekly sale
#

# attach(lm.train)
store.name = unique(mytrain$store)
dept.name = sort(unique(mytrain$dept))
no.dept = length(dept.name)

dept_store_test = table(mytest$dept, mytest$store)
dept_store_train = table(mytrain$dept, mytrain$store)
year.matrix = matrix(data = 0,nrow = length(unique(lm.train$dept)) , ncol = 3)
row.names(year.matrix) = dept.name
colnames(year.matrix) = unique(lm.train$year)

lm.train$logsales = log(4990+lm.train$sales) # log transfrom suggested by a lot of people


#  Model:1 , with a regression only with continous varaibles and for each unique store:dept combination
# Eliminated 
for (s in store.name)
{
  print(s)
  for (d in dept.name)
  {
    if (dept_store_test[as.character(d),as.character(s)]==0 | dept_store_train[as.character(d),as.character(s)]==0) next
    
    tmp.d.fit = lm(logsales~  isholiday + temp + fuel + cpi + unemp , 
                   data=lm.train[which(lm.train$store == s & lm.train$dept==d ), ], na.action=na.exclude)
    tmp.d.predict = predict(tmp.d.fit,newdata = lm.test[which(lm.test$store == s & lm.test$dept==d ),])
    submission[which(lm.test$store == s & lm.test$dept==d ),2] = exp(tmp.d.predict) - 4990
  }
}
submission[which(is.na(submission[,2])),2] = mean(submission[,2])

write.table(x=submission,                                      
            file='output/reg_model1.csv',
            sep=',', row.names=FALSE, quote=FALSE)         



# Model 2 : doing a complete fit with only the Dept Size month and Weeks as predicitor:

fitall = lm(logsales ~  as.factor(store)+as.factor(dept) + size + holiday+
              as.factor(month) + as.factor(wk) ,data=lm.train,na.action = na.exclude)


forecast= exp(predict.lm (object = fitall, newdata = lm.test,na.action = na.exclude)) - 4990


filepath = "output/reg_model_final.csv";
ID=apply(mytest[,c("store", "dept", "date")], 1, function(x)  
  paste(x[1],  x[2],  as.character(x[3]), sep='_'))
#ID[1:5] 
# need to trim the space in ID
ID = gsub("\\s","", ID)
myout = data.frame(ID=ID, Weekly_Sales = forecast)
write.csv(myout, file=filepath,  row.names=FALSE, quote = FALSE);

