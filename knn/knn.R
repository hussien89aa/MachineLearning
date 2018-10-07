#Knn( K-nearest neighbor) classifier implementation in R 
#Author: Hussein Alrubaye
rm(list = ls())
diabetes <- read.table(file.choose(), sep = ',' )
diabetes<- diabetes[-1,]; ## remove header out 768 rows

# convert data to numeric
data_numeric<- function(x){ (as.numeric( as.character(x)))}
diabetes.numeric <-  as.data.frame(lapply(diabetes, data_numeric))

#normilize data
data_norm<- function(x){ ((x-min(x))/(max(x)-min(x)) )}
diabetes.norm <-  as.data.frame(lapply(diabetes.numeric, data_norm)) 

# find train and test range
cols<- ncol(diabetes)
rows<- nrow(diabetes)
train.rows<- as.integer(rows*0.7) # 70% of data as training and 30% as testing

# split data to train and test
xtrain <- diabetes.norm[1:train.rows,1:(cols-1)]
ytrain <- diabetes.norm[1:train.rows, cols]
xtest <- diabetes.norm[(train.rows+1):rows,1:(cols-1)] 
ytest <- diabetes.norm[(train.rows+1):rows,cols]

#apply KNN
library(class)
ytest_pred <- knn(xtrain,xtest,ytrain, k=1,prob=T)

#find confusion matrix
confusion.matrix<-table(ytest_pred,ytest)
print(confusion.matrix)

# find f- meausre
fMeasure<- (confusion.matrix[1,1]+ confusion.matrix[2,2])/
  (confusion.matrix[1,1]+ confusion.matrix[1,2]+confusion.matrix[2,1]+ confusion.matrix[2,2])
print(fMeasure)

# new data
diabetes_pred<-merge(diabetes[(train.rows+1):rows,1:(cols-1)] ,ytest_pred, all = TRUE) 
View(diabetes_pred)

