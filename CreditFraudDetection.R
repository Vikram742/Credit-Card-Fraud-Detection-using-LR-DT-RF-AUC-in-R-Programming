# set the working directory and read the data
fraud<- read.csv("D:/IIMK/R Basic/Regression Analysis/Project/Credit Fraud Detection/creditcard.csv")
#check the columns and rows
dim(fraud)
# data cleaning
# check for missing values
sum(is.na(fraud))
# there are no missing values
# check data types
str(fraud)
#convert dependent variable "Class" into a factor variable
unique(fraud$Class)
# there are only two unique values 0 = no fraud and 1 = fraud
fraud$Class<-as.factor(fraud$Class)
str(fraud$Class)
#run logisitc regression
model<- glm(Class~., data = fraud , family = "binomial")
summary(model)
# run step wise function for step wise variable selection. V2,V3,V11,V12,V17,V18,V19,V24,V25,V26 have been removed
model1<- step(model, direction = "backward", trace = 0)
summary(model1)
#Let us check for multicollinearity
library(car)
vif(model1)
# V1, V6, V7 , V10 , V20, Amount have absolute values more than 5. Let us drop Amount first as it has the highest VIF
# We also drop the variables which were removed after running the step wise function 
fraud1<-fraud[,c(1:2,5:11,14:17,21:24,28:29,31)]
model2<-glm(Class~., data = fraud1, family = 'binomial')
summary(model2)
vif(model2)
#V10 has an abolsute value of 6.79. Let's drop V10.
fraud2<- fraud1[,c(1:8,10:20)]
model3<-glm(Class~., data=fraud2, family = 'binomial')
vif(model3)
# All the independent variable have a VIF value less than 5. We will use fraud2 for splitting the data
library(caret)
set.seed(1234)
index<-createDataPartition(fraud2$Class, p = 0.8, list = FALSE)
train<- fraud2[index,]
test<-fraud2[-index,]
#run logistic regression on train data and use step wise function
model4<-glm(Class~., data = train, family = 'binomial')
summary(model4)
model5<- step(model4, direction = 'backward', trace = 0) 
summary(model5)
#predict the test data
predicted<- predict(model5, newdata = test, type = "response")
predicted # this will show us the probability values
test$predicted<-predicted #create a new column called predicted in the test data
test$predicted<-ifelse(test$predicted >= 0.5 , 1 , 0)# as the dependent variable "Class" has 2 classes (0,1)
# we need to convert the probability values to 1 and 0
str(test$predicted)# this is a numerical vector which needs to be changed to factor as "Class" is a factor vector
test$predicted<-as.factor(test$predicted)
confusionMatrix(test$predicted,test$Class, positive = "1")
#Accuracy:99.92%, Sensitivity:60.2%, Specificity:99.98%
#Let us check if the data is imbalanced or not
table(train$Class)
barplot(table(train$Class))
# 0 = 227452 and 1 = 394. This proves that the data is very much imbalanced. We need to balance the data.
library(ROSE)
227452*2=454904
394*2=788
# We will be using over sampling, under sampling and both sampling for balancing the data
set.seed(1234)
over_data<- ovun.sample(Class~., data = train, method = "over", N=454904)$data
set.seed(1234)
under_data <- ovun.sample(Class~., data = train, method = "under", N=788)$data
set.seed(1234)
both_data<-ovun.sample(Class~., data= train, method = "both", p=0.5 , N= 227846)$data
#predict the over_data with logistic regression
model6<-glm(Class~., data = over_data, family = "binomial")
predLR_over<-predict(model6, newdata = test, type = "response")
predLR_over
test$predLR_over<-predLR_over
test$predLR_over<-ifelse(test$predLR_over >= 0.5 , 1 , 0)
str(test$predLR_over)
test$predLR_over<-as.factor(test$predLR_over)
confusionMatrix(test$predLR_over, test$Class, positive = "1")
#Accuracy:97.24%, Sensitivity:85.71%, Specificity:97.25% (Logistic Regression for oversampling)
#predict the under_data with logistic regression
model7<- glm(Class~., data = under_data, family = "binomial")
predLR_under<-predict(model7, newdata = test, type = "response")
predLR_under
test$predLR_under<-predLR_under
test$predLR_under<-ifelse(predLR_under >= 0.5,1,0)
str(test$predLR_under)
test$predLR_under<-as.factor(test$predLR_under)
confusionMatrix(test$predLR_under, test$Class, positive = "1")
#Accuracy:95.6%, Sensitivity:89.79%, Specificity:95.61% (Logistic Regression for undersampling)
#predict the both_data with logistic regression
model8<-glm(Class~., data = both_data, family = "binomial")
predLR_both<-predict(model8, newdata = test, type = "response")
predLR_both
test$predLR_both<-predLR_both
test$predLR_both<-ifelse(test$predLR_both >= 0.5, 1, 0)
str(test$predLR_both)
test$predLR_both<-as.factor(test$predLR_both)
confusionMatrix(test$predLR_both, test$Class, positive = "1")
#Accuracy:97.15%, Sensitivity:86.73%, Specificity:97.16% (Logistic Regression for both sampling)

#######DECISION TREE
#predict For over_data
library(rpart)
set.seed(1234)
model9<- rpart(Class~., data = over_data)
predDT_over<-predict(model9, newdata = test, type = "class")
predDT_over # the probability values are 0 or 1
test$predDT_over<-predDT_over
str(test$predDT_over) # it is already a factor vector so no need to change the data type
confusionMatrix(test$predDT_over,test$Class, positive = "1")
#Accuracy:96.21%, Sensitivity:85.71%, Specificity:96.22% (Decision Tree for over sampling)

#predict For under_data
set.seed(1234)
model10<- rpart(Class~., data=under_data)
predDT_under<-predict(model10, newdata = test, type = "class")
predDT_under
test$predDT_under<- predDT_under
str(predDT_under)
confusionMatrix(test$predDT_under, test$Class, positive = "1")
#Accuracy:95.77%, Sensitivity:85.71%, Specificity:95.79% (Decision Tree for under sampling)

#predict For both_data
set.seed(1234)
model11<-rpart(Class~., data = both_data)
predDT_both<-predict(model11, newdata = test, type = "class")
predDT_both
test$predDT_both<-predDT_both
confusionMatrix(test$predDT_both, test$Class, positive = "1")
#Accuracy:96.21%, Sensitivity:85.71%, Specificity:96.22% (Decision Tree for both sampling)

##### RANDOM FOREST
library(randomForest)
#predict For over_data
set.seed(1234)
model12<-randomForest(Class~., data = over_data)
predRF_over<-predict(model12, newdata = test, type = "class")
predRF_over# the probability values are 0 or 1
test$predRF_over<- predRF_over
str(test$predRF_over) # it is already a factor vector so no need to change the data type
confusionMatrix(test$predRF_over, test$Class, positive = "1")
#Accuracy:99.95%, Sensitivity:72.44%, Specificity:99.99% (Random Forest for over sampling)

#predict for under_data
set.seed(1234)
model13<-randomForest(Class~., data = under_data)
predRF_under<-predict(model13, newdata = test, type = "class")
predRF_under
test$predRF_under<-predRF_under
str(test$predRF_under)
confusionMatrix(test$predRF_under,test$Class, positive = "1")
#Accuracy:95.18%, Sensitivity:87.75%, Specificity:95.18% (Random Forest for under sampling)

#predict for both_data
set.seed(1234)
model14<-randomForest(Class~., data = both_data)
predRF_both<- predict(model14, newdata= test, type = "class")
predRF_both
test$predRF_both<-predRF_both
str(test$predRF_both)
confusionMatrix(test$predRF_both,test$Class, positive = "1")
#Accuracy:99.95%, Sensitivity:75.51%, Specificity:99.99% (Random Forest for both sampling)

#####ROC and AUC
library(pROC)
#### AUC for Logistic Regression
# AUC for over_sampling (Logistic Regression)
set.seed(1234)
predLR_O<-predict(model6,newdata = test, type = "response" )
predLR_O
test$predLR_O<- predLR_O
str(test$predLR_O)
par(pty='s')
roc(test$Class,test$predLR_O,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = "False Positive Percentage", ylab = "True Positive Percentage", main="AUC for oversampling under LR",
    col= "#EC7B5C",lwd=1, print.auc=TRUE)
# AUC for oversampling under LR is 97%

# AUC for under_sampling (Logistic Regression)
set.seed(1234)
predLR_U<-predict(model7,newdata = test, type = "response")
test$predLR_U<- predLR_U
par(pty='s')
roc(test$Class,test$predLR_U,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = "False Positive Percentage", ylab= "True Positive Percentage", main = "AUC for undersampling under LR",
    col = "#5CECB1",lwd=1, print.auc=TRUE)
# AUC for oversampling under LR is 96.6%

# AUC for both_sampling (Logistic Regression)
set.seed(1234)
predLR_B<-predict(model8,newdata = test, type = "response")
test$predLR_B<-predLR_B
par(pty = "s")
roc(test$Class,test$predLR_B,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = "False Positive Percentage", ylab = "True Positive Percentage",main =  "AUC for both sampling under LR",
    col="#5C74EC",lwd=1,print.auc=TRUE)
# AUC for both_sampling under LR is 97%


#####AUC for Decision Tree
#AUC for over_sampling (Decision Tree)
set.seed(1234)
predDT_O<-predict(model9,newdata = test, type = "prob")
predDT_O
test$predDT_O<- predDT_O[,2]# We are interested in the 2nd column which pertains to fraud detection
str(test$predDT_O)
par(pty='s')
roc(test$Class,test$predDT_O,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab= "False Positive Percentage",ylab="True Positive Percentage", main="AUC for over sampling under DT",
    col="#EC5CAD",lwd=1, print.auc=TRUE)
# AUC for over_sampling under DT is 95.2%

#AUC for under_sampling (Decision Tree)
set.seed(1234)
predDT_U<-predict(model10,newdata = test, type = "prob")
predDT_U
test$predDT_U<-predDT_U[,2]
par(pty='s')
roc(test$Class,test$predDT_U,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab="False Positive Percentage",ylab="True Positive Percentage",main="AUC for under sampling under DT",
    col="#B7950B",lwd=1, print.auc=TRUE)
# AUC for under_sampling under DT is 90.5%

#AUC for both_sampling (Decision Tree)
set.seed(1234)
predDT_B<-predict(model11,newdata = test, type ="prob")
predDT_B
test$predDT_B<-predDT_B[,2]
par(pty='s')
roc(test$Class,test$predDT_B,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab="False Positive Percentage",ylab="True Positive Percentage",main="AUC for both sampling under DT",
    col="#515A5A",lwd=1, print.auc=TRUE)
# AUC for both_sampling under DT is 95.2%


#####AUC for RANDOM FOREST
##AUC for over_sampling (Random Forest)
set.seed(1234)
predRF_O<-predict(model12,newdata=test, type="prob")
predRF_O
test$predRF_O<-predRF_O[,2]
par(pty='s')
roc(test$Class,test$predRF_O,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab= "False Positive Percentage",ylab="True Positive Percentage",main="AUC for over sampling under RF",
    col="#6495ED",lwd=1, print.auc=TRUE)
# AUC for over_sampling under RF is 96.5%

##AUC for under_sampling (Random Forest)
set.seed(1234)
predRF_U<-predict(model13,newdata = test, type = "prob")
predRF_U
test$predRF_U<-predRF_U[,2]
par(pty='s')
roc(test$Class,test$predRF_U,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab= "False Positive Percentage",ylab="True Positive Percentage", main="AUC for under sampling under RF",
    col="#FF7F50",lwd=1, print.auc=TRUE)
# AUC for under_sampling under RF is 97.5%

##AUC for both_sampling (Random Forest)
set.seed(1234)
predRF_B<-predict(model14,newdata = test, type = "prob")
predRF_B
test$predRF_B<-predRF_B[,2]
par(pty='s')
roc(test$Class,test$predRF_B,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = "False Positive Percentage", ylab ="True Positive Percentage", main= "AUC for both sampling under RF",
    col="#DFFF00",lwd=1,print.auc=TRUE)
# AUC for both_sampling under RF is 96.2%


###Conclusion: AUC is a better model than confusion matrix for judging which is the best model for prediction
#For oversampling
# LR:97% ,  DT: 95.2% ,RF:96.5%
# For undersampling
# LR:96.6%, DT: 90.5% ,RF:97.5%
# For both sampling
# LR:97.7%, DT: 95.2% ,RF:96.2%
# We should go for both sampling under Logistic Regression as it has the highest AUC


