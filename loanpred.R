
dataset = train_loan
testdata = test_loan

summary(dataset)
summary(test_loan)


#Data preprocessing 
Loan_ID = testdata$Loan_ID

dataset$Loan_ID = NULL
testdata$Loan_ID = NULL

#totalincometrain = dataset$ApplicantIncome + dataset$CoapplicantIncome
#totalincometest = testdata$ApplicantIncome + testdata$CoapplicantIncome

#dataset = cbind(dataset, totalincometrain)
#testdata = cbind(testdata, totalincometest)

#dataset$ApplicantIncome = NULL
#dataset$CoapplicantIncome = NULL
#testdata$ApplicantIncome = NULL
#testdata$CoapplicantIncome = NULL


#Converting these data to factor
#dataset$Gender = factor(dataset$Gender, levels = c('Female','Male'), labels = c(0,1))
#dataset$Married = factor(dataset$Married, levels = c('Yes','No'), labels = c(0,1))
#dataset$Education = factor(dataset$Education, levels = c('Graduate','Not Graduate'), labels = c(0,1))
#dataset$Self_Employed = factor(dataset$Self_Employed, levels = c('No','Yes'), labels = c(0,1))
#dataset$Property_Area = factor(dataset$Property_Area, levels = c('Rural','Semiurban', 'Urban'), labels = c(0,1,2))
#dataset$Dependents = factor(dataset$Dependents, levels =  c('0','1','2','3+'), labels = c(0,1,2,3))
#dataset$Credit_History = factor(dataset$Credit_History, levels = c("0","1"), labels = c(0,1))
dataset$Loan_Status = factor(dataset$Loan_Status, levels = c('N','Y'), labels = c(0,1))

# In XGBoot If you encoded your categorical variables as numeric but casted them as factors, you will get this error so i am converting them into numeric
dataset$Gender = as.numeric(dataset$Gender)
dataset$Married = as.numeric(dataset$Married)
dataset$Education = as.numeric(dataset$Education)
dataset$Self_Employed = as.numeric(dataset$Self_Employed)
dataset$Property_Area = as.numeric(dataset$Property_Area)
dataset$Dependents = as.numeric(dataset$Dependents)
dataset$Credit_History = as.numeric(dataset$Credit_History)
#dataset$Loan_Status = as.numeric(dataset$Loan_Status)





#test_set 

#testdata$Gender = factor(testdata$Gender, levels = c('Female','Male'), labels = c(0,1))
#testdata$Married = factor(testdata$Married, levels = c('Yes','No'), labels = c(0,1))
#testdata$Education = factor(testdata$Education, levels = c('Graduate','Not Graduate'), labels = c(0,1))
#testdata$Self_Employed = factor(testdata$Self_Employed, levels = c('No','Yes'), labels = c(0,1))
#testdata$Property_Area = factor(testdata$Property_Area, levels = c('Rural','Semiurban', 'Urban'), labels = c(0,1,2))
#testdata$Dependents = factor(testdata$Dependents, levels =  c('0','1','2','3+'), labels = c(0,1,2,3))
#testdata$Credit_History = factor(testdata$Credit_History, levels = c("0","1"), labels = c(0,1))

testdata$Gender = as.numeric(testdata$Gender)
testdata$Married = as.numeric(testdata$Married)
testdata$Education = as.numeric(testdata$Education)
testdata$Self_Employed = as.numeric(testdata$Self_Employed)
testdata$Property_Area = as.numeric(testdata$Property_Area)
testdata$Dependents = as.numeric(testdata$Dependents)
testdata$Credit_History = as.numeric(testdata$Credit_History)

  
#dealing with missing data of LoanAmount, Loan_Amount_Term
dataset$LoanAmount = ifelse(is.na(dataset$LoanAmount),
                            ave(dataset$LoanAmount, FUN = function(x)mean(x, na.rm = TRUE
                                                                          )),
                            dataset$LoanAmount)

dataset$Loan_Amount_Term = ifelse(is.na(dataset$Loan_Amount_Term),
                            ave(dataset$Loan_Amount_Term, FUN = function(x)mean(x, na.rm = TRUE
                            )),
                            dataset$Loan_Amount_Term)

testdata$LoanAmount = ifelse(is.na(testdata$LoanAmount),
                            ave(testdata$LoanAmount, FUN = function(x)mean(x, na.rm = TRUE
                            )),
                            testdata$LoanAmount)

testdata$Loan_Amount_Term = ifelse(is.na(testdata$Loan_Amount_Term),
                                  ave(testdata$Loan_Amount_Term, FUN = function(x)mean(x, na.rm = TRUE
                                  )),
                                  testdata$Loan_Amount_Term)

#dealing with missing values in Gender, Married, Education, Self_Employed and Property_Area using median
#This step is useful if you are converting them itno factors and not numeric
#dataset$Gender = ifelse(is.na(dataset$Gender), NA %in% 1, dataset$Gender)
#dataset$Married = ifelse(is.na(dataset$Married), NA %in% 0, dataset$Married)
#dataset$Dependents = ifelse(is.na(dataset$Dependents),NA %in% 1, dataset$Dependents)
#dataset$Education = ifelse(is.na(dataset$Education), NA %in% 0, dataset$Education)
#dataset$Self_Employed = ifelse(is.na(dataset$Self_Employed),NA %in% 0, dataset$Self_Employed)
#dataset$Credit_History = ifelse(is.na(dataset$Credit_History),NA %in% 1, dataset$Credit_History)


#similarly for testdata


#testdata$Gender = ifelse(is.na(testdata$Gender), NA %in% 1, testdata$Gender)
#testdata$Married = ifelse(is.na(testdata$Married), NA %in% 0, testdata$Married)
#testdata$Dependents = ifelse(is.na(testdata$Dependents), NA %in% 1, testdata$Dependents)
#testdata$Education = ifelse(is.na(testdata$Education), NA %in% 0, testdata$Education)
#testdata$Self_Employed = ifelse(is.na(testdata$Self_Employed),NA %in% 0, testdata$Self_Employed)
#testdata$Credit_History = ifelse(is.na(testdata$Credit_History),NA %in% 1, testdata$Credit_History)


#Feature scaling 
dataset[,6:9] = scale(dataset[,6:9])
testdata[, 6:9] = scale(testdata[, 6:9])
#dataset$totalincometrain = scale(dataset$totalincometrain)
#testdata$totalincometest = scale(testdata$totalincometest)



#Log Model
library(caTools)
classifier = glm(formula = Loan_Status ~ .,family = binomial, data = dataset)
#classifier = glm(formula = Loan_Status ~ Credit_History, family = binomial, data = dataset)

prob_predlog = predict(classifier, type = 'response', newdata = testdata)
Loan_Statuslog = ifelse(prob_predlog>0.5, 1, 0)

Loan_Status = ifelse(Loan_Statuslog == 1, "Y","N")
Logresult = cbind(Loan_ID, Loan_Status)
write.csv(Logresult, file = "Finallog.csv")

#K-Nearest Neighbour 
library(class)
cl = dataset[,12]
y_pred = knn(train = dataset[-12],
             test = testdata,
             cl,
             k = 5)

Loanyandn = ifelse(y_pred == 1, "Y","N")
Logresult = cbind(LoanID, Loanyandn)
colnames(Logresult)
write.csv(Logresult, file = "Finalknn.csv")



#SVM model
install.packages('e1071')
library(e1071)
classifier = svm(Loan_Status~., data = dataset , type = C-classification, kernel = 'linear')
prob_predlog = predict(classifier, type = 'response', newdata = testdata)
Loan_Statuslog = ifelse(prob_predlog>0.5, 1, 0)

Loan_Status = ifelse(Loan_Statuslog == 1, "Y","N")
Logresult = cbind(Loan_ID, Loan_Status)
write.csv(Logresult, file = "Finallog.csv")

#object 'classification' not found


#Naive Bayes
library(e1071)
classifier = naiveBayes(x = dataset[-12],
                        y = dataset$Loan_Status)
y_pred = predict(classifier, newdata = testdata)
Loanyandn = ifelse(y_pred == 1, "Y","N")
Logresult = cbind(LoanID, Loanyandn)
colnames(Logresult)
write.csv(Logresult, file = "FinalnavieBayes.csv")

#Classification Tree
library(rpart)
classifier = rpart(formula = Loan_Status~., data = dataset)
#classifier = rpart(formula = Loan_Status~ Credit_History, data = dataset)

y_pred = predict(classifier, newdata = testdata, type = 'class')
Loanyandn = ifelse(y_pred == 1, "Y","N")
Logresult = cbind(LoanID, Loanyandn)
colnames(Logresult)
write.csv(Logresult, file = "FinalclassificationTree.csv")

#Random Forest
#install.packages("randomForest")
library(randomForest)
classifier = randomForest(x = dataset[-10],
                          y = dataset$Loan_Status,
                          ntree = 120)
y_pred = predict(classifier, newdata = testdata)
Loanyandn = ifelse(y_pred == 1, "Y","N")
Logresult = cbind(LoanID, Loanyandn)
write.csv(Logresult, file = "Final120Tree.csv")


#XGBoost
#install.packages("xgboost")
library(xgboost)
classifier = xgboost(data = as.matrix(dataset[,-12]), label = dataset$Loan_Status, nrounds = 13)
y_pred = predict(classifier, newdata = as.matrix(testdata), class = binomial)
Loan_Status = ifelse(y_pred > 1.151, "Y","N")
Logresult = cbind(Loan_ID, Loan_Status)

write.csv(Logresult, file = "Finalxgboost.csv")
