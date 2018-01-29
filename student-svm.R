#install.packages("gstat")
#install.packages("hydroGOF")
library("hydroGOF")
#install.packages("ade4")
library(ade4)
#install.packages("caret")
library(caret)
#install.packages("data.table")
library(data.table)
setwd("C:/Users/Zakarya/Dropbox/Apprentissage statistique/TP3/student performance/svm")
#SVM for Portuguese test score
d_portuguese=read.table("student-por.csv",sep=";",header=TRUE)
print(nrow(d_portuguese)) # 382 students
#summary(d_portuguese)

#One Hot encoding of multiclass variables
nominal_variables = c('Mjob', 'Fjob', 'reason', 'guardian')
for (f in nominal_variables){
  df_all_dummy = acm.disjonctif(d_portuguese[f])
  d_portuguese[f] = NULL
  d_portuguese = cbind(d_portuguese, df_all_dummy)
}
#summary(d_portuguese)

#BINARY CLASSIFICATION
d_portuguese_binary = d_portuguese
#We binarize G3 (G3 = 1 if pupil score > 9, 0 else)
d_portuguese_binary$G3 <- ifelse(d_portuguese$G3 > 9,1,0)
#We must factorize G3
d_portuguese_binary[["G3"]] = factor(d_portuguese_binary[["G3"]])
#summary(d_portuguese_binary)

#Function to perform SVM on a specific dataset with 10 fold cross validation
apply_svm = function(dataframe, columns_to_exclude = NULL, log = FALSE, RMSE=FALSE){
  #Data partitionning 
  set.seed(3333)
  intrain <- createDataPartition(y = dataframe[["G3"]], p= 0.7, list = FALSE)
  training <- dataframe[intrain,]
  testing <- dataframe[-intrain,]
  
  if(!is.null(columns_to_exclude)){
    for(col in columns_to_exclude){
      if(log){
        print("column to exclude :")
        print(col)
      }
      training[col] = NULL
      testing[col] = NULL
    }
  }
  if(log){
    print(summary(training))
    print('dim(training); dim(testing);')
    print(dim(training)); print(dim(testing)); 
  }
  
  #set.seed(2222)
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  
  #Apply svm to the train set
  svm <- train(G3 ~., data = training, method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)
  
  test_pred <- predict(svm, newdata = testing)
  if(log){
    #plot(svm)
    print(svm)
    print(test_pred)
  }
  
  if(RMSE){
    return (rmse(test_pred, testing$G3))
  }
  else{
    return (confusionMatrix(test_pred, testing$G3 )$overall["Accuracy"])  
  }
  
}

#SVM pour la classification binaire des scores en Portugais : Methode A
print(apply_svm(d_portuguese_binary, NULL))

#SVM pour la classification binaire des scores en Portugais : Methode B
print(apply_svm(d_portuguese_binary, c("G2")))

#SVM pour la classification binaire des scores en Portugais : Methode C
print(apply_svm(d_portuguese_binary, c("G2", "G1")))



#Five level classification
#We transform G3 in five classes (G3 = A if G3 > 15, B if G3=14-15, C if G3 = 12-13, D if G3 = 10-11, F if G3 < 10)
d_portuguese_five = d_portuguese
d_portuguese_five$G3 <- ifelse(d_portuguese$G3 > 15, "A",
                    ifelse(d_portuguese$G3 > 13, "B",
                           ifelse(d_portuguese$G3 > 11, "C",
                                  ifelse(d_portuguese$G3 > 9, "D","F")
                                   )
                            )
                    )
#We must factorize G3
d_portuguese_five[["G3"]] = factor(d_portuguese_five[["G3"]])
d_portuguese_five$G3
summary(d_portuguese_five)

#SVM pour la classification 5-class des scores en Portugais : Methode A
print(apply_svm(d_portuguese_five, NULL))

#SVM pour la classification 5-class des scores en Portugais : Methode B
print(apply_svm(d_portuguese_five, c("G2")))

#SVM pour la classification 5-class des scores en Portugais : Methode C
print(apply_svm(d_portuguese_five, c("G2", "G1")))

#regression
d_portuguese_regression = d_portuguese

#SVM pour la regression en Portugais : Methode A
print(apply_svm(d_portuguese_regression, NULL, log=FALSE, RMSE=TRUE))

#SVM pour la regression en Portugais : Methode B
print(apply_svm(d_portuguese_regression, c("G2"), log=FALSE, RMSE=TRUE))

#SVM pour la regression en Portugais : Methode C
print(apply_svm(d_portuguese_regression, c("G2", "G1"), log=FALSE, RMSE=TRUE))



#SVM for math test score
d_math=read.table("student-mat.csv",sep=";",header=TRUE)
print(nrow(d_math)) # 382 students
#summary(d_math)

#One Hot encoding of multiclass variables
nominal_variables = c('Mjob', 'Fjob', 'reason', 'guardian')
for (f in nominal_variables){
  df_all_dummy = acm.disjonctif(d_math[f])
  d_math[f] = NULL
  d_math = cbind(d_math, df_all_dummy)
}
#summary(d_math)

#BINARY CLASSIFICATION
d_math_binary = d_math
#We binarize G3 (G3 = 1 if pupil score > 9, 0 else)
d_math_binary$G3 <- ifelse(d_math$G3 > 9,1,0)
#We must factorize G3
d_math_binary[["G3"]] = factor(d_math_binary[["G3"]])
#summary(d_math_binary)

#SVM pour la classification binaire des scores en Math : Methode A
print(apply_svm(d_math_binary, NULL))

#SVM pour la classification binaire des scores en Math : Methode B
print(apply_svm(d_math_binary, c("G2")))

#SVM pour la classification binaire des scores en Math : Methode C
print(apply_svm(d_math_binary, c("G2", "G1")))



#Five level classification
#We transform G3 in five classes (G3 = A if G3 > 15, B if G3=14-15, C if G3 = 12-13, D if G3 = 10-11, F if G3 < 10)
d_math_five = d_math
d_math_five$G3 <- ifelse(d_math$G3 > 15, "A",
                         ifelse(d_math$G3 > 13, "B",
                                ifelse(d_math$G3 > 11, "C",
                                       ifelse(d_math$G3 > 9, "D","F")
                                )
                         )
)
#We must factorize G3
d_math_five[["G3"]] = factor(d_math_five[["G3"]])
d_math_five$G3
summary(d_math_five)

#SVM pour la classification 5-class des scores en Math : Methode A
print(apply_svm(d_math_five, NULL))

#SVM pour la classification 5-class des scores en Math : Methode B
print(apply_svm(d_math_five, c("G2")))

#SVM pour la classification 5-class des scores en Math : Methode C
print(apply_svm(d_math_five, c("G2", "G1")))

#regression
d_math_regression = d_math

#SVM pour la regression en Maths : Methode A
print(apply_svm(d_math_regression, NULL, log=FALSE, RMSE=TRUE))

#SVM pour la regression en Maths : Methode B
print(apply_svm(d_math_regression, c("G2"), log=FALSE, RMSE=TRUE))

#SVM pour la regression en Maths : Methode C
print(apply_svm(d_math_regression, c("G2", "G1"), log=FALSE, RMSE=TRUE))

