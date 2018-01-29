#install.packages("ade4")
library(ade4)
#install.packages("caret")
library(caret)
#install.packages("data.table")
library(data.table)

library(randomForest)
library(DMwR)
library(Hmisc)


describe(d_portuguese)
#RF for Portuguese test score
d_portuguese=read.table("student-por.csv",sep=";",header=TRUE)
print(nrow(d_portuguese)) 
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

#Function to perform RF on a specific dataset with 10 fold cross validation
apply_rf = function(dataframe, columns_to_exclude = NULL, log = FALSE,indice){


  
  if(!is.null(columns_to_exclude)){
    for(col in columns_to_exclude){
      if(log){
        print("column to exclude :")
        print(col)
      }
      dataframe[col] = NULL
      
    }
  }
  if(log){
    print(summary(dataframe))
    print('dim(dataframe);')
    print(dim(dataframe)); 
  }
  
  
  set.seed(3333)
  
  intrain <- createDataPartition( dataframe[["G3"]], p= 0.7, list = FALSE)
  training <- dataframe[intrain,]
  testing <- dataframe[-intrain,]
  

  
  stud.rf <<- randomForest(G3 ~ ., ntree = 500, data = training, importance=TRUE)
  
  testing$predicted <- predict(stud.rf, testing)
  
  
  return (confusionMatrix(testing$predicted, testing$G3)$overall["Accuracy"])
}

#RF pour la classification binaire des scores en Portugais : Methode A
print(apply_rf(d_portuguese_binary, NULL))
stud.rf1 =stud.rf


#RF pour la classification binaire des scores en Portugais : Methode B
print(apply_rf(d_portuguese_binary, c("G2")))
stud.rf2 =stud.rf

#RF pour la classification binaire des scores en Portugais : Methode C
print(apply_rf(d_portuguese_binary, c("G2", "G1")))
stud.rf3 =stud.rf




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
#summary(d_portuguese_five)

#RF pour la classification 5-class des scores en Portugais : Methode A
print(apply_rf(d_portuguese_five, NULL))
stud.rf4 =stud.rf

#RF pour la classification 5-class des scores en Portugais : Methode B
print(apply_rf(d_portuguese_five, c("G2")))
stud.rf5 =stud.rf

#RF pour la classification 5-class des scores en Portugais : Methode C
print(apply_rf(d_portuguese_five, c("G2", "G1")))
stud.rf6 =stud.rf



##########################################################################################

#RF for math test score
d_math=read.table("student-mat.csv",sep=";",header=TRUE)
print(nrow(d_math)) 
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

#RF pour la classification binaire des scores en Math : Methode A
print(apply_rf(d_math_binary, NULL))
stud.rf7 =stud.rf

#RF pour la classification binaire des scores en Math : Methode B
print(apply_rf(d_math_binary, c("G2")))
stud.rf8 =stud.rf

#RF pour la classification binaire des scores en Math : Methode C
print(apply_rf(d_math_binary, c("G2", "G1")))
stud.rf9 =stud.rf


# Graphique des importances
par(mfrow = c(2, 3), col="navy", bg="white")

varImpPlot(stud.rf4, type = 1, main="Math Méthode A")
varImpPlot(stud.rf5, type = 1, main="Math Méthode B")
varImpPlot(stud.rf6, type = 1, main="Math Méthode C")

varImpPlot(stud.rf1, type = 1, main="Portugais Méthode A")
varImpPlot(stud.rf2, type = 1, main="Portugais Méthode B")
varImpPlot(stud.rf3, type = 1, main="Portugais Méthode C")


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
print(apply_rf(d_math_five, NULL))
stud.rf10 =stud.rf


#SVM pour la classification 5-class des scores en Math : Methode B
print(apply_rf(d_math_five, c("G2")))
stud.rf11 =stud.rf


#SVM pour la classification 5-class des scores en Math : Methode C
print(apply_rf(d_math_five, c("G2", "G1")))
stud.rf12 =stud.rf

