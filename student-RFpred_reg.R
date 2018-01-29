#install.packages("ade4")
library(ade4)
#install.packages("caret")
library(caret)
#install.packages("data.table")
library(data.table)

install.packages("gstat")
install.packages("hydroGOF")

library(gstat)
library(hydroGOF)

library(randomForest)
library(DMwR)
library(Hmisc)


describe(d_portuguese)
#RF for Portuguese test score
d_portuguese=read.table("student-por.csv",sep=";",header=TRUE)
print(nrow(d_portuguese)) 
#summary(d_portuguese)



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
  
  #Data partitionning 
  set.seed(3333)
  
  intrain <- createDataPartition( dataframe[["G3"]], p= 0.7, list = FALSE)
  training <- dataframe[intrain,]
  testing <- dataframe[-intrain,]
  
  
  
  stud.rf <<- randomForest(G3 ~ ., ntree = 500, data = training, importance=TRUE)
  
  testing$predicted <- predict(stud.rf, testing)
  
  RMSE = rmse(testing$predicted, testing$G3)

  
  return (RMSE)
}

#RF pour la régression des scores en Portugais : Methode A
print(apply_rf(d_portuguese, NULL))
stud.rf1 =stud.rf

#RF pour la régression des scores en Portugais : Methode B
print(apply_rf(d_portuguese, c("G2")))
stud.rf2 =stud.rf

#RF pour la régression des scores en Portugais : Methode C
print(apply_rf(d_portuguese, c("G2", "G1")))
print(stud.rf3 )


##########################################################################################

#RF for math test score
d_math=read.table("student-mat.csv",sep=";",header=TRUE)
print(nrow(d_math)) # 382 students
#summary(d_math)



#RF pour la régression des scores en Math : Methode A
print(apply_rf(d_math, NULL))
stud.rf7 =stud.rf

#RF pour la régression des scores en Math : Methode B
print(apply_rf(d_math, c("G2")))
stud.rf8 =stud.rf

#RF pour la régression des scores en Math : Methode C
print(apply_rf(d_math, c("G2", "G1")))
stud.rf9 =stud.rf




