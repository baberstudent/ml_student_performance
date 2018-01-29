
#telechargement des données
d_math=read.table("student-mat.csv",sep=";",header=TRUE)
d_portuguese=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d_math,d_portuguese,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","G3","y","y1"))

library(DMwR)
library(car)
library(lattice)
library(rpart)
library(tree)
library(caret)
library(corrplot)
library(rpart.plot)

#attach(d_math)

###########################################
#*******Statistiques descriptives**********

#summary
summary(d_math)
summary(d_portuguese)
summary(d3)

#corrélation entre variables
corrplot(cor(d_math[c(3,7:8,13:15,24:33)],d_math[c(3,7:8,13:15,24:33)]), type="upper", order="hclust", tl.col="black", tl.srt=45)
corrplot(cor(d_portuguese[c(3,7:8,13:15,24:33)],d_portuguese[c(3,7:8,13:15,24:33)]), type="upper", order="hclust", tl.col="black", tl.srt=45)

#graphiques barplot: math
par(mfrow=c(1,3))

barplot(table(d_math$y)/nrow(d_math) * 100,ylab = "%")
barplot(table(d_math$y1)/nrow(d_math) * 100,ylab = "%")
barplot(table(d_math$G3)/nrow(d_math) * 100,ylab = "%")

barplot(table(d_math$y,d_math$sex)/nrow(d_math) * 100,ylab = "%", col = c("black","white"))
legend(x="topright", legend=c("Fail","pass"), cex=0.8,fill=c("black",'white'),bty="n")

#graphiques: portugais
par(mfrow=c(1,3))

barplot(table(d_portuguese$y)/nrow(d_portuguese) * 100,ylab = "%")
barplot(table(d_portuguese$y1)/nrow(d_portuguese) * 100,ylab = "%")
barplot(table(d_portuguese$G3)/nrow(d_portuguese) * 100,ylab = "%")

barplot(table(d_portuguese$sex, d_portuguese$y)/nrow(d_portuguese) * 100,ylab = "%", col = c("black","white"), beside=T, legend=rownames(table(d_portuguese$sex, d_portuguese$y)))
legend(x="topright", legend=c("Fail","pass"), cex=0.8,fill=c("black",'white'),bty="n")


# Des NA non renseignés ?
length(which(is.na(d_math$sex) & is.na(d_math$G3)))
length(which(is.na(d_portuguese$sex) & is.na(d_portuguese$G3)))


#######################################
#*************math*********************


#**********classification**************

#Données d'entrainement et test
intrain=createDataPartition(y = d_math[["G3"]], p= 0.7, list = FALSE)
training.d_math <- d_math[intrain,]
testing.d_math <- d_math[-intrain,]

#####1- Arbres de décision (DT)####

#(a) Calcul du modèle :
rt.y = rpart(y~., data = training.d_math[c(1:30,34:34)])
rt.y

rt.y1 = rpart(y1~., data = training.d_math[c(1:30,35:35)])
rt.y1

#(b) Pour afficher l'arbre de décision obtenu 
par(mfrow=c(1,3))

plot(rt.y, uniform = F, branch = 0.5, margin = 0.1)
text(rt.y, all = FALSE, use.n = TRUE)

plot(rt.y1, uniform = F, branch = 0.5, margin = 0.1)
text(rt.y1, all = FALSE, use.n = TRUE)

#(c) Graphiques (DT) avec la validation croisée pour avoir le nombre de feuile optimal
#y
d_mathTree.y <- rpart(y~.,data=training.d_math[c(1:30,34:34)],control=rpart.control(minsplit=5,cp=0))
plotcp(d_mathTree.y)

d_mathOptimal.y<- prune(d_mathTree.y,cp=d_mathTree.y$cptable[which.min(d_mathTree.y$cptable[,4]),1])
prp(d_mathOptimal.y,extra=1)

#table(d_math[301:395,c(1:32,35:35)]$y, predict(d_mathOptimal.y, d_math[301:395,c(1:32,35:35)], type="class"))
confusionMatrix(predict(d_mathOptimal.y, testing.d_math[c(1:32,34:34)], type="class"),testing.d_math[c(1:30,34:34)]$y)

#y1
d_mathTree.y1 <- rpart(y1~.,data=training.d_math[c(1:30,35:35)],control=rpart.control(minsplit=5,cp=0))
plotcp(d_mathTree.y)

d_mathOptimal.y1<- prune(d_mathTree.y1,cp=d_mathTree.y1$cptable[which.min(d_mathTree.y1$cptable[,4]),1])
prp(d_mathOptimal.y1,extra=1)

#table(d_math[301:395,c(1:32,35:35)]$y, predict(d_mathOptimal.y, d_math[301:395,c(1:32,35:35)], type="class"))
confusionMatrix(predict(d_mathOptimal.y1, testing.d_math[c(1:32,35:35)], type="class"),testing.d_math[c(1:32,35:35)]$y1)


#*******2- Regression (RT) *********

#(a) Calcul du modèle :
rt.G3 = rpart(G3~ ., data = training.d_math[c(1:30,33:33)])
rt.G3

plot(rt.G3, uniform = F, branch = 0.5, margin = 0.1)
text(rt.G3, all = FALSE, use.n = TRUE)

#(b) Pour afficher l'arbre de décision obtenu 
rt.predictions.G3 = predict(rt.G3, testing.d_math)
regr.eval(testing.d_math[,"G3"], rt.predictions.G3, train.y = testing.d_math[,"G3"])

#Pour afficher les erreurs, on peut tracer la courbe des valeurs prédites contre les valeurs observées :
plot(rt.predictions.G3, testing.d_math[,"G3"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)



#######################################
#*************Portuguais*********************

#**********classification**************

#Données d'entrainement et test
intrain=createDataPartition(y = d_portuguese[["G3"]], p= 0.7, list = FALSE)
training.d_portuguese <- d_portuguese[intrain,]
testing.d_portuguese <- d_portuguese[-intrain,]

#####1- Arbres de décision (DT)####

#(a) Calcul du modèle :
rt.y = rpart(y~., data = training.d_portuguese[c(1:30,34:34)])
rt.y

rt.y1 = rpart(y1~., data = training.d_portuguese[c(1:30,35:35)])
rt.y1

#(b) Pour afficher l'arbre de décision obtenu 
plot(rt.y, uniform = F, branch = 0.5, margin = 0.1)
text(rt.y, all = FALSE, use.n = TRUE)

plot(rt.y1, uniform = F, branch = 0.5, margin = 0.1)
text(rt.y1, all = FALSE, use.n = TRUE)

#(c) Graphiques (DT) avec la validation croisée pour avoir le nombre de feuile optimal
#y
d_portugueseTree.y <- rpart(y~.,data=training.d_portuguese[c(1:31,34:34)],control=rpart.control(minsplit=5,cp=0))
plotcp(d_portugueseTree.y)

d_portugueseOptimal.y<- prune(d_portugueseTree.y,cp=d_portugueseTree.y$cptable[which.min(d_portugueseTree.y$cptable[,4]),1])
prp(d_portugueseOptimal.y,extra=1)

#table(d_portuguese[301:395,c(1:32,35:35)]$y, predict(d_portugueseOptimal.y, d_portuguese[301:395,c(1:32,35:35)], type="class"))
confusionMatrix(predict(d_portugueseOptimal.y, testing.d_portuguese[c(1:31,34:34)], type="class"),testing.d_portuguese[c(1:30,34:34)]$y)

#y1
d_portugueseTree.y1 <- rpart(y1~.,data=training.d_portuguese[c(1:31,35:35)],control=rpart.control(minsplit=5,cp=0))
plotcp(d_portugueseTree.y)

d_portugueseOptimal.y1<- prune(d_portugueseTree.y1,cp=d_portugueseTree.y1$cptable[which.min(d_portugueseTree.y1$cptable[,4]),1])
prp(d_portugueseOptimal.y1,extra=1)

#table(d_portuguese[301:395,c(1:32,35:35)]$y, predict(d_portugueseOptimal.y, d_portuguese[301:395,c(1:32,35:35)], type="class"))
confusionMatrix(predict(d_portugueseOptimal.y1, testing.d_portuguese[c(1:31,35:35)], type="class"),testing.d_portuguese[c(1:32,35:35)]$y1)


#*******2- Regression (RT) *********

#(a) Calcul du modèle :
rt.G3 = rpart(G3~ ., data = training.d_portuguese[c(1:32,33:33)])
rt.G3

plot(rt.G3, uniform = F, branch = 0.5, margin = 0.1)
text(rt.G3, all = FALSE, use.n = TRUE)

#(b) Pour afficher l'arbre de décision obtenu 
rt.predictions.G3 = predict(rt.G3, testing.d_portuguese)
regr.eval(testing.d_portuguese[,"G3"], rt.predictions.G3, train.y = testing.d_portuguese[,"G3"])

#Pour afficher les erreurs, on peut tracer la courbe des valeurs prédites contre les valeurs observées :
plot(rt.predictions.G3, testing.d_portuguese[,"G3"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
