#Modeling report 
#Pierre Passerotte 88941

install.packages("neuralnet")
require(neuralnet)
?infert

# Libraries 
library(readr)
library(dplyr)
library(readr)

#Importing dataset 


krkopt <- read_csv("krkopt.data", col_names = FALSE, 
                   col_types = cols(`3` = col_double()))
View(krkopt)

# Overview of the dataset
library(dplyr)
glimpse(krkopt)
summary(krkopt)



library(readr)
krkopt <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/chess/king-rook-vs-king/krkopt.data", 
                   col_names = FALSE)
View(krkopt)
str(krkopt)
names(krkopt)<-c("wkf","wkr","wrf","wrr","bkf","bkr","class")
krkopt=krkopt%>%mutate_if(is.character, as.factor)
krkopt=krkopt%>%mutate_if(is.numeric, as.factor)
krkopt<-na.omit(krkopt)

rand = sample(1:nrow(krkopt),0.7*nrow(krkopt)) 
Train = krkopt[rand,]
Val = krkopt[-rand,]

# Overview 

gg1<-ggplot(data = krkopt) +
  geom_bar(mapping = aes(x = class))
+ggtitle("Distribution target's level")

gg2<-ggplot(data = krkopt) +
  geom_bar(mapping = aes(x = wkf))
gg2

gg3<-ggplot(data = krkopt) +
  geom_bar(mapping = aes(x = wkr))
gg3


#classification tree

RPART <- rpart(class ~. , data=Train, cp=0, method = "class")
RPART$cptable
CP.OPT<-RPART$cptable[which.min(RPART$cptable[,"xerror"]),"CP"]
OPRPART<-prune(RPART, cp = CP.OPT)
RPART_pred<-predict(OPRPART, newdata=Val,type="class")
rpart.plot(RPART, extra = 2, branch)
plotcp(RPART)
confusionMatrix(RPART_pred, Val$class)

#constructing random forest (?a prend un peu de temps mais il est plut?t pr?cis)
library(randomForest)
library(pROC)
Model <- randomForest(class~., data=Train, ntrees=200)
pred<-predict(Model, newdata=Val)
confusionMatrix(pred,Val$class)
varImpPlot(Model) #?a te montre quelles variables sont les plus importantes dans ta classification


## Conditional Inference Trees
library(party)
ctree_initial = ctree(class~., data=Train)
summary(ctree_initial)
plot(ctree_initial)
ctree_prediction = predict(ctree_initial, newdata=Val)
confusionMatrix(ctree_prediction,Val$class)

#logistical regression
library(MASS)
library(ROCR)
library(pROC)
library(Ecdat)
library(nnet)
library(caret)
multi<- multinom(class ~., family=binomial(logit), data=Train, control = list(maxit = 50))
multi.prediction = predict(multi, Val, type= 'class')
pred.obs=data.frame(multi.prediction=multi.prediction, Val$class)
ctable1<- table(Val$class, multi.prediction)
confusionMatrix(multi.prediction, Val$class)

#multiclass roc plot
plot(multiclass.roc(pred.obs$Val.class, pred.obs$Val.class))

View(Train$class)
View(glm.predictionTrain)
classtable <- table(train$class , )
pred.obs=data.frame(glm.prediction=glm.prediction, Val$class)
View(Val$class)
ctable1<- table(Val$class, glm.prediction)
ctable1
round((sum(diag(ctable1))/sum(ctable1))*100,2)

View(glm.prediction)

# Ordinal logistic regression :

krkopt$class <- factor(krkopt$class, levels=c("draw","zero", "one", "two","three", "four" , "five","six", "seven", "eight", "nine", "ten", "eleven", "twelve" , "thirteen", "fourteen" , "fifteen" ,"sixteen"), ordered=TRUE)
View(krkopt)
rand = sample(1:nrow(krkopt),0.7*nrow(krkopt)) 
Training.Ordinal = krkopt[rand,]
Validation.Ordinal = krkopt[-rand,]

polrMod <- polr(class ~. , data=Training.Ordinal)
summary(polrMod)
predictedClass <- predict(polrMod, Validation.Ordinal, type="class")
ctabletest <- table(Validation.Ordinal$class, predictedClass) 
round((sum(diag(ctabletest))/sum(ctabletest))*100,2)

## Neural Network 

#Applying for neural network 
#constructing neural networks

library(NeuralNetTools)
library(caret)
library(nnet)
library(AMORE)
options(tinytex.verbose = TRUE)


nn1<-nnet(class ~ . , data = Training.Ordinal, size = 10,decay = 5e-6, maxit = 20)
#Running confusion matrix of neural net based on neural net tools package
NN1.predict<-predict(nn1,newdata=Validation.Ordinal, type='class')
NN1.predict<-factor(NN1.predict)
ConfusionNN<-confusionMatrix(NN1.predict, Validation.Ordinal$class)
ConfusionNN
