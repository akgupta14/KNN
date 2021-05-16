
glass_data <- read.csv(file.choose())

head(glass_data)
describe(glass_data)
colnames(glass_data)
dim(glass_data)

#to check number of rows with missing values

nrow(glass_data)-sum(complete.cases(glass_data))
pairs(glass_data)


table(glass_data$Type)

###Run PCA to see if the data is noisy

library(gdata)

newdata <- glass_data[,1:9]

colnames(newdata)

library(dplyr)
pca_data <- newdata %>% mutate_if(is.numeric,scale)

head(pca_data)

pca <- princomp(pca_data,cor=T,scores = T,covmat = NULL)

summary(pca)
str(pca)

pca_data2  <- cbind(pca_data,glass_data[,10]) 

install.packages("reshape")
library(reshape)

#Change column name to Type
pca_data2 <- rename(pca_data2,c(`glass_data[, 10]`="Type"))

head(pca_data2)

#Attach PCA scores to data

pca_data3 <- cbind(pca_data2,pca$scores[,1:2])

head(pca_data3)

plot(pca$scores[,1:2],col="blue",pch=18,cex=0.3,lwd=3,ellipse=T)
text(pca$scores[,1:2],groups=,cex = 1)


library(ggplot2)

str(pca_data3)
pca_data3$Type <- as.factor(pca_data3$Type)

ggplot(pca_data3,aes(x=Comp.1,y=Comp.2,color=Type,shape=Type))+
geom_hline(yintercept = 0,lty=2)+
geom_vline(xintercept = 0,lty=2)+
geom_point(alpha=0.8)

# Using PCA plot,data seems to be noisy

library(caret)
library(class)

intraining<-createDataPartition(pca_data3$Type,p=.7,list = F)
KNN_train<-pca_data3[intraining,]
KNN_test<-pca_data3[-intraining,]

set.seed(40)
calc_class_err =function(actual,predicted){mean(actual!= predicted)}

#To check how calc_class_err works on k=9

colnames(pca_data3)
calc_class_err(actual = KNN_test$Type,
               predicted =knn(train = KNN_train[,-c(10:12)],test = KNN_test[,-c(10:12)],cl=KNN_train$Type,k=9)
)

k_to_try <- c()
k_to_try = 1:100
err_k = rep(x=0 , times=length(k_to_try))

for (i in (k_to_try)) 
{
  print(i)
  glass_pred = knn(train = KNN_train[,-c(10:12)],test = KNN_test[,-c(10:12)],cl=KNN_train$Type,k=k_to_try[i])
  
  err_k[i] =calc_class_err(KNN_test$Type,glass_pred)  
}

glass_pred

#####Check accuracy on test data
confusionMatrix(glass_pred,KNN_test$Type)

#Test accuracy is 42%

#Let's check Training accuracy

confusionMatrix(glass_pred,KNN_train$Type)



#Plot to see the value of k for which classification error is minimum
plot(err_k,type="b",col="blue",xlab = "K",ylab = "Classification error",
     main="K vs classification error")

#Add line for min- error

abline(h=min(err_k),col="red")

min(err_k)

#To see which K gives the minimum error 
which(err_k==min(err_k))

#K=9


####With Bagging 

acc<- c()
k_to_try <- c()
k_to_try = 1:100
err_k = rep(x=0 , times=length(k_to_try))

for(i in 1:100)
  
{
  print(i)
  
  intraining<-createDataPartition(pca_data3$Type,p=.7,list = F)
  KNN_train<-pca_data3[intraining,]
  KNN_test<-pca_data3[-intraining,]
  
  for (i in (k_to_try)) 
  {
    print(i)
    glass_pred = knn(train = KNN_train[,-c(10:12)],test = KNN_test[,-c(10:12)],cl=KNN_train$Type,k=k_to_try[i])
    
    err_k[i] =calc_class_err(KNN_test$Type,glass_pred)  
    a[i]<- table(KNN_test$Type,glass_pred)
    acc <- c(acc,sum(diag(a))/sum(a))
      }
}

summary(acc)
warnings()

#Testing Accuracy is 51%

#Plot to see the value of k for which classification error is minimum
plot(err_k,type="b",col="blue",xlab = "K",ylab = "Classification error",
     main="K vs classification error")

#Add line for min- error

abline(h=min(err_k),col="red")

min(err_k)

#To see which K gives the minimum error 
which(err_k==min(err_k))

#########################################################################################

Zoo_data <- read.csv(file.choose())

head(Zoo_data)
dim(Zoo_data)

library(Hmisc)
describe(Zoo_data)
str(Zoo_data)

table(Zoo_data$animal.name,Zoo_data$type)
table(Zoo_data$hair,Zoo_data$type)
table(Zoo_data$feathers,Zoo_data$type)
table(Zoo_data$eggs,Zoo_data$type)
table(Zoo_data$milk,Zoo_data$type)
table(Zoo_data$airborne,Zoo_data$type)
table(Zoo_data$aquatic,Zoo_data$type)
table(Zoo_data$predator,Zoo_data$type)
table(Zoo_data$toothed,Zoo_data$type)
table(Zoo_data$backbone,Zoo_data$type)
table(Zoo_data$breathes,Zoo_data$type)
table(Zoo_data$venomous,Zoo_data$type)
table(Zoo_data$fins,Zoo_data$type)
table(Zoo_data$legs,Zoo_data$type)
table(Zoo_data$tail,Zoo_data$type)
table(Zoo_data$domestic,Zoo_data$type)
table(Zoo_data$catsize,Zoo_data$type)
table(Zoo_data$type)

nrow(Zoo_data)-sum(complete.cases(Zoo_data))

temp <- Zoo_data[,-1]

head(temp)
install.packages("DataExplorer")
library(DataExplorer)

temp <- as.data.frame(temp)

library(ggplot2)
plot_boxplot(temp,by="type")

dev.off()

plot_histogram(temp)

install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(temp,type="lower",outline.color = "black",lab=T,
           ggtheme = ggplot2::theme_gray,method = "circle")
           
library(dplyr)
Bytype  <- Zoo_data %>% group_by(animal.name) %>% summarise(sum=sum(type)) 

Zoo_data$type <- as.factor(Zoo_data$type)
str(Zoo_data)
Zoo_data$animal.name <- NULL
head(Zoo_data)

colnames(Zoo_data)
pca <- princomp(Zoo_data[,-17],cor=T,scores = T,covmat = NULL)

summary(pca)

Zoo_pca_data <- cbind(Zoo_data,pca$scores[,1:2])

head(Zoo_pca_data)

library(ggplot2)

str(Zoo_pca_data)
Zoo_pca_data$type <- as.factor(Zoo_pca_data$type)

ggplot(Zoo_pca_data,aes(x=Comp.1,y=Comp.2,color=type,shape=type))+
  geom_hline(yintercept = 0,lty=2)+
  geom_vline(xintercept = 0,lty=2)+
  geom_point(alpha=0.8)

#Data seems to be correctly classified using PC1 and PC2


library(caret)
library(class)


#Generate random numbers that is 70% of the total number of rows
ran <- sample(1:nrow(Zoo_data),0.7*nrow(Zoo_data))

#Extract training data set

Zoo_train <- Zoo_data[ran,]

#Extract test dataset

Zoo_test <- Zoo_data[-ran,]

dim(Zoo_train)
dim(Zoo_test)
str(Zoo_train)
str(Zoo_test)

colnames(Zoo_train)
colnames(Zoo_test)


set.seed(40)
calc_class_err =function(actual,predicted){mean(actual!= predicted)}

acc <- c()
k_to_try <- c()
k_to_try = 1:50
err_k = rep(x=0 , times=length(k_to_try))

for (i in (k_to_try))
{
  
  print(i)
  
  Zoo_pred = knn(train = Zoo_train[,-16],test = Zoo_test[,-16],cl=Zoo_train$type,k=k_to_try[i])
  err_k[i] = calc_class_err(Zoo_test$type,Zoo_pred)  
  
  a<-table(Zoo_test$type,Zoo_pred)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

Zoo_pred


confusionMatrix(Zoo_pred,Zoo_test$type)

#Plot K values 

plot(err_k,col="blue",xlab = "K", ylab = "Classification error", main="Plot of K vs Classification error")
abline(h=min(err_k),col="red",xlim=c(0,50))

which(err_k==min(err_k))

plot(acc,type = "b",xlim=c(0,100))

#with Bagging

calc_class_err =function(actual,predicted){mean(actual!= predicted)}

acc <- c()
k_to_try <- c()
k_to_try = 1:50
err_k = rep(x=0 , times=length(k_to_try))

for (i in (1:50))
{
 print (i)  

  ran <- sample(1:nrow(Zoo_data),0.7*nrow(Zoo_data))
  
  Zoo_train <- Zoo_data[ran,]
  Zoo_test <- Zoo_data[-ran,]

  for (i in (k_to_try))
  {
    
    print(i)
    
    Zoo_pred = knn(train = Zoo_train[,-16],test = Zoo_test[,-16],cl=Zoo_train$type,k=k_to_try[i])
    err_k[i] = calc_class_err(Zoo_test$type,Zoo_pred)  
    
    a<-table(Zoo_test$type,Zoo_pred)
    acc<-c(acc,sum(diag(a))/sum(a))
    
  }
}

confusionMatrix(Zoo_pred,Zoo_test$type)

#Plot K values 

plot(err_k,col="blue",xlab = "K", ylab = "Classification error", main="Plot of K vs Classification error")
abline(h=min(err_k),col="red",xlim=c(0,50))

which(err_k==min(err_k))



#####Run KNN with PCA scores

#Generate random numbers that is 70% of the total number of rows
ran <- sample(1:nrow(Zoo_pca_data),0.7*nrow(Zoo_pca_data))

#Extract training data set

Zoo_pca_train <- Zoo_pca_data[ran,]

#Extract test dataset

Zoo_pca_test <- Zoo_pca_data[-ran,]

dim(Zoo_pca_train)
dim(Zoo_pca_test)
str(Zoo_pca_train)
str(Zoo_pca_test)

colnames(Zoo_pca_train)
colnames(Zoo_pca_test)

set.seed(41)
calc_class_err =function(actual,predicted){mean(actual!= predicted)}

acc <- c()
k_to_try <- c()
k_to_try = 1:50
err_k = rep(x=0 , times=length(k_to_try))

for (i in (k_to_try))
{
  
  print(i)
  
  Zoo_pred = knn(train = Zoo_pca_train[,c("Comp.1","Comp.2")],test = Zoo_pca_test[,c("Comp.1","Comp.2")],cl=Zoo_pca_train$type,k=k_to_try[i])
  err_k[i] = calc_class_err(Zoo_pca_test$type,Zoo_pred)  
  
  a<-table(Zoo_pca_test$type,Zoo_pred)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

Zoo_pred

confusionMatrix(Zoo_pred,Zoo_pca_test$type)


#####With PCA Scores Accuracy of the model is more 61%

#######Let's run Random Forest to do some feature engineering############

library(randomForest)

model <- randomForest(Zoo_data$type~.,data = Zoo_data,ntree=1000)

importance(model)


###########Decision tree for feature engineering 

ran <- sample(1:nrow(Zoo_data),0.7*nrow(Zoo_data))

Zoo_train <- Zoo_data[ran,]

Zoo_test <- Zoo_data[-ran,]

colnames(Zoo_test)
library(C50)

fittree <- C5.0(Zoo_train$type~.,data = Zoo_train)

summary(fittree)

#Bagging and Boosting

acc<- c()

for(i in 1:1000)
  
{
  
  print(i)
  
  intraininglocal <- createDataPartition(Zoo_data$type,p=0.70,list = F)
  
  training1 <- Zoo_data[intraininglocal,]
  testing <- Zoo_data[-intraininglocal,]
  
  dim(training1)
  dim(testing)
  
  fittree <- C5.0(training1$type~.,data=training1,trials=5)
  
  pred <- predict.C5.0(fittree,testing[,-17])
  
  a<- table(testing$type,pred)
  acc <- c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)

summary(fittree)

##### Milk,backbone,hair,feathers,fins,legs,airborne,breathes are important features as per the bagged and boosted decision tree model, We can remove features like predator,aquatic,tail and eggs


colnames(Zoo_data)

Zoo_data_2 <- Zoo_data[,c(1:5,9,12:13,17)]

head(Zoo_data_2)

ran <- sample(1:nrow(Zoo_data_2),0.7*nrow(Zoo_data_2))

Zoo_train_2 <- Zoo_data_2[ran,]

Zoo_test_2 <- Zoo_data_2[-ran,]

head(Zoo_train_2)
head(Zoo_test_2)

colnames(Zoo_train_2)

set.seed(45)
calc_class_err =function(actual,predicted){mean(actual!= predicted)}

library(caret)
library(class)


acc <- c()
k_to_try <- c()
k_to_try = 1:50
err_k = rep(x=0 , times=length(k_to_try))

for (i in (k_to_try))
{
  
  print(i)
  
  Zoo_pred_2 = knn(train = Zoo_train_2[,-9],test = Zoo_test_2[,-9],cl=Zoo_train_2$type,k=k_to_try[i])
  err_k[i] = calc_class_err(Zoo_test_2$type,Zoo_pred_2)  
  
  a<-table(Zoo_test_2$type,Zoo_pred_2)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

Zoo_pred_2

str(Zoo_data)
str(Zoo_test)

confusionMatrix(Zoo_pred_2,Zoo_test_2$type)

#Accuracy with KNN model post feature selection is 32%%

#Accuracy is maximum with PCA scores
