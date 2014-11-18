# ASSIGNMENT 1:

# DATE: 16/SEPT/2014

# MY WORKING DIRECTORY
setwd("D:/Documents and Settings/UPV-EHU/Escritorio/Practical_Machine_Learning/Assignment1")

# SETTING THE SEED TO GUARANTEE THE REPRODUCIBILITY.
set.seed(1985)

# CARGA DE LOS DATOS:

# Training dataset:
training.data <- read.csv("pml-training.csv",header=TRUE,na.strings=c("NA","#DIV/0!", ""))
str(training.data)
head(training.data)
summary(training.data)

# Delete missing values (100 variables will be deleted):
training.data <- training.data[,colSums(is.na(training.data)) == 0] 
str(training.data)

# Thee are many variables that are not related with this work, so they must be dropped
# in order to fit a better classificator. Theese are:
names(training.data)[c(1:7)]

# So the training sata set is:
training.data <- training.data[,-c(1:7)]


# Testing dataset:
testing.data <- read.csv("pml-testing.csv",header=TRUE,na.strings=c("NA","#DIV/0!", ""))
str(testing.data)
head(testing.data)
summary(testing.data)

# Delete missing values (100 variables will be deleted):
testing.data <- testing.data[,colSums(is.na(testing.data)) == 0] 
str(testing.data)

# Thee are many variables that are not related with this work, so they must be dropped
# in order to fit a better classificator. Theese are:
names(testing.data)[c(1:7)]

# So the testing data set is:
testing.data <- testing.data[,-c(1:7)]

# BUILT THE BEST PREDICTION MODEL.

# In order to build a good perdiction model, we have to use only the training data set,
# and when we build it, try to know with the testing data set, whether iis a good 
# predictor model.

# We use the crossvalidation in the training set because it's size is enougth to do that
# abd this will help us to select the best model to predict the class levels ni the testing 
# set in the best way.

library(caret)
inTrain <- createDataPartition(y=training.data$classe,p=0.75,list=FALSE)
training.train <- training.data[inTrain,]
training.test <- training.data[-inTrain,]
dim(training.train); dim(training.test)

# The dependent variable is classe and it's distributionin in the training and in
# the training,train set is:
table(training.data$classe)
table(training.train$classe)
par(mfrow=c(2,1))
plot(training.data$classe,col="red",main="Classe variable description in\n training data set",
     ylab="Frequency",xlab="Class levels")
plot(training.train$classe,col="green",main="Classe variable description in\n training.train data set",
     ylab="Frequency",xlab="Class levels")

# Now we fit a model trough:
# i) trees.
# ii) Random forests.
# iii) Boosting.
# iv) lda.
# v) Naive-Bayes.

# 1. TREES
library(rpart)
tree <- rpart(classe~., data=training.train,method="class")
predict.tree <- predict(tree,type="class",newdata=training.test)
cmatrix.tree <- confusionMatrix(predict.tree,training.test$classe)


# 2. RANDOM FORESTS
library(randomForest)
rf <- randomForest(classe ~. , data=training.train, method="class")
prediction.rf <- predict(rf, training.test, type = "class")
cmatrix.rf <- confusionMatrix(prediction.rf, training.test$classe)

# 3. LDA
lda <- train(classe~., data=training.train,method="lda")
predict.lda <- predict(lda,newdata=training.test)	        
cmatrix.lda <- confusionMatrix(training.test$classe,predict.lda)

# 4. NAIVE-BAYES
library(klaR)
nb <- NaiveBayes(as.factor(classe)~., data=training.train)
predict.nb <- suppressWarnings(predict(nb,newdata=training.test))
cmatrix.nb <- confusionMatrix(training.test$classe,predict.nb$class)

# Comparison of the models:
accuracy.tree <- round(as.numeric(cmatrix.tree$overall[1]),4)
accuracy.rf <- round(as.numeric(cmatrix.rf$overall[1]),4)
accuracy.lda <- round(as.numeric(cmatrix.lda$overall[1]),4)
accuracy.nb <- round(as.numeric(cmatrix.nb$overall[1]),4)
method <- c("Tree","Random forest","LDA","Naive-Bayes")
acc <- c(accuracy.tree,accuracy.rf,accuracy.lda,accuracy.nb)
a <- matrix(0,ncol=4,nrow=2)
a[1,] <- method
a[2,] <- acc
print(a)

# The model wich endorse the best fit is:
a[,2]
cmatrix.rf

# Once we selected the bes model, let's go to use it in test set.

rf.test <- predict(rf,testing.data,type="class")
rf.test

