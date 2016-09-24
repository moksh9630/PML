library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

train <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")

## Removing irrelevant variables with more NA values
train1 <- select(train,select = -c(classe,X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window,num_window))
test1 <- select(test,select = -c(problem_id,X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window,num_window))

comb <- rbind(train1,test1)
comb <- comb[,colSums(is.na(comb)) == 0]

## Checking for variables with low variance

library(caret)
lowvar <- nearZeroVar(comb,saveMetrics = TRUE)
comb <- comb[,lowvar[,'nzv']== 0]

## Finding variables which are highly correlated

tmp <- cor(comb)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
comb <- comb[,!apply(tmp,2,function(x) any(x > 0.9))]

train1 <- comb[1:19622,]
test1 <- comb[19623:19642,]

train1$classe <- train$classe
## Dividing the training set for training and cross validation

set.seed(33)
spl <- sample.split(train1,SplitRatio = 0.7)
train2 <- subset(train1,spl == TRUE)
cv <- subset(train1,spl == FALSE)

## Adding ID back to test set
id <- 1:20
test1$id <- id

## Building a Regression Tree on the training set

model1 <- rpart(classe~.,data = train2,method = "class",minbucket = 50)
pred <- predict(model1,newdata = cv,type = "class")
table(cv$classe,pred)

## Building a Random Forest

model2 <- randomForest(classe~.,data = train2,nodesize = 25,ntree = 200)
pred2 <- predict(model2,newdata = cv)
table(cv$classe,pred2)