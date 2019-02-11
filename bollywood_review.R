
#Knowing the data
summary(bolly)
str(bolly)
plot(bolly$Budget,bolly$BO_Collection)
table(bolly$Box_Office_Verdict)

#BO_Collection is in character so changeing it to numeric
bolly$BO_Collection <- as.numeric(bolly$BO_Collection)
str(bolly)
#removing the names of movies coloum
bolly1 <- bolly[,-1]
bolly1$Box_Office_Verdict <- as.factor(bolly1$Box_Office_Verdict)

#from summary the independent variables are having difference in scale
#normalizing the variables
norm <- function(x){
  diff <- max(x)- min(x); a <- (x-min(x))/diff
}
bolly1$BO_Collection <- norm(bolly1$BO_Collection)
bolly1$Budget<- norm(bolly1$Budget)

#dividing data into train and test
size <- round(0.3 * 52)
set.seed(1)
split <- sample(nrow(bolly),size = size)
length(split)
train <- bolly1
test <- bolly1[split,]

#SVM
library(kernlab)
set.seed(12)
model <- ksvm(train$Box_Office_Verdict~.,data = train,kernel = "rbfdot")
pred <- predict(model,newdata = test)
mean(test$Box_Office_Verdict == pred)#0.75

model <- ksvm(train$Box_Office_Verdict~.,data = train,kernel = "vanilladot")
pred <- predict(model,newdata = test)
mean(test$Box_Office_Verdict == pred)#0.562

model <- ksvm(train$Box_Office_Verdict~.,data = train,kernel = "polydot")
pred <- predict(model,newdata = test)
mean(test$Box_Office_Verdict == pred)#0.562

model <- ksvm(train$Box_Office_Verdict~.,data = train,kernel = "laplacedot")
pred <- predict(model,newdata = test)
mean(test$Box_Office_Verdict == pred)#0.9375
summary(model)

#Knn
install.packages("class")
library(class)
#seperating our dependent variable from train and test
colnames(bolly)
traink <- train[,-3]
testK <- test[,-3]
l_train <- train[,3]
l_test <- test[,3]

#converting above l_train and l_test to vector or list 
l_train <- l_train[["Box_Office_Verdict"]]
l_test <- l_test[["Box_Office_Verdict"]]

pred <- knn(train = traink,test = testK,cl = l_train, k = 3) 
mean(pred == l_test )
pred <- knn(train = traink,test = testK,cl = l_train, k = 2) 
mean(pred == l_test )

final <- knn(train = traink,test = testK,cl = l_train, k = 3) 
mean(final == l_test )

#in the above SVM provide more better prediction of 93%

#comparison using crosstable
library(gmodels)
CrossTable(test$Box_Office_Verdict,pred)
