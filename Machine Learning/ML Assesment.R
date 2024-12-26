library(class)
library(caret)
library(factoextra)
library(tidytable)
library(ggplot2)

setwd("C:/Users/Ashish Khatavkar/Downloads/MNIST-data")

#code to load the MNIST digit recognition dataset into R
load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train_data <<- load_image_file('train-images-idx3-ubyte')
  test_data <<- load_image_file('t10k-images-idx3-ubyte')
  
  train_data$y <<- load_label_file('train-labels-idx1-ubyte')
  test_data$y <<- load_label_file('t10k-labels-idx1-ubyte')
  
  return(
    list(
      train = train_data,
      test = test_data
    )
  )
}

show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

#load data
mnist <- load_mnist()
str(mnist)

#code showing first 25 cases
labels <- paste(mnist$train$y[1:25],collapse = ", ")
par(mfrow=c(5,5), mar=c(0.1,0.1,0.1,0.1))
for(i in 1:25) show_digit(mnist$train$x[i,], axes=F)

#code ot get summary of the data
summary(train_data$x)
summary(train_data$y)

#code to get dimensions of the data
dim(train_data$x)
dim(test_data$x)

#how the pictures looks like
train_data$x[1,]
show_digit(train_data$x[1,])

#having a look at the individual features (pixels)
pairs(test_data$x[,404:408],  col=c("red","green","blue","aquamarine","burlywood","darkmagenta","chartreuse","yellow","chocolate","darkolivegreen")
      [test_data$y+1])

correlation_matrix <- cor(test_data$x[, 404:408])

C <- cov(train_data$x)
image(C)

################################################################################

#Find the number of observation
NROW(train_data$x)
NROW(test_data$x)

#code to check if there are any missing values on the data
any(is.na(train_data$x)) 
any(is.na(train_data$y)) 
any(is.na(test_data$x))
any(is.na(test_data$y)) 

#Normalise the data to transforming them to a range of 0 - 1;
x_train<- train_data$x/255
x_test <- test_data$x/255
y_train <- train_data$y
y_test <- test_data$y

#creating partition/subset of the original training dataset to 3% of it
Find_k = createDataPartition(y_train, p=0.03, list=FALSE, times=1)
train_kx = x_train[Find_k,]
train_ky = y_train[Find_k]

#after reducing the dataset to check the no of rows
NROW(train_kx)
NROW(train_ky)

#range of k 0-42 by taking sqrt of the training no of observations
#always take odd values for k so as to avoid ties
error_train_full <- replicate(0,42)

for(k in 1:42){
  predictions <-knn(train=train_kx, test=train_kx, cl=train_ky,k)
  error_train_full[k] <- 1-mean(predictions==train_ky)
}
error_train_full <- unlist(error_train_full, use.names=FALSE)

error_test_full <- replicate(0,42)

for(k in 1:42){
  predictions <- knn(train=train_kx, test=x_test, cl=train_ky, k)
  error_test_full[k] <- 1-mean(predictions==y_test)
}
error_test_full <- unlist(error_test_full, use.names=FALSE)

#plots the misclassification of errors to find the optimal value of k from range of k
png("k_values_knn_no_pca.png", height=800, width=1000)
plot(error_train_full, type="o", ylim=c(0,0.11), col="blue", xlab="K values", ylab="Misclassification errors", main="Test vs train error for varying k values without PCA")
lines(error_test_full, type="o", col="red")
legend("topright",legend=c("Training error", "Test error"), col=c("blue","red"), lty=1:1)
dev.off()

#code knn without pca
#for k = 3
predict_noPCA_3 <- knn(train= train_kx, test = x_test, cl= train_ky, k=3, prob=TRUE)
cm_noPCA_3 <- confusionMatrix(as.factor(predict_noPCA_3), as.factor(y_test))
print(cm_noPCA_3)

#k=5
predict_noPCA_5 <- knn(train= train_kx, test = x_test, cl= train_ky, k=5, prob=TRUE)
cm_noPCA_5 <- confusionMatrix(as.factor(predict_noPCA_5), as.factor(y_test))
print(cm_noPCA_5)

#k=7
predict_noPCA_7 <- knn(train= train_kx, test = x_test, cl= train_ky, k=7, prob=TRUE)
cm_noPCA_7 <- confusionMatrix(as.factor(predict_noPCA_7), as.factor(y_test))
print(cm_noPCA_7)

#k=11
predict_noPCA_11 <- knn(train= train_kx, test = x_test, cl= train_ky, k=11, prob=TRUE)
cm_noPCA_11 <- confusionMatrix(as.factor(predict_noPCA_11), as.factor(y_test))
print(cm_noPCA_11)

######################################PCA#############################################

min(x_train)
max(x_train)

#covariance matrix
covariance_train <- cov(x_train)
str(covariance_train)
dim(covariance_train)

#Data already scaled above so we simply apply prcomp() for PCA as follows
pca_train <- prcomp(x_train, center=TRUE, scale.=FALSE)

# Visualizing the explained variance
var_explained <- summary(pca_train)$importance[2, ]
plot(var_explained, type = 'b', main = "Explained Variance by Principal Components", xlab = "Principal Component", ylab = "Proportion of Variance Explained")

#Heuristics rule to determine no of PCA  components
#Take so many principal components so that the cumulative percentage of the explained
#variance is at least 80%

# Determine the number of components to retain based on cumulative variance USING Heuristics
cum_var_explained <- cumsum(var_explained)
num_components <- which(cum_var_explained >= 0.80)[1]  # at least 80% variance explained

# Plot Variance explained vs Number of Principal Components "Scree Plot"
plot(cum_var_explained, type = "b", xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     main = "Variance Explained vs Number of Principal Components")

str(pca_train)
View(pca_train$x)

####### Reconstruction of MNIST digits with different number of PC #############
reconstruction_1PC = t(t(pca_train$x[,1:1] %*%
                           t(pca_train$rotation[,1:1])) +
                         pca_train$center)
reconstruction_90PC = t(t(pca_train$x[,1:90] %*%
                            t(pca_train$rotation[,1:90])) +
                          pca_train$center)
reconstruction_150PC = t(t(pca_train$x[,1:150] %*%
                             t(pca_train$rotation[,1:150])) +
                           pca_train$center)
reconstruction_784PC = t(t(pca_train$x[,1:784] %*%
                             t(pca_train$rotation[,1:784])) +
                           pca_train$center)
# png("pca_reconstructions.png",height=800,width=1400)
par(mfrow=c(2,2))
show_digit(reconstruction_1PC[340,], main="1 Component")
show_digit(reconstruction_90PC[340,], main="90 Components")
show_digit(reconstruction_150PC[340,], main="150 Components")
show_digit(reconstruction_784PC[340,], main="784 Components")

############################## KNN ON PCA ######################################
tr_label <- y_train
plot(pca_train$x, col=tr_label, main = 'PC1 v PC2 by Label')
# Spread of PC1 only
plot(pca_train$x[,1], col = tr_label, main = 'Variance of Pixels in the Sample for PC 1', ylab = '', xlab
     = 'PC 1')
#Spread of PC2 only
plot(pca_train$x[,2],col = tr_label, main = 'over PC 2', ylab = '', xlab = 'PC 2')
#Spread of PC44 only
plot(pca_train$x[,44],col = tr_label, main = 'over PC 44', ylab = '', xlab = 'PC 44')

#Insert your number of Principal components in code below
trainFinal = as.matrix(x_train) %*% pca_train$rotation[,1:90]
head(trainFinal)

#Select number of principal components
num_components <- 90

#code to transform the data using PC 90
train_final <- as.matrix(x_train) %*% pca_train$rotation[, 1:num_components]

#code to train kNN classifier with k = 3
k <- 3
predict_pca_on_knn <- knn(train = train_final, test = x_test %*% pca_train$rotation[, 1:num_components], cl = y_train, k = k)

#code to calculate confusion matrix
conf_matrix_knnpca <- confusionMatrix(factor(predict_pca_on_knn, levels = levels(factor(y_train))), factor(y_test))
conf_matrix_knnpca

# Plot scree plot
fviz_eig(pca_train, addlabels = TRUE)

##################second classifier

library(randomForest)
library(readr)

set.seed(132)

# Specify the number of training samples to use and no of trees
numTrain <- 40000
numTrees <- 50

#code to randomly select row indices to create a training dataset of the specified size
rows <- sample(1:nrow(x_train), numTrain)

labels <- as.factor(y_train)

train1 <- x_train

#code to train the Random Forest model with the specified number of trees
rf <- randomForest(train1[rows, ], labels[rows], ntree = numTrees)
plot(rf)
summary(rf)

#code to xtract the out-of-bag (OOB) error rate from the final row of the error rate matrix
oob_error_rate <- rf$err.rate[nrow(rf$err.rate), "OOB"]

#code to alculate the overall accuracy from the OOB error rate
accuracy <- 1 - oob_error_rate

#extract the error rate by class (for each digit) after 50 trees
err <- rf$err.rate
errbydigit <- data.frame(Label = 1:9, Error = err[50, 2:10])

#codelot the error by digit
errbydigitplot <- ggplot(data = errbydigit, aes(x = Label, y = Error)) +
  geom_bar(stat = "identity")

errbydigitplot + scale_x_discrete(limits = 0:9) + xlab("Digit Label") +
  ylab("Error")

#code to predict using the trained Random Forest model (using OOB samples) and capture the predicted classes
oob_predicted <- predict(rf, type = "response")

#code to enerate the confusion matrix using the OOB predictions
conf_matrix <- confusionMatrix(as.factor(oob_predicted), labels)

print(conf_matrix)

pca_train_data <- x_train %*% pca_train$rotation[, 1:90]
pca_test_data <- x_test %*% pca_train$rotation[, 1:90]

dim(pca_train_data)
dim(pca_test_data)


set.seed(132)
numTrainPCA <- min(40000, nrow(pca_train_data))
numTreesPCA <- 50

rowsPCA <- sample(1:nrow(pca_train_data), numTrainPCA)

#code to training the Random Forest model on PCA-transformed data
rf_pca <- randomForest(x = pca_train_data[rowsPCA, ], y = as.factor(y_train[rowsPCA]), ntree = numTreesPCA)
plot(rf_pca)

#code to predict using the PCA-based model on PCA-transformed test data
predictions_pca <- predict(rf_pca, pca_test_data)

conf_matrix_pca <- confusionMatrix(predictions_pca, as.factor(y_test))
print(conf_matrix_pca)
