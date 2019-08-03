data.set <- "C:/Users/User/Documents/exer/knnwithR/dataset/setofdata.csv"
print("read dataset")
df <- read.csv(data.set, header=TRUE)
df
#random number
ran <- sample(1:nrow(df), 0.9 * nrow(df))
#normalization
norm <- function(x) {(x-min(x))/(max(x)-min(x))}
data_norm <- as.data.frame(lapply(df[,c(1,2,3,4)],norm))
summary(data_norm)
#extract training set
data_train <- data_norm[ran,]
#extract testing set
data_test <- data_norm[-ran,]
data_target_cat <- df[ran,5]
data_test_cat <- df[-ran,5]
##load the package class
 library(class)
 ##run knn function
 pr <- knn(data_train,data_test,cl=data_target_cat,k=2)
 
 ##create confusion matrix
 tab <- table(pr,data_test_cat)
 
 ##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
 
 accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
 accuracy(tab)

