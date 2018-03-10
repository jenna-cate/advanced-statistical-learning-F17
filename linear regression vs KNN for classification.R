## Comparison of linear regression and KNN for classification of handwritten digits 2 & 3. ##

library(readr)
library(FNN) 

zip3 <- read_csv("~/Documents/ASL HW1/zip.3", col_names = FALSE) ## data files can be found on the ESL book site: https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.digits/
zip2 <- read_csv("~/Documents/ASL HW1/zip.2", col_names = FALSE)
ziptest <- read_delim("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.test.gz", delim=" ", col_names = FALSE)

ziptrain <- as.matrix(rbind(zip2,zip3))
ziptest <- as.matrix(rbind(ziptest[ziptest[,1] == 2,], ziptest[ziptest[,1] == 3,]))

ultest <- ziptest[,-1]
testlabels <- ziptest[,1]

colnames(ultest) <- paste0("X", 1:256)

#training labels 

cl <- rbind(matrix(2,nrow(zip2),1),matrix(3,nrow(zip3),1))


### K-Nearest Neighbors ###

#train and predict 
knn.1 = knn(ziptrain, ultest, cl, k=1) # smallest neighborhod (model on 1 nearest neighbor)
knn.3 = knn(ziptrain, ultest, cl, k=3)
knn.5 = knn(ziptrain, ultest, cl, k=5)
knn.7 = knn(ziptrain, ultest, cl, k=7)
knn.15 = knn(ziptrain, ultest, cl, k=15) # largest neighborhood

# calc miclassification rate
knn.misclass1 <- 1-mean(testlabels==knn.1)
knn.misclass3 <- 1-mean(testlabels==knn.3)
knn.misclass5 <- 1-mean(testlabels==knn.5)
knn.misclass7 <- 1-mean(testlabels==knn.7)
knn.misclass15 <- 1-mean(testlabels==knn.15)

#make a table for misclassification
knn.misclass <- matrix(c(knn.misclass1*100, knn.misclass3*100, knn.misclass5*100, knn.misclass7*100, knn.misclass15*100), ncol=1)
colnames(knn.misclass) <- c("percent misclassified")
rownames(knn.misclass) <- c("k=1","k=3", "k=5", "k=7", "k=15")
knn.misclass <- as.table(knn.misclass)


### Linear Regression ###
lm.zip <- lm(cl ~ ., data=data.frame(ziptrain))

lm.pred <- predict(lm.zip, newdata=data.frame(ultest))
lm.pred <- ifelse(lm.pred<=2.5,2,3) 

lm.misclass <- 1-mean(testlabels==lm.pred)
sprintf("The linear model's misclassification rate on test data is %f percent", lm.misclass*100)

print("The knn models' misclassification rates can be seen in the table below.")
print(knn.misclass)

## knn outperforms lm for all neighborhood sizes


