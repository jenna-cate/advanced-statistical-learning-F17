library(MASS) #has mvrnorm
##pkg mvtnorm
# set global means for the two classes, ORANGE and BLUE
globalORANGE <- c(0, 1)
globalBLUE <- c(1, 0)
v <- diag(2)

# generate local means for each class
localORANGE <- mvrnorm(10, globalORANGE, v)
localBLUE <- mvrnorm(10, globalBLUE, v)

# generate training data
trainORANGE <- matrix(0, nrow = 100, ncol = 2)
for(k in 1:100) { # 100 means per class
     mkindex <- sample(1:10, size = 1, prob = rep(0.1, 10)) # randomly chosen index for mean k
     mk <- localORANGE[mkindex,] # mean for data point k
     trainORANGE[k,] <- mvrnorm(1, t(mk), diag(2)/5) # generate data pt k
}
#trainORANGE <- cbind(0, trainORANGE)

trainBLUE <- matrix(0, nrow = 100, ncol = 2)
for(k in 1:100) { # 100 means per class
  mkindex <- sample(1:10, size = 1, prob = rep(0.1, 10)) # randomly chosen index for mean k
  mk <- localBLUE[mkindex,] # mean for data point k
  trainBLUE[k,] <- mvrnorm(1, t(mk), diag(2)/5) # generate data pt k
}
#trainBLUE <- cbind(1,trainBLUE)

train <- rbind(trainORANGE, trainBLUE)

  
plot(trainORANGE, col = "orange", xlab = "x", ylab = "y")
points(trainBLUE, col = "blue")

# generate test data
testORANGE <- matrix(0, nrow = 5000, ncol = 2)
for(k in 1:5000) { # 5000 means per class
  mkindex <- sample(1:10, size = 1, prob = rep(0.1, 10)) # randomly chosen index for mean k
  mk <- localORANGE[mkindex,] # mean for data point k
  testORANGE[k,] <- mvrnorm(1, t(mk), diag(2)/5) # generate data pt k
}
testORANGE <- cbind(0, testORANGE)

testBLUE <- matrix(0, nrow = 5000, ncol = 2)
for(k in 1:5000) { # 5000 means per class
  mkindex <- sample(1:10, size = 1, prob = rep(0.1, 10)) # randomly chosen index for mean k
  mk <- localBLUE[mkindex,] # mean for data point k
  testBLUE[k,] <- mvrnorm(1, t(mk), diag(2)/5) # generate data pt k
}
testBLUE <- cbind(1,testBLUE)

test <- rbind(testORANGE, testBLUE)