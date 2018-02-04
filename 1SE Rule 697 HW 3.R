library(mlbench)
library(glmnet)

#-- Settings
n.train = 200 # number of training obs
n.test = 50 # number of test obs
K = 10 # number of CV folds
alpha = 0.5 # glmnet tuning alpha (1 = lasso, 0 = ridge)
M = 20 # number of simulations
lam.seq = exp(seq(log(100),log(1e-5),length=500))
evaluation <- matrix(0, nrow=M, ncol=2)

  #-- Data Generating Function
  getData <- function(n) mlbench.friedman1(n, sd=2) # data generating function
#-- Simulations
for(m in 1:M){
  # 1. Generate Training Data
  data.train <- getData(n.train)
  x.train <- data.train$x
  y.train <- data.train$y
  
  # 2. Build Training Model using cross-validation, e.g., cv.glmnet()
  cv.model <- cv.glmnet(x.train, y.train, alpha=alpha, lambda=lam.seq, k=K)
  
  # 3. get lambda that minimizes cv error and 1 SE rule
  lambda.min <- cv.model$lambda.min
  lambda.1se <- cv.model$lambda.1se
  
  # 4. Generate Test Data
  data.test <- getData(n.test)
  x.test <- data.test$x
  y.test <- data.test$y
  
  # 5. Predict y values for test data (for each model)
  min.pred <- predict(cv.model, x.test, s="lambda.min")
  se.pred <- predict(cv.model, x.test, s="lambda.1se")
 
  # 6. Evaluate predictions
  min.r = -sweep(min.pred, 1, y.test, '-')
  se.r = -sweep(se.pred, 1, y.test, '-')
  min.mse = colMeans(min.r^2)
  se.mse = colMeans(se.r^2)
  evaluation[m,] <- c(min.mse,se.mse)
}

  #-- Compare
# compare performance of the approaches
  sum(evaluation[,1]>evaluation[,2])
  