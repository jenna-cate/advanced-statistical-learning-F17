### A function for logistic regression, plus a test example. ###

logreg <- function(X,y) {
  #set parameters
  niters <- 10
  dif = 1e-5
  #prep
  X <- as.matrix(X)
  X <- cbind(1, X)
  y <- as.matrix(y)
  i <- nrow(X)    #label dimensions - usually needed
  j <- ncol(X)
  
  p <- matrix(0, i, 1)
  B <- matrix(0, 1, j) 
  
  for(N in 1:niters) {
    Bold <- B #update betas
    for(k in 1:i) { p[k] <- 1/(1 + exp(-(X[k,]%*%t(Bold))))}  # create p vector
    z <- X%*%t(Bold) + (y-p)*(1/(p*(1-p)))  #z matrix, here the (y-p)*(1/(p*(1-p))) is equivalent to solve(W)%*%(y-p)
    B <- solve(t(X)%*%(X*(c(p*(1-p)))))%*%t(X)%*%(z*c(p*(1-p)))  # X*(c(p*(1-p))) is equivalent to W%*%X
    B<-t(B)
    delta <- abs((sum(abs(B))-sum(abs(Bold))))
    if(delta < dif){
      break
    }
  }
  return(B)
}


# - -
## Test Example
## Data (files here, hw 3 prob 2: https://mdporter.github.io/ST697/homework.html)
library(readr)
y <- read_csv("hw3_2_Y.csv", col_names = FALSE)
hw3_2_beta <- read_csv("hw3_2_beta.csv", col_names = FALSE)
X <- read_csv("hw3_2_X.csv", col_names = FALSE)

logregBeta = logreg(X, y) ## current ratio to correct ans:            X1
