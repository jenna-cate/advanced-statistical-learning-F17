library(MASS)
library(glmnet)
set.seed(40)
n = 20
x1 = rnorm(n)
x2 = rnorm(n, mean = x1, sd = .01)
cor(x1,x2)

y = rnorm(n, mean=1+x1+x2,sd=2)

lam.seq = exp(seq(log(100),log(1e-5),length=500))

### Computer Model
lm.mod <- lm.ridge(y~x1+x2, lambda=lam.seq)   # ridge regression model
beta = coef(m)                   # matrix of estimated coefficients (for each lambda)
penalty = rowSums(beta[,-1]^2)   # total penalty P(\beta) (sum of squared betas)

ridge.stuff = data.frame(lam = lam.seq, intercept=beta[,1], x1=beta[,2],
                         x2=beta[,3], penalty=penalty, GCV=m$GCV, 
                         row.names=NULL)
head(ridge.stuff)

#####
lambda = lam.seq
X = cbind(x1,x2)

ridreg <- function(X, y, lambda) {
  #prep
  X <- as.matrix(X)
  y <- as.matrix(y)  
  n <- nrow(X)    #label dimensions - usually needed
  p <- ncol(X)
  nlambs <- length(lambda) 
  
  # center & scale
  muy <- mean(y) 
  muX <- colMeans(X)
  y <- y - muy # center y
  X <- sweep(X,2,muX,'-') # subtract colmeans - used in centering & scaling
  s <- sqrt(colSums(X^2))
  X <- sweep(X,2,s,'/')
  
  ## precalc for pieces in function B = solve(t(X)%*%X+lambda*diag(p))%*%t(X)%*%y
  XtX <- crossprod(X)  # t(X)%*%X
  Xty <- crossprod(X,y)  # t(X)%*%y
  Ip <- diag(p)

  ## prep to loop
  Beta <- matrix(0, nrow = p, ncol = nlambs) #initialize Beta: one set per lam
  edof <- numeric(nlambs)
  
  #solve for the ridge solution for each lambda
  for(i in 1:nlambs){ 
    lamb = lambda[i]
    Beta[,i] <- solve(XtX+lamb*Ip)%*%Xty
    edof[i] <- sum(diag((X%*%solve(XtX+lamb*Ip)%*%t(X)))) # using the number of nonzero coeffs to estimate df
  }
  
  # unscale!
  Beta = sweep(Beta, 1, s, '/')
  intercept = muy-muX %*% Beta
  Beta = t(rbind(intercept, Beta))
  return(Beta)
}
  
ridregpred <- function(mod.rid, lambda, newdata){
  #order lambdas & betas 
  #locate lambda input & associated betas
  #interpolate beta vals
  #calc preds
  
}  
