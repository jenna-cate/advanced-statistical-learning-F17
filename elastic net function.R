# - - - 
## Elastic net function using coordinate descent & a test example. ##
# - - -

elastic <- function(X, y, lambda, alpha, thresh){
  
  coord_dec.pen <- function(X, y, beta.start, niters, lambda) {
    beta = matrix(0, niters, dim(X)[2])
    beta[1,] = beta.start
    b = beta.start
    p = length(beta.start)
    soft <- function(c,thres) {sign(c)*(abs(c) - thres)*( abs(c) > thres)}
    for(l in 2:niters){  ## basically dJ
      yhat = X %*% b 
      r = y - yhat
      for(j in 1:p) {   ## basically dL
        r.adj = r + as.matrix(X[,j]*b[j])  
        b[j] = (soft(c = crossprod(X[,j], r.adj)/n, thres = lambda*alpha))/(1+(lambda*(1-alpha))) ## and here is dP
        r = r.adj-X[,j]*b[j] 
      }
      beta[l,] = b  
      # delta = abs(sum(abs(b[i-1,]-abs(b[i,]))))    convergence criteria
    }
    return(b)  ## the final b (elastic net converged sol for the given lambda)
  }
  
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
  X = sweep(X, 2, muX, '-')
  s = sqrt(colSums(X^2)/n)
  X = sweep(X, 2, s, '/')
  
  ## prep to loop
  Beta <- matrix(0, nrow = p, ncol = nlambs) #initialize Beta: one set per lam
  
  ## solve for the ridge solution for each lambda
  lamb = lambda[1]
  Beta[,1]<-coord_dec.pen(X, y, beta.start, niters, lamb)
  for(q in 2:nlambs){ ## fill out the rest of Beta with beta.start = Beta from previous iteration
    lamb = lambda[q]
    Beta[,q] <- coord_dec.pen(X, y, Beta[,q-1], niters, lamb) 
  }
  
  # unscale
  Beta = sweep(Beta, 1, s, '/')
  intercept = muy- muX %*% Beta
  Beta = rbind(intercept, Beta)
  return(Beta)

  }

### Test Example ###

# - - - 
## Data (files here under hw 3, prob 3: https://mdporter.github.io/ST697/homework.html)
# - - -
library(readr)
y <- read_csv("hw3_3_Y.csv", col_names = FALSE)
hw3_3_beta <- read_csv("hw3_3_beta.csv", col_names = FALSE)
X <- read_csv("hw3_3_X.csv", col_names = FALSE)

lambda = c(1.71971082351927, 1.56693645367945, 1.4277341377924, 1.3008981719911, 1.18532996381701, 1.08002851673778, 0.984081759994118, 0.896658648679222, 0.81700196562535, 0.744421762750966, 0.678289385060521, 0.618032025535625, 0.563127763754679, 0.513101045267865, 0.467518562572006, 0.425985502008261, 0.388142124075087, 0.353660647536777, 0.322242410339198, 0.293615282741962, 0.267531310260795, 0.243764566004414, 0.222109193803878, 0.202377625184931, 0.184398954739609, 0.168017459825357, 0.153091251769996, 
           0.13949104689992, 0.127099046746766, 0.115807917733435, 0.105519861501992, 0.09614576783108, 0.0876044428058068, 0.0798219055549007, 0.0727307474637936, 0.0662695483134498, 0.0603823442878115, 0.0550181432419967, 0.0501304830327345, 0.0456770290855071, 0.041619207712718, 0.0379218720068632, 0.0345529974148323, 0.03148340435655, 0.0286865054854125, 0.0261380754014105, 0.0238160408223037, 0.0217002893954092, 0.0197724954940248, 0.0180159614896167, 0.0164154731249497, 0.0149571677354689, 0.0136284141775315, 
           0.012417703423483, 0.0113145488759655, 0.0103093955380282, 0.00939353725231448, 0.00855904129249238, 0.00779867965377363, 0.00710586644739417, 0.00647460085679626, 0.00589941516142618, 0.00537532737795459, 0.00489779810872015, 0.00446269122363868, 0.00406623803502302, 0.00370500465501321)
thresh = 1e-5
alpha = .32
beta.start = matrix(0, nrow = dim(X)[2], ncol = 1)
niters = 50
X <- as.matrix(X)
y <- as.matrix(y)  
enetbeta = elastic(X, y, lambda, alpha, thresh)
