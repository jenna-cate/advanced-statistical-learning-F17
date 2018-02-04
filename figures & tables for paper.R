\begin{figure}
```{r, echo=FALSE}
G = matrix(c(.5, 0, 1, 1, .5, 0, 0, 1, .5), nrow = 3, ncol = 3)  # create R-P-S game
rownames(G) <- c('r', 'p', 's')
colnames(G) <- c('r', 'p', 's')

hedgegame <- function(G, nrounds, w, b) {
  
  # -- housekeeping -- #
  i = dim(G)[1]
  j = dim(G)[2]
  if(missing(w)) {w = matrix(1/i, ncol = i)} #initialize weight matrix
  if(missing(nrounds)) {nrounds = 1000} # initialize number of rounds
  if(missing(b)) {b = .5} # set beta
  
  rowms <- matrix(0, nrow = nrounds, ncol = i) # to keep track of updates to the mixed strategies
  colstrat <- matrix(0, nrow = nrounds, ncol = 1)
  rowstrat <- matrix(0, nrow = nrounds, ncol = 1)
  costp <- matrix(0, nrow = nrounds, ncol = 1)
  coste <- matrix(0, nrow = nrounds, ncol = 1)
  ## --     -- ##
  
  # function to choose col that maximizes utility for col player
  oracle <-function(p){ 
    utility = matrix(0, nrow = j, ncol = 1)
    for(s in 1:j){
      utility[s] <- p%*%G[,s] # calc utility (expected payoff) for each col strategy in response to p
    }
    i = as.matrix(which(utility==max(utility))) # choose stategy with largest utility
    index = sample(x = i, size = 1) # randomly sample from thos strategies (in case more than 1 has the max utility)
    rbind(index, as.matrix(G[,index])) # return vector containing col strategy and associated cost col
  } 
  
  for(t in 1:nrounds){
    p = w/sum(w)          # normalize weights to create probability dist over row strategies
    colmove = oracle(p)   # call oracle for best response strategy [Ref. 1]
    lossvec = as.matrix(colmove[-1]) 
    rowmove = sample(c(1:3), size = 1, prob = p)
    l = G[rowmove, colmove[1]]     # loss row player incurs from col player's strategy
    
    
    # expert chooses 1 strat
    expert = sample(x = c(1:3), size = 1) # expms = (1/3, 1/3, 1/3) 
    le = G[expert, colmove[1]]
    ##
    rowms[t,] = p
    colstrat[t] = colmove[1]
    rowstrat[t] = rowmove
    costp[t] = l
    coste[t] = le
    w = w * t(b^lossvec) # update weights according to observed environment loss
  }
  
  list("costp" = costp, "coste" = coste, "algcost" = sum(costp), "expcost" = sum(coste), "regret" = sum(costp)-sum(coste), "colstrat" = colstrat, "rowstrat" = rowstrat, "rowms" = rowms, "nrounds" = nrounds, "b" = b)
} 


regret = matrix(0, nrow = 70, ncol = 1)
for(n in 1:70){
  hedgeresults = hedgegame(G, nrounds = 30*n)
  regret[n] = (hedgeresults$regret)/(30*n)
}

matplot(x = c(1:70), y = regret, typ = 'l', lty = 1, xlab = "rounds of play", ylab = "normalized regret")
```
\caption{Normalized Regret for Hedge as # Plays Increases.}
  \end{figure}
  
  
  \begin{figure}
  ```{r, echo = FALSE}
  G = matrix(c(.5, 0, 1, 1, .5, 0, 0, 1, .5), nrow = 3, ncol = 3)  # create R-P-S game
  rownames(G) <- c('r', 'p', 's')
  colnames(G) <- c('r', 'p', 's')
  
  hedgegame <- function(G, nrounds, w, b) {
    
    # -- housekeeping -- #
    i = dim(G)[1]
    j = dim(G)[2]
    if(missing(w)) {w = matrix(1/i, ncol = i)} #initialize weight matrix
    if(missing(nrounds)) {nrounds = 1000} # initialize number of rounds
    if(missing(b)) {b = .5} # set beta
    
    rowms <- matrix(0, nrow = nrounds, ncol = i) # to keep track of updates to the mixed strategies
    colstrat <- matrix(0, nrow = nrounds, ncol = 1)
    rowstrat <- matrix(0, nrow = nrounds, ncol = 1)
    costp <- matrix(0, nrow = nrounds, ncol = 1)
    coste <- matrix(0, nrow = nrounds, ncol = 1)
    ## --     -- ##
    
    for(t in 1:nrounds){
      p = w/sum(w)          # normalize weights to create probability dist over row strategies
      colmove = sample(x = c(1:3), size = 1, prob = c(1/5,3/5,1/5))   # call oracle for best response strategy [Ref. 1]
      lossvec = G[,colmove] 
      rowmove = sample(c(1:3), size = 1, prob = p)
      l = G[rowmove, colmove[1]]     # loss row player incurs from col player's strategy
      
      
      # expert chooses 1 strat
      expert = sample(x = c(1:3), size = 1) # expms = (1/3, 1/3, 1/3) 
      le = G[expert, colmove[1]]
      ##
      rowms[t,] = p
      colstrat[t] = colmove[1]
      rowstrat[t] = rowmove
      costp[t] = l
      coste[t] = le
      w = w * t(b^lossvec) # update weights according to observed environment loss
    }
    
    list("costp" = costp, "coste" = coste, "algcost" = sum(costp), "expcost" = sum(coste), "regret" = sum(costp)-sum(coste), "colstrat" = colstrat, "rowstrat" = rowstrat, "rowms" = rowms, "nrounds" = nrounds, "b" = b)
  } 
  
  algvsexp = matrix(0, nrow = 50, ncol = 2)
  for(n in 1:50){
    hedgeresults = hedgegame(G)
    algvsexp[n,] = cbind(hedgeresults$algcost, hedgeresults$expcost)
  }
  matplot(x=c(1:50), y=algvsexp, typ = 'l', lty = 1, xlab = "trials", ylab = "expert cost vs alg cost")
  
  ```
  \caption{Red line shows }
  \end{figure}
  
 
  
  Hedge algorithm with regret calculation:
    ```{r, eval = false}
  G = matrix(c(.5, 0, 1, 1, .5, 0, 0, 1, .5), nrow = 3, ncol = 3)  # create R-P-S game
  rownames(G) <- c('r', 'p', 's')
  colnames(G) <- c('r', 'p', 's')
  
  hedgegame <- function(G, nrounds, w, b) {
    
    # -- housekeeping -- #
    i = dim(G)[1]
    j = dim(G)[2]
    if(missing(w)) {w = matrix(1/i, ncol = i)} #initialize weight matrix
    if(missing(nrounds)) {nrounds = 1000} # initialize number of rounds
    if(missing(b)) {b = .5} # set beta
    
    rowms <- matrix(0, nrow = nrounds, ncol = i) # to keep track of updates to the mixed strategies
    colstrat <- matrix(0, nrow = nrounds, ncol = 1)
    rowstrat <- matrix(0, nrow = nrounds, ncol = 1)
    costp <- matrix(0, nrow = nrounds, ncol = 1)
    coste <- matrix(0, nrow = nrounds, ncol = 1)
    ## --     -- ##
    
    # function to choose col that maximizes utility for col player
    oracle <-function(p){ 
      utility = matrix(0, nrow = j, ncol = 1)
      for(s in 1:j){
        utility[s] <- p%*%G[,s] # calc utility (expected payoff) for each col strategy in response to p
      }
      i = as.matrix(which(utility==max(utility))) # choose stategy with largest utility
      index = sample(x = i, size = 1) # randomly sample from thos strategies (in case more than 1 has the max utility)
      rbind(index, as.matrix(G[,index])) # return vector containing col strategy and associated cost col
    } 
    
    ## - - Hedge - - ##
    for(t in 1:nrounds){
      p = w/sum(w)          # normalize weights to create probability dist over row strategies
      colmove = oracle(p)   # call oracle for best response strategy [Ref. 1]
      lossvec = as.matrix(colmove[-1]) 
      rowmove = sample(c(1:3), size = 1, prob = p)
      l = G[rowmove, colmove[1]]     # loss row player incurs from col player's strategy
      
      
      # expert chooses 1 strat
      expert = sample(x = c(1:3), size = 1) # expms = (1/3, 1/3, 1/3) 
      le = G[expert, colmove[1]]
      ##
      rowms[t,] = p
      colstrat[t] = colmove[1]
      rowstrat[t] = rowmove
      costp[t] = l
      coste[t] = le
      w = w * t(b^lossvec) # update weights according to observed environment loss
    }
    
    list("costp" = costp, "coste" = coste, "algcost" = sum(costp), "expcost" = sum(coste), "regret" = sum(costp)-sum(coste), "colstrat" = colstrat, "rowstrat" = rowstrat, "rowms" = rowms, "nrounds" = nrounds, "b" = b)
  } 
  
  
  regret = matrix(0, nrow = 70, ncol = 1)
  for(n in 1:70){
    hedgeresults = hedgegame(G, nrounds = 30*n)
    regret[n] = (hedgeresults$regret)/(30*n)
  }
  
  matplot(x = c(1:70), y = regret, typ = 'l', lty = 1, xlab = "rounds of play", ylab = "normalized regret")
  ```
  
  Uneven game with loss comparison:
    ```{r, eval = false}
  G = matrix(c(.5, 0, 1, 1, .5, 0, 0, 1, .5), nrow = 3, ncol = 3)  # create R-P-S game
  rownames(G) <- c('r', 'p', 's')
  colnames(G) <- c('r', 'p', 's')
  
  hedgegame <- function(G, nrounds, w, b) {
    
    # -- housekeeping -- #
    i = dim(G)[1]
    j = dim(G)[2]
    if(missing(w)) {w = matrix(1/i, ncol = i)} #initialize weight matrix
    if(missing(nrounds)) {nrounds = 1000} # initialize number of rounds
    if(missing(b)) {b = .5} # set beta
    
    rowms <- matrix(0, nrow = nrounds, ncol = i) # to keep track of updates to the mixed strategies
    colstrat <- matrix(0, nrow = nrounds, ncol = 1)
    rowstrat <- matrix(0, nrow = nrounds, ncol = 1)
    costp <- matrix(0, nrow = nrounds, ncol = 1)
    coste <- matrix(0, nrow = nrounds, ncol = 1)
    ## --     -- ##
    
    for(t in 1:nrounds){
      p = w/sum(w)          # normalize weights to create probability dist over row strategies
      colmove = sample(x = c(1:3), size = 1, prob = c(1/5,3/5,1/5))   # call oracle for best response strategy [Ref. 1]
      lossvec = G[,colmove] 
      rowmove = sample(c(1:3), size = 1, prob = p)
      l = G[rowmove, colmove[1]]     # loss row player incurs from col player's strategy
      
      
      # expert chooses 1 strat
      expert = sample(x = c(1:3), size = 1) # expms = (1/3, 1/3, 1/3) 
      le = G[expert, colmove[1]]
      ##
      rowms[t,] = p
      colstrat[t] = colmove[1]
      rowstrat[t] = rowmove
      costp[t] = l
      coste[t] = le
      w = w * t(b^lossvec) # update weights according to observed environment loss
    }
    
    list("costp" = costp, "coste" = coste, "algcost" = sum(costp), "expcost" = sum(coste), "regret" = sum(costp)-sum(coste), "colstrat" = colstrat, "rowstrat" = rowstrat, "rowms" = rowms, "nrounds" = nrounds, "b" = b)
  } 
  
  algvsexp = matrix(0, nrow = 50, ncol = 2)
  for(n in 1:50){
    hedgeresults = hedgegame(G)
    algvsexp[n,] = cbind(hedgeresults$algcost, hedgeresults$expcost)
  }
  matplot(x=c(1:50), y=algvsexp, typ = 'l', lty = 1, xlab = "trials", ylab = "expert cost vs alg cost")
  
  ```
  
  