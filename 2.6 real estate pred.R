library(MASS)
library(glmnet)

realestate_test <- read_csv("~/Documents/ASL HW 2/realestate-test.csv")
realestate_train <- read_csv("~/Documents/ASL HW 2/realestate-train.csv")

y <- realestate_train$price
x <- model.matrix(price~., realestate_train)
newx <- model.matrix(~., realestate_test)

lam.seq = exp(seq(log(100),log(1e-5),length=500))
lm.model <- lm.ridge(y~x-1, lambda=lam.seq)   # ridge regression model

beta = coef(lm.model)                   # matrix of estimated coefficients (for each lambda)
penalty = rowSums(beta[,-1]^2)   # total penalty P(\beta) (sum of squared betas)

ridge.stuff = data.frame(lam = lam.seq, intercept=beta[,1], x1=beta[,2],
                         x2=beta[,3], penalty=penalty, GCV=m$GCV, 
                         row.names=NULL)

library(glmnet)
glm.model <- glmnet(x, y, alpha=0, lambda=lam.seq)
cv.gm <- cv.glmnet(x, y, alpha=0, lambda=lam.seq)
loocv.gm <- cv.glmnet(x, y, alpha=0, lambda=lam.seq, nfolds = 1460)
loocv.gm$lambda.min
cv.gm$lambda.min
glm.model$lambda.min
#- ridge path vs. lambda 
plot(glm.model, "lambda", las=1); abline(h=1,lty=1,col="lightgray")
plot(cv.gm, "lambda", las=1); abline(h=1,lty=1,col="lightgray")

#- ridge path vs. L1 norm of penalty
plot(glm.model, "norm", las=1); abline(h=1,lty=1,col="lightgray")

yhat <- predict(cv.gm, newx, s="lambda.min")
yhat.tr <- predict(cv.gm, x, s="lambda.min")
error <- y-yhat.tr
rmse.tr <- sqrt(mean(error^2))
write.csv(yhat, file = "Predictions.csv")
