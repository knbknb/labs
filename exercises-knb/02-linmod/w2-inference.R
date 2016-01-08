
library(UsingR)

x = father.son$fheight

y = father.son$sheight

n = length(y)

N = 50

set.seed(1)

index = sample(n,N)

sampledat = father.son[index,]

x = sampledat$fheight

y = sampledat$sheight

betahat = lm(y~x)$coef

#The formula for the standard error in the previous video was
#(the following two lines are not R code):
        
#        SE(betahat) = sqrt(var(betahat))

#var(betahat) = sigma^2 (X^T X)^-1

#We will estimate or calculate each part of this equation and then combine them.

#First, we want to estimate sigma^2, the variance of Y. As we have seen in the previous unit, 
# the random part of Y is only coming from epsilon, 
# because we assume X*beta is fixed. So we can try to estimate 
#the variance of the epsilons from the residuals, 
#the Yi minus the fitted values from the linear model.

fit = lm(y ~ x)

fit$fitted.values

SSR <-sum(y^2 - fit$fitted.values^2)

sigma2 = SSR / 48

X = cbind(rep(1,N), x)
X


mat <- solve(t(X) %*% X) #%*% t(X) %*% Y

#Now we are one step away from the standard error of beta-hat. 
#Take the diagonals from the (X^T X)^-1 matrix above, using the diag() function. 
# Now multiply our estimate of sigma^2 and the diagonals of this matrix. 
#This is the estimated variance of beta-hat, so take the square root of this. 
#You should end up with two numbers, the standard error for the intercept and
#the standard error for the slope.
sqrt(diag(mat) * sigma2)
#What is the standard error for the slope?