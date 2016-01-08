# "Let's use the example from the lecture" "to visualize how there is not a single"
# "best beta-hat, when the design matrix"  "has collinearity of columns."
#An example can be made with:

sex <- factor(rep(c("female","male"),each=4))
trt <- factor(c("A","A","B","B","C","C","D","D"))

#The model matrix can then be formed with:

X <- model.matrix( ~ sex + trt)

# "And we can see that the number of independent columns is less than the"
# "number of columns of X:" 
qr(X)$rank

#Suppose we observe some outcome, Y. For simplicity we will use synthetic data:

Y <- 1:8

#Now, we will fix the value for two beta's and optimize the remaining betas. 
#We will fix beta_male and beta_D. And then we will find the optimal value 
#for the remaining betas, in terms of minimizing sum((Y - X beta)^2).

#The optimal value for the other betas is the one that minimizes:
        
 #       sum( ( (Y - X_male* beta_male - X_D beta_D) - X_R beta_R )^2 )

#Where X_male is the male column of the design matrix, 
        #X_D is the D column, and X_R has the remaining columns.

#So all we need to do is redefine Y as
#Y* = Y - X* beta_male - X** beta_D 
#and fit a linear model. The following line of code creates this  variable Y*, 
#after fixing beta_male to a value 'a', and beta_D to a value, 'b':
        
        makeYstar <- function(a,b) Y - X[,2] * a - X[,5] * b

#Now we'll construct a function which, for a given value a and b, 
        #gives us back the the sum of squared residuals after fitting the other terms.

fitTheRest <- function(a,b) {
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}

#What is the sum of squared residuals when the male coefficient is 1 and the D coefficient is 2, 
#and the other coefficients are fit using the linear model solution?
fitTheRest(1,2)


#We can apply our function fitTheRest to a grid of values
# for beta_male and beta_D, using the expand.grid function in R. 
# expand.grid takes two vectors and returns a matrix with rows
# containing all possible combination. Try it out:
        
        expand.grid(1:3,1:3)

#We can run fitTheRest on a grid of values, using the following code
#(the Vectorize() is necessary as outer() requires only vectorized functions):
        
        betas = expand.grid(-2:8,-2:8)

rss = apply(betas,1,function(x) fitTheRest(x[1],x[2]))

betas[which(rss==min(rss)),]

library(rafalib)

## plot the pairs what are minimum

themin=min(rss)

plot(betas[which(rss==themin),])
