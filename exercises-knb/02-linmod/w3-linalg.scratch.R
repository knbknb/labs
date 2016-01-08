x <- c(1,1,3,3)
f <- formula(~ x)

f
~x
?formula
# look into the variables
model.matrix(f)

xfact <- as.factor(x)
ffact <- ~xfact
model.matrix(ffact)


#### w3 ex 3
nx <- 5
ny <- 7
X = cbind(rep(1,nx + ny),rep(c(0,1),c(nx, ny)))
X

t(X) %*% X

(1/ny + 1/nx)







#W4ex1
#Suppose we have an experiment with two species A and B, and two conditions: control and treated.
library("contrast")
species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))

# And we will use a formula of '~ species + condition'.

# The model matrix is then:
 Y <- c(10 , 12, 22, 23)       
X <- model.matrix(~ species + condition)
X
#Contrasts Exercises #1 (1 point possible)
 solve(t(X) %*% X) %*% t(X) %*% Y
#Suppose we want to build a contrast of coefficients for the above experimental design.
fit <-lm(Y ~ species + condition)
#You can either figure this question out through logic, by looking at the design matrix, 
#or using the contrast() function from the contrast library. 
#The contrast vector is returned as contrast(...)$X.
#contrast(fit, list("species"="A", condition="treated"))$X
contrast(fit, list(species="B",condition="control"), list(species="A",condition="treated"))$X

#What should the contrast vector be, for the contrast of 
#(species=B and condition=control) vs (species=A and condition=treatment)? 
#Assume that the beta vector from the model fit by R is: 
#Intercept, speciesB, conditiontreated.


#Contrasts Exercises #2 (1 point possible)

#Load the spider dataset like this:
        
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)
fitTL <- lm(friction ~ type + leg, data=spider)
#What is the t-value for the contrast of leg pair L4 vs leg pair L2?
L4vsL2 <- contrast(fitTL,list(leg="L4",type="pull"),list(leg="L2",type="pull"))




#In the book page, we computed Sigma using:
        
X <- model.matrix(~ type + leg, data=spider)
(Sigma <- sum(fitTL$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X))

#Our contrast matrix is:
        
(C <- matrix(c(0,0,-1,0,1),1,5))

