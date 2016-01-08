

#We will use the spider dataset to try out the QR decomposition as a solution to linear models. Load the full spider dataset, by using the code in the Interactions and Contrasts book page. Run a linear model of the friction coefficient with two variable (no interactions):
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

        fit <- lm(friction ~ type + leg, data=spider)

#The solution we are interested in solving is:
        
        betahat <- coef(fit)

#So for our matrix work, 

Y <- matrix(spider$friction, ncol=1)

X <- model.matrix(~ type + leg, data=spider)

#In the material on QR decomposition, we saw that the solution for beta is:
        
#        R beta = Q^T Y
#QR Exercises #1 (1 point possible)

#What is the first row, first column element in the Q matrix for this linear model?
#unanswered
QR <- qr(X)
Q <- qr.Q(QR)
Q[1,1]


#QR Exercises #2 (1 point possible)
#What is the first row, first column element in the R matrix for this linear model?

R <- qr.R(QR)  
R[1,1]


#QR Exercises #3 (1 point possible)

#What is the first row, first column element of Q^T Y (use crossprod() as in the book page)
#unanswered

#Loading
#You have used 0 of 5 submissions

#Finally convince yourself that the QR gives the least squares solution by putting all the pieces together:
        
#       R^-1 (Q^T Y) compared to betahat
