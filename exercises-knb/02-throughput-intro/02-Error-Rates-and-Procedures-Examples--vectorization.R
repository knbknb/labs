# Error Rates and Procedures Examples
# with and without vectorization
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv" 
filename <- "../03-advinference/femaleControlsPopulation.csv" 
if (!file.exists(filename)) download(url,destfile=filename) 
set.seed(1) 
population = unlist( read.csv("femaleControlsPopulation.csv") )

#To give an example of how we can simulate V and S we constructed a simulation with:
B <- 1000 ##number of simulations        
alpha <- 0.05
N <- 12
m <- 100
p0 <- 0.90 ##10% of diets work, 90% don't
m0 <- m*p0
m1 <- m-m0
nullHypothesis <- c( rep(TRUE,m0), rep(FALSE,m1))
delta <- 3 # treatment: diet B is 3 grams higher on average

system.time(
        pVals <- replicate(B,{
                        control <- sample(population,N)
                        treatment <- sample(population,N)
                        t.test(treatment,control)$p.val < alpha
                })
)
# this is R
sum(pVals < 0.05)
# ~959

#We then ran a Monte Carlo simulation by repeating a procedure in which 
#10,000 tests were run one by one using sapply.

 
calls <- 0
system.time(
        VandS <<- replicate(B,{
                calls <<- sapply(1:m, function(i){
                        control <- sample(population,N)
                        treatment <- sample(population,N)
                        if(!nullHypothesis[i]) treatment <- treatment + delta
                        ifelse(t.test(treatment,control)$p.val < alpha ,
                               "Called as Significant",
                               "Called Not Significant")
                })
                cat("V = ",sum(nullHypothesis & calls),
                    "S = ",sum(!nullHypothesis & calls), "\n")
        })
)
table(nullHypothesis, calls)

#In each iteration we checked if that iteration was associated with the null or alternative hypothesis. 
#We did this with the line

#if(!nullHypothesis[i]) treatment <- treatment + delta

#In R, operations based on matrices are typically much faster than operations performed 
#within loops or sapply. We can vectorize the code to make it go much faster. 
#This means that instead of using sapply to run m tests, 
#we will create a matrix with all data in one call to sample.

#This code runs several times faster than the code above, 
#which is necessary here due to the fact that we will be generating several simulations. 
#Understanding this chunk of code and how it is equivalent to the code above using sapply 
#will take a you long way in helping you code efficiently in R.

library(genefilter) ##rowttests is here
set.seed(1)
##Define groups to be used with rowttests
g <- factor( c(rep(0,N),rep(1,N)) )
B <- 1000 ##number of simulations
calls_vec <-0
system.time(
        VandS <- replicate(B,{
                ##matrix with control data (rows are tests, columns are mice)
                controls <- matrix(sample(population, N*m, replace=TRUE),nrow=m)
                
                ##matrix with control data (rows are tests, columns are mice)
                treatments <-  matrix(sample(population, N*m, replace=TRUE),nrow=m)
                
                ##add effect to 10% of them
                treatments[which(!nullHypothesis),]<-treatments[which(!nullHypothesis),]+delta
                
                ##combine to form one matrix
                dat <- cbind(controls,treatments)
                
                calls_vec <<- rowttests(dat,g)$p.value < alpha
                
                #c(sum(nullHypothesis & calls),sum(!nullHypothesis & calls))
                cat("V = ",sum(nullHypothesis & calls),
                    "S = ",sum(!nullHypothesis & calls), "\n")
        })
)
table(calls_vec, nullHypothesis)
#Note that this code is about 100 times faster!