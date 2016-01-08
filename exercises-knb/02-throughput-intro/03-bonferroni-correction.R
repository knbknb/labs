#03-bonferroni-correction.R


# 
# This assessment should help you understand the concept of a error controlling procedure. You can think of it as defnining a set of instructions, such as "reject all the null hypothesis for  for which p-values < 0.0001" or "reject the null hypothesis for the 10 features with smallest p-values". Then, knowing the p-values are random variables, we use statistical theory to compute how many mistakes, on average, will we make if we follow this procedure. 
# More precisely we commonly bounds on these rates, meaning 
# that we show that they are smaller than some predermined value.
# 
# As described in the video, we can compute different error rates. 
# The FWER tells us the probability of having at least one false positive. 
# The FDR is the expected rate of rejected null hypothesis.
# 
# Note 1: the FWER and FDR are not procedures but error rates. 
# We will review procedures here and use Monte Carlo simulations 
# to estimate their error rates.
# 
# Note 2: We sometimes use the colloquial term "pick genes that" 
# meaning "reject the null hypothesis for genes that."
# 
# Bonferroni Correction Exercises #1 (Bonferonni versus Sidak) 
# 

# So we have learned about the family wide error rate FWER. 
# This is the probability of incorrectly rejecting the null at least once.
# Using the notation in the video this probability is written like this: 
#         Pr(V>0).
# 
# What we want to do in practice is choose a procedure that guarantees this
# probability is smaller than a predetermined value such as 0.05. 
# Here we keep it general and instead of 0.05 we use α
# 
# We have already learned that the procedure "pick all the genes with p-value <0.05" 
# fails miserably as we have seen that Pr(V>0)≈1. 
# So what else can we do?
# 
# The Bonferroni procedure assumes we have computed p-values for each 
# test and asks what constant k should we pick so that the procedure "pick all genes with p-value less than k" has Pr(V>0)=0.05. 
# And we typically want to be conservative rather than lenient, 
# so we accept a procedure that has Pr(V>0)≤0.05.
# 
# So the first result we rely on is that this probability is largest 
# when all the null hypotheses are true:
#         
#         Pr(V>0)≤Pr(V>0|all nulls are true)
# 
# or using the notation in the video:
#         
#         Pr(V>0)≤Pr(V>0|m1=0)
# 
# In an earlier assessment 
# we showed that if the tests are independent then
# 
# Pr(V>0|m1=0)=1−(1−k)m
# 
# And we pick k so that 1−(1−k)m=α⟹k=1−(1−α)1/m
# 
# Now, this requires the tests to be independent. 
# The Bonferroni procedure does not make this assumption 
# and as we saw in the video 
# if we set k=α/m this procedure has the property that Pr(V>0)≤α.

#In R define

#alphas <- seq(0,0.25,0.01)

#Make a plot of α/m and 1−(1−α)1/m for various values of m>1.
library(dplyr)
library(ggplot2)
ms <- c(2,10, 100,1000);
alphas <- seq(0,0.25,0.01)

dat <- sapply(ms, function(m){
   dfa <- lapply(alphas, function(a){
           data.frame("alpha"=a,"m"=as.integer(m),"a.m"=a/m, "k"=1 - (1-a)^(1/m))
   })    
})
dat <- data.frame(do.call(bind_rows, dat))


dat %>%
        ggplot(aes(y=a.m, x=alpha, facet = m, color=k)) +
        ylab("a/m") +
        xlab("alpha") +
        geom_point() +
        facet_wrap(~m) 

alphas <- seq(0,0.25,0.01)

#rafa's solution
#par(mfrow=c(2,2))
par(mfrow=c(1,1))
for(m in c(2,10,100,1000)){
        plot(alphas,alphas/m - (1-(1-alphas)^(1/m)),type="l", 
             ylab="alpha/m - k", main=paste0("[m = ", m, "]"))
        abline(h=0,col=2,lty=2)
}        

dat %>%
        ggplot(aes(alpha, a.m)) +
        ylab("a/m") +
        geom_point() +
        facet_wrap(~m) +
        ggtheme()



#Which procedure is more conservative
#(picks less genes, i.e. rejects less null hypothesis): 
#Bonferroni's or Si#dak's?

# Answer: Bonferroni's


#Bonferroni Correction Exercises #2 (Monte Carlo Simulation) (1 point possible)

#Monte Carlo simulation. 
#To simulate the p-value results of, say, 
#8,793 t-tests for which the null is true we don't actual have 
#to generate the original data. 
#As we learned in class 
#we can generate p-values from a uniform distribution like this:
# m <- 8793
# pvals <- runif(m,0,1)

#Using what we have learned, set the cutoff 
#using the Bonferroni correction and report back the FWER. 
#Set the seed at 1,set.seed(1) and run 10,000 simulation. 
#Report the Monte Carlo estimate of the FWER below.

#Using the same seed repeat the above for Sidak's cutoff.
#Report the FWER below.

set.seed(1) 
m <- 8793
nruns <- 10000
alpha <- 0.05
bonf <- alpha/m
sidak <- (1-(1-alpha)^(1/m))
fwer <- function(cutoff, nrun){
                res <- replicate(nrun, {
                pvals <- runif(m,0,1) < cutoff 
                dim(table(pvals)) - 1
        })
        sum(res > 0) / nruns
}
fwer(sidak, nruns)
fwer(bonf, nruns)

set.seed(1)
B <- 10000
m <- 8793
alpha <- 0.05
pvals <- matrix(runif(B*m,0,1),B,m)
k <- (1-(1-alpha)^(1/m))
mistakes <- rowSums(pvals<k) 
mean(mistakes>0)

N <- 12
m <- 100
p0 <- 0.90 ##10% of diets work, 90% don't
m0 <- m*p0
m1 <- m-m0
nullHypothesis <- c( rep(TRUE,m0), rep(FALSE,m1))
delta <- 3 # treatment: diet B is 3 grams higher on average

system.time(
        pVals <- replicate(pvals <- runif(8793,0,1),{
                
                p.vals < alpha
        })
)

